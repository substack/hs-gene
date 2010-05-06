{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import qualified Language.Haskell.Parser as LH
import qualified Language.Haskell.Syntax as LH
import qualified Language.Haskell.Pretty as LH
import qualified Language.Haskell.Interpreter as H

import Language.Haskell.Pointfree (pointfree)
import qualified Language.Haskell.Pointfree.Common as PF
import qualified Language.Haskell.Pointfree.Parser as PF

import Control.Applicative ((<$>),(<*>))
import Control.Arrow ((&&&),second)
import Control.Monad (liftM2,join)
import Control.Monad.CatchIO
import Control.Monad.Trans (liftIO)
import System.Random (randomRIO,randomRs,newStdGen)

import qualified Data.Map as M
import Data.List ((\\))

import Data.Generics (Data(..),Typeable(..),everywhereM,mkM)

type Export = (String,String)

type Interpreter a = (MonadCatchIO m, Functor m) => m (Either H.InterpreterError a)
type InterpreterT a = (MonadCatchIO m, Functor m) => H.InterpreterT m a

type ImportQ = (H.ModuleName,Maybe H.ModuleName)

data ModuleInfo = ModuleInfo {
    moduleImports :: [ImportQ],
    moduleExports :: [Export],
    moduleName :: H.ModuleName
} deriving Show

withModule :: FilePath -> [ImportQ]
    -> (ModuleInfo -> InterpreterT a) -> Interpreter a
withModule srcFile imports f = H.runInterpreter $ do
    H.loadModules [srcFile]
    
    mInfo@ModuleInfo {
        moduleImports = mImports,
        moduleName = name
    } <- liftIO (getInfo srcFile)
    
    H.setImportsQ $ (name,Nothing) : imports ++ mImports
    exps <- exports name
    f $ mInfo { moduleExports = exps }

exports :: H.Id -> InterpreterT [(String,String)]
exports name = mapM f =<< (concatMap ids <$> H.getModuleExports name)
    where
        f :: String -> InterpreterT (String,String)
        f x = ((,) x) <$> H.typeOf x
        
        ids :: H.ModuleElem -> [String]
        ids (H.Fun x) = [x]
        ids (H.Class _ xs) = xs
        ids (H.Data _ xs) = xs

getInfo :: FilePath -> IO ModuleInfo
getInfo srcFile = do
    src <- readFile srcFile
    ($ LH.parseModule src) $ \m -> case m of
        LH.ParseFailed loc msg ->
            fail $ srcFile ++ " (preprocessed) : "
                ++ (show $ LH.srcLine loc) ++ "," ++ (show $ LH.srcColumn loc) ++ "\n"
                ++ msg
                ++ '\n' : (lines src !! (LH.srcLine loc - 1))
                ++ '\n' : replicate (LH.srcColumn loc - 1) ' ' ++ "^-- here\n"
        LH.ParseOk (LH.HsModule srcLoc (LH.Module modName) mExports imports decls) -> do
            return $ ModuleInfo {
                    moduleImports = ims',
                    moduleName = modName,
                    moduleExports = undefined
                }
                where
                    f = mName . LH.importModule &&& (Just mName <*>) . LH.importAs
                    mName (LH.Module name) = name
                    ims = map f imports
                    ims' = if elem "Prelude" $ map fst ims
                        then ims
                        else ("Prelude",Nothing) : ims

unpoint :: String -> PF.Expr
unpoint = (\(PF.TLE x) -> x) . (\(Right e) -> e) . pointfree

-- order-1 mutations
mutate :: FilePath -> String -> IO String
mutate srcFile expr = 
    (show <$>) . withModule srcFile imports $ \info -> everyExp (unpoint expr)
        $ \e -> do
            t <- H.typeOf $ show e
            let matches = show e : [ name | (name,eType) <- moduleExports info,
                    eType == t, name /= show e ]
            i <- liftIO $ randomRIO (0, length matches - 1)
            liftIO $ print (i,t,matches)
            return $ unpoint $ matches !! i
    where
        imports = [("Control.Monad",Nothing),("Control.Arrow",Nothing)]
        
        everyExp :: (Data a, Monad m) => a -> (a -> m a) -> m a
        everyExp x f = everywhereM (mkM f) x

type ClassVar = [(String,Integer)]

type ClassVars = M.Map String ClassVar

data TypeSig = TypeVar ClassVar | TypeFun TypeSig TypeSig | TypeCon String
    deriving (Show,Eq,Ord)

-- Parse a string type signature into a recursive datatype, with the class
-- constraints for type variables distributed into the leaves for making
-- comparing leaves easier.
parseTypeSig :: String -> TypeSig
parseTypeSig expr = sigWalk xType where
    (LH.ParseOk xModule) = LH.parseModule $ "undefined :: " ++ expr
    (LH.HsModule _ _ _ _ [LH.HsTypeSig _ _ qualType]) = xModule
    (LH.HsQualType xContext xType) = qualType
    
    -- Associate each variable with its typeclasses.
    -- Each variable has an argument index, starting at 0.
    classVars :: ClassVars
    classVars = foldr (\(v,ci) m -> M.insertWith (++) v [ci] m) M.empty
        $ [ (v,(c,i)) | (c,vs) <- M.toList classes, (v,i) <- zip vs [0..] ]
    
    -- Build a map of the classes to the bound variables.
    classes :: M.Map String [String]
    classes = M.fromList $ map f xContext where
        f (LH.UnQual (LH.HsIdent className), tyVars) =
            (className,[ t | LH.HsTyVar (LH.HsIdent t) <- tyVars ])
    
    -- Turn the LH.HsType tree into a TypeSig tree.
    sigWalk :: LH.HsType -> TypeSig
    sigWalk (LH.HsTyFun t1 t2) = TypeFun (sigWalk t1) (sigWalk t2)
    sigWalk (LH.HsTyVar (LH.HsIdent var)) =
        TypeVar (M.findWithDefault [] var classVars)
    sigWalk (LH.HsTyCon (LH.UnQual (LH.HsIdent name))) = TypeCon name

subTypes :: TypeSig -> [TypeSig]
subTypes t@(TypeFun t1 t2) = t : subTypes t1 ++ subTypes t2
subTypes t = [t]

-- Note: doesn't work with multi-param type classes
instanceOf :: String -> String -> InterpreterT Bool
instanceOf xType xInstance = H.typeChecks $
    "let f :: (" ++ xInstance ++ " a) => "
    ++ "a -> a; f = id in f (undefined :: " ++ xType ++ ")"
