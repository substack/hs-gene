{-# LANGUAGE Rank2Types #-}
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

import Data.Generics (Data(..),everywhereM,mkM)

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

update :: (PF.Expr -> PF.Expr) -> PF.Expr -> PF.Expr
update f e@PF.Var{} = f e
update f (PF.Lambda pat expr) = f $ PF.Lambda pat (update f expr)
update f (PF.App e1 e2) = f $ PF.App (update f e1) (update f e2)
update f _ = error "Let encountered in update"

updateM :: (Monad m, Functor m)
    => (PF.Expr -> m PF.Expr) -> PF.Expr -> m PF.Expr
updateM f e@PF.Var{} = f e
updateM f (PF.Lambda pat expr) = f =<< (PF.Lambda pat <$> updateM f expr)
updateM f (PF.App e1 e2) = f =<< liftM2 PF.App (updateM f e1) (updateM f e2)
updateM f _ = error "Let encountered in update"

everyExp :: (Data a, Monad m) => a -> (a -> m a) -> m a
everyExp x f = everywhereM (mkM f) x

everyExp_ :: (Data a, Monad m) => a -> (a -> m ()) -> m ()
everyExp_ x f = everywhereM (mkM $ \d -> f d >> return d) x
    >> return ()

printSubTypes :: FilePath -> String -> Interpreter ()
printSubTypes srcFile expr =
    withModule srcFile imports $ \info -> everyExp_ (unpoint expr)
        $ \e -> do
            t <- H.typeOf $ show e
            liftIO $ putStrLn $ show e ++ " :: " ++ t
    where imports = [("Control.Monad",Nothing),("Control.Arrow",Nothing)]

printMatches :: FilePath -> String -> Interpreter ()
printMatches srcFile expr =
    withModule srcFile imports $ \info -> everyExp_ (unpoint expr)
        $ \e -> do
            t <- H.typeOf $ show e
            let matches = [ name | (name,eType) <- moduleExports info,
                    eType == t, name /= show e ]
            liftIO $ putStrLn
                $ show e ++ " :: " ++ t ++ " => " ++ show matches
    where imports = [("Control.Monad",Nothing),("Control.Arrow",Nothing)]

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
    where imports = [("Control.Monad",Nothing),("Control.Arrow",Nothing)]
