module Main where

-- the Exts module doesn't have Interpreter:
import qualified Language.Haskell.Parser as H
import qualified Language.Haskell.Syntax as H
import qualified Language.Haskell.Pretty as H
import qualified Language.Haskell.Interpreter as H
import qualified Language.Preprocessor.Cpphs as C

-- hs2pf needs the Exts module:
import qualified Language.Haskell.Exts.Syntax as E
import qualified Language.Haskell.Exts.Parser as E

import Control.Applicative ((<$>),(<*>))
import Control.Arrow ((&&&))
import Control.Monad.CatchIO
import Control.Monad.Trans (liftIO)
import System.Random (randomRs,newStdGen)

import Data.Maybe (mapMaybe)
import qualified Data.Typeable as T
import qualified Data.PolyTypeable as T

import Language.Pointfree.Parser (hs2pf)

type Env = [(String,String)]
type Types = [(String,T.TypeRep)]



functions :: (MonadCatchIO m, Functor m)
    => FilePath -> Env
    -> m (Either H.InterpreterError Types)
functions srcFile env = H.runInterpreter $ do
    loadModuleFromString =<< liftIO (preprocess srcFile env)
    
    ModuleInfo {
        moduleImports = imports,
        moduleName = name
    } <- liftIO (info srcFile env)
    
    H.setImportsQ
        $ (name,Nothing)
        : ("Data.Typeable",Nothing)
        -- : ("Data.PolyTypeable",Nothing)
        : ("GHC.Err",Nothing)
        : imports
    
    mapM g =<< mapMaybe f <$> H.getModuleExports name
        where
            typeRepExpr x = "Data.Typeable.typeOf (GHC.Err.undefined :: " ++ x ++ ")"
            --typeRepExpr x = "Data.PolyTypeable.polyTypeOf (GHC.Err.undefined :: " ++ x ++ ")"
            interp x = H.interpret (typeRepExpr x) (H.as :: T.TypeRep)
            g x = ((,) x) <$> (interp =<< H.typeOf x)
            f (H.Fun x) = Just x
            f _ = Nothing

type ImportQ = (H.ModuleName,Maybe H.ModuleName)

data ModuleInfo = ModuleInfo {
    moduleImports :: [ImportQ],
    moduleName :: H.ModuleName
} deriving Show

info :: FilePath -> Env -> IO ModuleInfo
info srcFile env = do
    src <- preprocess srcFile env
    ($ H.parseModule src) $ \m -> case m of
        H.ParseFailed loc msg ->
            fail $ srcFile ++ " (preprocessed) : "
                ++ (show $ H.srcLine loc) ++ "," ++ (show $ H.srcColumn loc) ++ "\n"
                ++ msg
                ++ '\n' : (lines src !! (H.srcLine loc - 1))
                ++ '\n' : replicate (H.srcColumn loc - 1) ' ' ++ "^-- here\n"
        H.ParseOk (H.HsModule srcLoc (H.Module modName) mExports imports decls) ->
            return $ ModuleInfo { moduleImports = ims', moduleName = modName }
                where
                    f = mName . H.importModule &&& (Just mName <*>) . H.importAs
                    mName (H.Module name) = name
                    ims = map f imports
                    ims' = if elem "Prelude" $ map fst ims
                        then ims
                        else ("Prelude",Nothing) : ims

preprocess :: FilePath -> Env -> IO String
preprocess srcFile env =
    C.macroPass env opts
    =<< C.cppIfdef srcFile env [] opts
    =<< readFile srcFile
    where opts = C.defaultBoolOptions { C.locations = False }

loadModuleFromString ::
    (H.MonadInterpreter m, Functor m) => String -> m ()
loadModuleFromString src = do
    tmpFile <- ("/tmp/" ++) . (++ ".hs")
        . take 12 . randomRs ('a','z') <$> liftIO newStdGen
    liftIO $ writeFile tmpFile src
    H.loadModules [tmpFile]
