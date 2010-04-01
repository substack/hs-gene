module Main where

import Language.Haskell.Parser
import Language.Haskell.Syntax
import Language.Haskell.Pretty
import Language.Haskell.Interpreter
import qualified Language.Preprocessor.Cpphs as C

import Control.Applicative ((<$>),(<*>))
import Control.Arrow ((&&&))
import Control.Monad.CatchIO

import Data.Maybe (mapMaybe)
import qualified Data.Typeable as T

import System.Random

type Defs = [(String,String)]

functions :: (MonadCatchIO m, Functor m)
    => FilePath -> Defs
    -> m (Either InterpreterError [(String, T.TypeRep)])
functions srcFile defs = runInterpreter $ do
    loadModuleFromString =<< liftIO (preprocess srcFile defs)
    
    ModuleInfo {
        moduleImports = imports,
        moduleName = name
    } <- liftIO (info srcFile defs)
    
    setImportsQ
        $ (name,Nothing)
        : ("Data.Typeable",Nothing)
        : imports
    
    mapM g =<< mapMaybe f <$> getModuleExports name
        where
            typeRepExpr x =
                "Data.Typeable.typeOf (undefined :: "
                    ++ x
                ++ ")"
            interp x = interpret (typeRepExpr x) (as :: T.TypeRep)
            g x = ((,) x) <$> (interp =<< typeOf x)
            f (Fun x) = Just x
            f _ = Nothing

type ImportQ = (ModuleName,Maybe ModuleName)

data ModuleInfo = ModuleInfo {
    moduleImports :: [ImportQ],
    moduleName :: ModuleName
} deriving Show

info :: FilePath -> Defs -> IO ModuleInfo
info srcFile defs = do
    (=<< parseModule <$> preprocess srcFile defs) $ \m -> case m of
        ParseFailed loc msg ->
            fail $ show loc ++ ": " ++ msg
        ParseOk (HsModule srcLoc (Module modName) mExports imports decls) ->
            return $ ModuleInfo { moduleImports = ims', moduleName = modName }
                where
                    f = mName . importModule &&& (Just mName <*>) . importAs
                    mName (Module name) = name
                    ims = map f imports
                    ims' = if elem "Prelude" $ map fst ims
                        then ims
                        else ("Prelude",Nothing) : ims

preprocess :: FilePath -> Defs -> IO String
preprocess srcFile defs =
    C.macroPass defs opts
    =<< C.cppIfdef srcFile defs [] opts
    =<< readFile srcFile
    where opts = C.defaultBoolOptions { C.locations = False }

loadModuleFromString ::
    (MonadInterpreter m, Functor m) => String -> m ()
loadModuleFromString src = do
    tmpFile <- ("/tmp/" ++) . (++ ".hs")
        . take 12 . randomRs ('a','z') <$> liftIO newStdGen
    liftIO $ writeFile tmpFile src
    loadModules [tmpFile]
