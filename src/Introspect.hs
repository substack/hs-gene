module Main where

import Language.Haskell.Parser
import Language.Haskell.Syntax
import Language.Haskell.Pretty
import Language.Haskell.Interpreter

import Control.Applicative ((<$>),(<*>))
import Control.Arrow ((&&&))
import Control.Monad (mzero,msum)
import Control.Monad.CatchIO

import Data.Maybe (mapMaybe)
import qualified Data.Typeable as T
import qualified Data.Typeable
import Data.List (intersperse)
import qualified Data.Map as M

main = do
    m <- parseModule <$> readFile "src/Introspect.hs"
    let ParseOk (HsModule srcLoc mod mExports imports decls) = m
        f (HsPatBind _ (HsPVar (HsIdent ident)) rhs _) =
            Just (ident, prettyPrint rhs)
        f _ = Nothing
        identMap = M.fromList $ mapMaybe f decls
    print $ identMap

functions :: (MonadCatchIO m, Functor m) =>
      String -> m (Either InterpreterError [(String, T.TypeRep)])
functions src = runInterpreter $ do
    loadModules [src]
    ModuleInfo {
        moduleImports = imports,
        moduleName = name
    } <- liftIO (info src)
    
    setImportsQ $ (name,Nothing) : ("Data.Typeable",Nothing) : imports
    
    mapM g =<< mapMaybe f <$> getModuleExports name
        where
            typeRepExpr x = "Data.Typeable.typeOf (undefined :: " ++ x ++ ")"
            interp x = interpret (typeRepExpr x) (as :: T.TypeRep)
            g x = ((,) x) <$> (interp =<< typeOf x)
            f (Fun x) = Just x
            f _ = Nothing

type ImportQ = (ModuleName,Maybe ModuleName)

data ModuleInfo = ModuleInfo {
    moduleImports :: [ImportQ],
    moduleName :: ModuleName
}

info :: String -> IO ModuleInfo
info src = do
    m <- parseModule <$> readFile src
    let ParseOk (HsModule srcLoc (Module modName) mExports imports decls) = m
        f = mName . importModule &&& (Just mName <*>) . importAs
        mName (Module name) = name
        ims = map f imports
        ims' = if elem "Prelude" $ map fst ims
            then ims
            else ("Prelude",Nothing) : ims
    return $ ModuleInfo {
        moduleImports = ims',
        moduleName = modName
    }
