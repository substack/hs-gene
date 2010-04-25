{-# LANGUAGE Rank2Types #-}
module Main where

import qualified Language.Haskell.Parser as H
import qualified Language.Haskell.Syntax as H
import qualified Language.Haskell.Pretty as H
import qualified Language.Haskell.Interpreter as H
import qualified Language.Preprocessor.Cpphs as C

import Language.Haskell.Pointfree (pointfree)
import qualified Language.Haskell.Pointfree.Common as PF
import qualified Language.Haskell.Pointfree.Parser as PF

import Control.Applicative ((<$>),(<*>))
import Control.Arrow ((&&&),second)
import Control.Monad (liftM2,join)
import Control.Monad.CatchIO
import Control.Monad.Trans (liftIO)
import System.Random (randomRs,newStdGen)

import Data.Maybe (mapMaybe)
import qualified Data.Typeable as T
import qualified Data.PolyTypeable as T

type Env = [(String,String)]
type Types = [(String,T.TypeRep)]

type Interpreter a = (MonadCatchIO m, Functor m) => m (Either H.InterpreterError a)
type InterpreterT a = (MonadCatchIO m, Functor m) => H.InterpreterT m a

type ImportQ = (H.ModuleName,Maybe H.ModuleName)

data ModuleInfo = ModuleInfo {
    moduleImports :: [ImportQ],
    moduleName :: H.ModuleName
} deriving Show

withModule :: FilePath -> Env -> [ImportQ]
    -> (ModuleInfo -> InterpreterT a) -> Interpreter a
withModule srcFile env imports f = H.runInterpreter $ do
    loadModuleFromString =<< liftIO (preprocess srcFile env)
    
    mInfo@ModuleInfo {
        moduleImports = mImports,
        moduleName = name
    } <- liftIO (getInfo srcFile env)
    
    H.setImportsQ $ (name,Nothing) : imports ++ mImports
    f mInfo

exports :: ModuleInfo -> InterpreterT [(String,String)]
exports ModuleInfo{ moduleName = name } =
    mapM f =<< (concatMap eId <$> H.getModuleExports name)
    where
        f :: String -> InterpreterT (String,String)
        f x = ((,) x) <$> H.typeOf x
        
        eId :: H.ModuleElem -> [String]
        eId (H.Fun x) = [x]
        eId (H.Data _ xs) = xs
        eId _ = []

getInfo :: FilePath -> Env -> IO ModuleInfo
getInfo srcFile env = do
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

topToExpr :: String -> PF.Expr
topToExpr = (\(PF.TLE x) -> x) . (\(Right e) -> e) . pointfree

update :: (PF.Expr -> PF.Expr) -> PF.Expr -> PF.Expr
update f e@PF.Var{} = f e
update f (PF.Lambda pat expr) = f $ PF.Lambda pat (update f expr)
update f (PF.App e1 e2) = f $ PF.App (update f e1) (update f e2)
update f _ = error "Lambda encountered in update"

updateM :: Monad m => (PF.Expr -> m PF.Expr) -> PF.Expr -> m PF.Expr
updateM f e@PF.Var{} = f e
updateM f (PF.Lambda pat expr) = (f . PF.Lambda pat) =<< updateM f expr
updateM f (PF.App e1 e2) = f =<< liftM2 PF.App (updateM f e1) (updateM f e2)
updateM f _ = error "Lambda encountered in update"

printSubTypes :: FilePath -> Env -> String -> Interpreter PF.Expr
printSubTypes srcFile env expr = withModule srcFile env imports
    $ \_ -> (flip updateM $ topToExpr expr)
    $ \e -> do
        t <- H.typeOf $ show e
        liftIO $ putStrLn $ show e ++ " :: " ++ t
        return e
    where imports = [("Control.Monad",Nothing),("Control.Arrow",Nothing)]

printMatches :: FilePath -> Env -> String -> Interpreter PF.Expr
printMatches srcFile env expr = withModule srcFile env imports $ \_ -> do
    (flip updateM $ topToExpr expr) $ \e -> do
        t <- H.typeOf $ show e
        let matches = undefined
        liftIO $ putStrLn $ show e ++ " :: " ++ t ++ " => " ++ matches
        return e
    where imports = [("Control.Monad",Nothing),("Control.Arrow",Nothing)]

-- | Mutate a top-level declaration
mutate :: MonadCatchIO m => FilePath -> Env -> Types -> String
    -> m (Either H.InterpreterError PF.Expr)
mutate path env pool name = undefined

--printTypes :: FilePath -> Env -> Interpreter PF.Expr
--printTypes srcFile env = withModule srcFile env $ \_ -> updateM return
