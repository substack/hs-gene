{-# LANGUAGE ExtendedDefaultRules #-}
-- ^ tries really hard to use monomorphic types
-- todo: expand PolyTypeable to work with this

import Data.Typeable (Typeable,TypeRep,typeOf,typeRepArgs)
import Data.Dynamic (Dynamic,toDyn)

import qualified Data.Map as M
import Control.Arrow (second,(&&&))
import Control.Monad (filterM)

type TypeTrans = (TypeRep,TypeRep)
type TTFunc = (TypeTrans,Dynamic)
type Pool = M.Map String TTFunc

transOf :: Typeable a => a -> TypeTrans
transOf x = (inT,outT) where
    [inT,outT] = case typeRepArgs $ typeOf x of
        xx@[_,_] -> xx
        args -> error $ show (typeOf x) ++ " : " ++ show args

pool :: Pool
pool = M.fromList [
        ("sin",(transOf &&& toDyn $ sin)),
        ("cos",(transOf &&& toDyn $ cos)),
        ("(+1)",(transOf &&& toDyn $ (+1)))
    ]

findPaths :: TypeTrans -> Pool -> [[(String,TTFunc)]]
findPaths (from,to) pool =
    filter typesMatch
    $ filter ((from ==) . fst . fst . snd . head)
    $ filter ((to ==) . snd . fst . snd . last)
    $ tail $ filterM (const [False,True])
    $ M.assocs pool

typesMatch :: [(String,TTFunc)] -> Bool
typesMatch chain = all checker $ zip fx (tail fx)
    where
        fx :: [TTFunc]
        fx = map snd chain
        
        checker :: (TTFunc,TTFunc) -> Bool
        checker ((x,_),(y,_)) = snd x == fst y
