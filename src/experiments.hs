{- Find all valid function composition chains between two monomorphic types
   given some pool of monomorphic functions.

    Example:
    > let t = typeOf (undefined :: Double) \
    in map (map fst) $ findPaths (t,t) pool
    [["sin"],["cos"],["cos","sin"]]

-}

{-# LANGUAGE ExtendedDefaultRules #-}
-- ^ tries really hard to make everything use monomorphic types
-- todo: expand PolyTypeable to work with this

{-# LANGUAGE TypeSynonymInstances #-}

import Data.Typeable (Typeable,TypeRep,typeOf,typeRepArgs)
import Data.Dynamic (Dynamic,toDyn)

import qualified Data.Map as M
import Data.Function (on)
import Data.List (inits,nubBy,permutations)
import Control.Arrow (second,(&&&))
import Control.Monad (filterM)

type TypeTrans = (TypeRep,TypeRep)
type TTFunc = (TypeTrans,Dynamic)
type TTChain = [(String,TTFunc)]

type Pool = M.Map String TTFunc

transOf :: Typeable a => a -> TypeTrans
transOf x = (inT,outT) where
    [inT,outT] = case typeRepArgs $ typeOf x of
        xx@[_,_] -> xx
        args -> error $ "Can't decompose type: " ++ show (typeOf x)

pool :: Pool
pool = M.fromList [
        ("sin",(t sin)),
        ("cos",(t cos)),
        ("tan",(t tan)),
        --("acos",(t acos)),
        ("succ",(t (succ :: Int -> Int))),
        ("pred",(t (pred :: Int -> Int)))
    ]
    where
        t :: Typeable a => a -> TTFunc
        t = transOf &&& toDyn

findPaths :: TypeTrans -> Pool -> [TTChain]
findPaths (from,to) pool =
    -- inner types agree:
    filter typesMatch
    -- outer types match up:
    $ filter ((from ==) . fst . fst . snd . head)
    $ filter ((to ==) . snd . fst . snd . last)
    -- all possible chains: (could be more efficient)
    $ nubBy ((==) `on` map fst)
    $ concatMap (tail . inits)
    $ permutations
    $ M.assocs pool

typesMatch :: TTChain -> Bool
typesMatch chain = all checker $ zip fx (tail fx)
    where
        fx :: [TTFunc]
        fx = map snd chain
        
        checker :: (TTFunc,TTFunc) -> Bool
        checker ((x,_),(y,_)) = snd x == fst y
