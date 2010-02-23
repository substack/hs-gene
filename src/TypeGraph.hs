{-# LANGUAGE ExtendedDefaultRules #-}

import Data.Typeable (Typeable,TypeRep,typeOf,typeRepArgs,mkAppTy)
import Data.Dynamic (Dynamic,toDyn)
import Control.Arrow ((&&&))

import Data.Graph.Class

import qualified Data.Map as M

type TTChain = [(String,[TypeRep])]

type Pool = M.Map String [TypeRep]

typeReps :: TypeRep -> [TypeRep]
typeReps t = case typeRepArgs t of
    [x,y] -> x : typeReps y
    [] -> [t]

{-
pool :: Pool
pool = M.fromList [
        ("sin",(t sin)),
        ("cos",(t cos)),
        ("tan",(t tan)),
        ("atan2",(t atan2)),
        --("acos",(t acos)),
        ("succ",(t (succ :: Int -> Int))),
        ("pred",(t (pred :: Int -> Int)))
        --("(-1)",(t (-1)))
    ]
    where t = typeReps . typeOf
-}
