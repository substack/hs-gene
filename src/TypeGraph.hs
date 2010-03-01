{-# LANGUAGE ExtendedDefaultRules #-}

import Data.Typeable (Typeable,TypeRep,typeOf,typeRepArgs,typeRepKey)
import Data.Graph.Class
import qualified Data.Map as M
import Control.Arrow ((***))
import Control.Monad (join)
import System.IO.Unsafe (unsafePerformIO)
import Data.Ord (comparing)

typeReps :: TypeRep -> [TypeRep]
typeReps t = case typeRepArgs t of
    [x,y] -> x : typeReps y
    [] -> [t]

type Label = String
(==>) :: Typeable a => Label -> a -> (Label,[TypeRep])
label ==> t = (label, typeReps $ typeOf t)

type Pool = M.Map Label [TypeRep]
pool :: Pool
pool = M.fromList [
        "sin" ==> sin,
        "cos" ==> cos,
        "tan" ==> tan,
        "atan2" ==> atan2,
        "succ" ==> (succ :: Int -> Int),
        "pred" ==> (pred :: Int -> Int),
        "0.0" ==> 0.0,
        "0" ==> 0
    ]

instance Ord TypeRep where
    compare = comparing (unsafePerformIO . typeRepKey)
