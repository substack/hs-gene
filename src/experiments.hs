{-# LANGUAGE ExtendedDefaultRules #-}
-- ^ tries really hard to use monomorphic types

--import Data.PolyTypeable (polyTypeOf)
--import Data.PolyTypeable.Utils
import Data.Typeable (TypeRep,typeOf,typeRepArgs)
import Data.Dynamic (Dynamic,toDyn,fromDynamic,dynApp)

import qualified Data.Map as M
import Control.Arrow (second,(&&&))

type Pool = M.Map String (TypeRep,Dynamic)

pool :: Pool
pool = M.fromList [
        ("sin",(typeOf &&& toDyn $ sin)),
        ("cos",(typeOf &&& toDyn $ cos)),
        ("(+1)",(typeOf &&& toDyn $ (+1)))
    ]

findPaths :: TypeRep -> TypeRep -> Pool -> [[String]]
findPaths from to pool
    | (from == to) && hasTo = [["id"]] ++ paths
    | otherwise = paths
    where
        hasTo = elem to
            $ map (head . typeRepArgs . fst)
            $ M.elems pool
        
        paths = concatMap pathf $ M.keys pool
        
        pathf :: String -> [[String]]
        pathf k = map (k :)
            $ filter (not . null)
            $ findPaths
                (last . typeRepArgs . fst $ pool M.! k)
                to
                (M.delete k pool)
