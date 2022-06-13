module MultisetMap () where

import Data.Map (Map)
import qualified Data.Map as Map

insert elem bag = if Map.member elem bag then
                    Map.insert elem (frequency + 1) bag
                  else
                    Map.insert elem 1 bag
                  where
                    frequency = bag Map.! elem

remove elem bag = if not (Map.member elem bag) then
                    bag
                  else if frequency == 1 then
                    Map.delete elem bag
                  else
                    Map.insert elem (frequency - 1) bag
                  where
                    frequency = bag Map.! elem

search elem bag = if not (Map.member elem bag) then 0
                  else bag Map.! elem

union bag1 bag2 = Map.unionWith max bag1 bag2

intersection bag1 bag2 = Map.intersectionWith min bag1 bag2

minus bag1 bag2 = union notInBag2 (Map.filter (> 0) diffBags)
                  where
                    notInBag2 = difference bag1 bag2
                    diffBags = Map.intersectionWith (-) bag1 bag2

sum bag1 bag2 = Map.unionWith (+) bag1 bag2

inclusion bag1 bag2 = undefined

size bag = Map.fold (+) 0 bag
