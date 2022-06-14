module MultisetList () where

import Data.List as List

getKey elem = fst elem

getFrequency elem = snd elem

updateElem elem updateFunc = (getKey elem, updateFunc (getFrequency elem) 1)

insert' elem [] = [(elem, 1)]
insert' elem bag | getKey (head bag) == elem = [updateElem (head bag) (+)] ++ tail bag
                 | otherwise = [head bag] ++ insert' elem (tail bag)

remove elem [] = []
remove elem bag | getKey (head bag) == elem = if getFrequency (head bag) == 1 then tail bag 
                                              else [updateElem (head bag) (-)] ++ (tail bag)
                | otherwise = [head bag] ++ remove elem (tail bag)

search _ [] = 0
search elem bag | getKey (head bag) == elem = getFrequency (head bag)
                | otherwise = search elem (tail bag)

contains _ [] = False
contains elem bag | getKey (head bag) == elem = True
                  | otherwise = contains elem (tail bag)

notInBag1 bag1 bag2 = [elem | elem <- bag2, not (contains (getKey elem) bag1)]

notInBag2 bag1 bag2 = [elem | elem <- bag1, not (contains (getKey elem) bag2)]

symmetricDiff bag1 bag2 = (notInBag1 bag1 bag2) ++ (notInBag2 bag1 bag2)

union' bag1 bag2 = (symmetricDiff bag1 bag2) ++ (intersectionBy bag1 bag2 max)

intersection bag1 bag2 = intersectionBy bag1 bag2 min

intersectionBy bag1 bag2 intersectFunc = [((getKey elem), intersectFunc (getFrequency elem) (search (getKey elem) bag2)) | elem <- bag1, contains (getKey elem) bag2]

minus bag1 bag2 = [elem | elem <- bag1, not (contains (getKey elem) bag2)] ++ (filter (\e -> (getFrequency e) > 0) diffBags)
                  where
                    diffBags = intersectionBy bag1 bag2 (-)

sum' bag1 bag2 = (symmetricDiff bag1 bag2) ++ (intersectionBy bag1 bag2 (+))

inclusion bag1 bag2 = [elem | elem <- bag1, (getFrequency elem) <= (search (getKey elem) bag2)] /= []

size [] = 0
size bag = getFrequency (head bag) + size (tail bag)
