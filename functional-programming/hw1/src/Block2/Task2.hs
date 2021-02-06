module Block2.Task2 ( splitOn
                    , joinWith
                    ) where

import           Data.List.NonEmpty as NonEmpty

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn d xs = foldr splitter ([] :| [])  xs where
    splitter x acc@(h :| t)
        | x == d = [] <| acc
        | otherwise = (x : h) :| t

joinWith :: a -> NonEmpty [a] -> [a]
joinWith d xs = foldl1 joiner xs where
    joiner acc x = acc ++ [d] ++ x


