module Block1.Task1 where

import Text.Read ( readMaybe )

stringSum :: String -> Maybe Int
stringSum = fmap sum . traverse (\x -> readMaybe x :: Maybe Int) . words