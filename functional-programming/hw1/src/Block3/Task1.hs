module Block3.Task1 ( maybeConcat
                    , eitherConcat
                    ) where

import           Data.Maybe ()

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat xs = case mconcat xs of
    Nothing  -> []
    Just res -> res


eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat xs = foldl comb (mempty, mempty) xs where
    comb (accl, accr) (Left x)  = (accl <> x, accr)
    comb (accl, accr) (Right x) = (accl, accr <> x)
