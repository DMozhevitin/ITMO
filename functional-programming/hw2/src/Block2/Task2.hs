module Block2.Task2
  (moving) where

import Control.Monad.State (State, execState, get, put)
import Data.Sequence (Seq(..), length, empty, (|>))
import Prelude hiding (sum, length)
import Control.Monad (forM_)

moving :: (Fractional a) => Int -> [a] -> [a]
moving n ys = moving' ys (empty, 0.0, []) where

    moving' xs initialState = case execState (forM_ xs push) initialState of
        (_, _, res) -> reverse res

    push :: (Fractional a) => a -> State (Seq a, a, [a]) ()
    push x = do
        (prefix, sum, ans) <- get
        let (h :<| t) = prefix

        let (extracted, prefix') = if length prefix == n then 
                (h, t)
            else
                (0, prefix)
            

        let prefix'' = prefix' |> x
        let sum' = sum - extracted + x
        let len = length prefix''
        let avg' = sum' / fromIntegral len
        
        put (prefix'', sum', avg' : ans)
