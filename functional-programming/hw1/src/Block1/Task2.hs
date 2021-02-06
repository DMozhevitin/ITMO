module Block1.Task2
  ( Nat(..)
  , isEven
  , isOdd
  ) where

data Nat = Z | S Nat deriving Show

isEven :: Nat -> Bool
isEven Z         = True
isEven (S Z)     = False
isEven (S (S n)) = isEven n

isOdd :: Nat -> Bool
isOdd = not . isEven


instance Num Nat where
    (+) Z n            = n
    (+) n Z            = n
    (+) n@(S _) (S m') = S n + m'

    (*) Z _            = Z
    (*) _ Z            = Z
    (*) (S Z) n        = n
    (*) n (S Z)        = n
    (*) n@(S _) (S m') = n + n * m'

    (-) Z _           = Z
    (-) n Z           = n
    (-) (S n') (S m') = n' - m'

    fromInteger n = fromInteger' n Z where
        fromInteger' 0 res  = res
        fromInteger' n' cnt = fromInteger' (pred n') (S cnt)

    abs n = n

    signum Z = 0
    signum _ = 1


instance Eq Nat where
    (==) Z Z           = True
    (==) Z _           = False
    (==) _ Z           = False
    (==) (S n') (S m') = n' == m'

instance Ord Nat where
    (<=) n m = (n - m == Z)

instance Enum Nat where
    succ n = S n

    pred Z     = Z
    pred (S n) = n

    toEnum = fromInteger . toInteger

    fromEnum = fromInteger . toInteger

instance Integral Nat where
    quotRem n m = quotRem' n m Z where
        quotRem' n' m' cnt
            | n' < m' = (cnt, n')
            | otherwise = quotRem' (n' - m') m' (S cnt)

    div n m = a where
        (a, _) = quotRem n m

    mod n m = b where
        (_, b) = quotRem n m

    toInteger n = toInteger' n 0 where
        toInteger' Z res      = res
        toInteger' (S n') cnt = toInteger' n' (succ cnt)


instance Real Nat where
    toRational = fromIntegral . toInteger


