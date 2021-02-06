module Block1.Task1
  ( DayOfWeek(..)
  , nextDay
  , afterDays
  , isWeekend
  ,daysToParty
  ) where

data DayOfWeek = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving (Show, Eq)

nextDay :: DayOfWeek -> DayOfWeek
nextDay Sunday    = Monday
nextDay Monday    = Tuesday
nextDay Tuesday   = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday  = Friday
nextDay Friday    = Saturday
nextDay Saturday  = Sunday

afterDays :: DayOfWeek -> Int -> DayOfWeek
afterDays d 0 = d
afterDays d n = afterDays (nextDay d) (pred n)

isWeekend :: DayOfWeek -> Bool
isWeekend Sunday   = True
isWeekend Saturday = True
isWeekend _        = False


daysToParty :: DayOfWeek -> Int
daysToParty d = daysToParty' d 0 where
    daysToParty' Friday res = res
    daysToParty' d' cnt     = daysToParty' (nextDay d') (succ cnt)
