module Validators where

import Data.Either (Either(..))
import Data.List (List, singleton)
import Data.String (length, contains)
import Data.String.Pattern (Pattern(..))
import Prelude ((<), (>))
import Type.Data.Boolean (kind Boolean)

type Validator e a = a -> Either (List e) a

makeValidator :: forall a e. (a -> Boolean) -> e -> Validator e a
makeValidator f error a = if f a
                      then Left (singleton error)
                      else Right a

-- form validators...

data Error = Empty | TooLong | ContainsHorse

longEnough :: Validator Error String
longEnough = makeValidator (\a -> length a < 1) Empty

tooLong :: Validator Error String
tooLong = makeValidator (\a -> length a > 10) TooLong

containsHorse :: Validator Error String
containsHorse = makeValidator (contains (Pattern "horse")) ContainsHorse
