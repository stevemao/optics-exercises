module Polymorphic where
import Control.Lens

-- vorpal :: Lens (Vorpal x) (Vorpal y) x y

data Preferences a b =
    Preferences { _best :: a
                , _worst :: b
              }
    deriving (Show)

best :: Lens (Preferences a b) (Preferences a' b) a a'
best = lens getter setter
    where getter = _best
          setter pref newValue = pref{_best = newValue}

worst :: Lens (Preferences a b) (Preferences a b') b b'
worst = lens getter setter
    where getter = _worst
          setter pref newValue = pref{_worst = newValue}

data Result e =
    Result { _lineNumber :: Int
           , _result     :: Either e String
           }

-- result :: Lens (Result e) (Result e') e e'

data Predicate a =
    Predicate (a -> Bool)

pred :: Lens (Predicate a) (Predicate b) (a -> Bool) (b -> Bool)
pred = lens getter setter
    where getter (Predicate f) = f
          setter _ newValue = Predicate newValue
