{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module CustomFolds where
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Lens
import Data.Maybe

newtype Name = Name
    { _getName :: String
    } deriving Show
makeLenses ''Name

data ShipCrew = ShipCrew
    { _shipName :: Name
    , _captain    :: Name
    , _firstMate  :: Name
    , _conscripts :: [Name]
    } deriving (Show)
makeLenses ''ShipCrew

-- >>> Name "Two-faced Tony" ^. getName
-- "Two-faced Tony"

-- >>> :t getName
-- >>> :t to _getName
-- getName :: (Profunctor p, Functor f) => p String (f String) -> p Name (f Name)
-- to _getName :: (Profunctor p, Contravariant f) => Optic' p f Name String

-- >>> ["Yer", "a", "wizard", "Harry"] ^.. folded . folded
-- "YerawizardHarry"

-- >>> [[1, 2, 3], [4, 5, 6]] ^.. folded . folding (take 2)
-- [1, 2, 4, 5]

-- >>> [[1, 2, 3], [4, 5, 6]] ^.. folded . to (take 2)
-- [[1,2], [4,5]]

-- >>> ["bob", "otto", "hannah"] ^.. folded . to reverse
-- ["bob", "otto", "hannah"]

-- >>> ("abc", "def") ^.. folding (\(a, b) -> [a, b]). to reverse . folded
-- "cbafed"




-- >>> [1..5] ^.. folded . to (*100)
-- [100,200,300,400,500]

-- >>> (1, 2) ^.. each
-- [1,2]

-- >>> [(1, "one"), (2, "two")] ^.. folded . folded
-- ["one", "two"]

-- >>> (Just 1, Just 2, Just 3) ^.. each . folded
-- [1, 2, 3]

-- >>> [Left 1, Right 2, Left 3] ^.. folded . folded
-- [2]

-- >>> [([1, 2], [3, 4]), ([5, 6], [7, 8])] ^.. folded . each . folded
-- [1, 2, 3, 4, 5, 6, 7, 8]

-- >>> [1, 2, 3, 4] ^.. folded . to (\a -> if odd a then Left a else Right a)
-- [Left 1, Right 2, Left 3, Right 4]

-- >>> [(1, (2, 3)), (4, (5, 6))] ^.. each . folding (\(a, (b, c)) -> [a, b, c])
-- [1,2,3,4,5,6]

eitherToList :: Either e a -> [a]
eitherToList (Right a) = [a]
eitherToList (Left e) = []

-- >>> [(Just 1, Left "one"), (Nothing, Right 2)] ^.. folded . folding (\(a, b) -> maybeToList a <> eitherToList b)
-- [1, 2]

-- >>> [(1, "one"), (2, "two")] ^.. folded . folding (\(a, b) -> [Left a, Right b])
-- [Left 1, Right "one", Left 2, Right "two"]

-- >>> S.fromList ["apricots", "apples"] ^.. folded . to reverse . folded
-- "selppastocirpa"



-- >>> [(12, 45, 66), (91, 123, 87)] ^.. folded . _2 . to show . to reverse . folded
-- "54321"

-- >>> [(1, "a"), (2, "b"), (3, "c"), (4, "d")] ^.. folded . to (\(a, b) -> if a `mod` 2 == 0 then [b] else []) . folded
-- ["b", "d"]
