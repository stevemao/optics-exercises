{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module SimpleFolds where
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Lens

beastSizes :: [(Int, String)]
beastSizes = [(3, "Sirens"), (882, "Kraken"), (92, "Ogopogo")]

-- >>> beastSizes ^.. folded
-- [(3,"Sirens"),(882,"Kraken"),(92,"Ogopogo")]

-- >>> beastSizes ^.. folded . folded
-- ["Sirens","Kraken","Ogopogo"]

-- >>> beastSizes ^.. folded . folded . folded
-- "SirensKrakenOgopogo"

-- >>> beastSizes ^.. folded . _2
-- ["Sirens","Kraken","Ogopogo"]

-- >>> toListOf (folded . folded) [[1, 2, 3], [4, 5, 6]]
-- [1,2,3,4,5,6]

-- >>> toListOf (folded . folded) (M.fromList [("Jack", "Captain" :: String), ("Will", "First Mate")])
-- "CaptainFirst Mate"

-- >>> ("Hello" :: String, "It's me") ^.. both . folded
-- "HelloIt's me"

-- >>> ("Why", "So", "Serious?") ^.. each
-- ["Why","So","Serious?"]

quotes :: [(T.Text, T.Text, T.Text)]
quotes = [("Why", "So", "Serious?"), ("This", "is", "SPARTA")] 

-- >>> quotes ^.. each . each . each
-- "WhySoSerious?ThisisSPARTA"

-- >>> folded' :: Fold [a] a; folded' = folded
-- >>> _1' :: Fold (a, b) a; _1' = _1

-- >>> toListOf (folded . _1) [(1, 'a'), (2, 'b'), (3, 'c')]
-- [1,2,3]

-- >>> folded' :: Fold (S.Set a) a; folded' = folded
-- >>> _2' :: Fold (a, b) b; _2' = _2
-- >>> toListOf' :: Fold (a, S.Set b) b -> (a, S.Set b) -> [b]; toListOf' = toListOf

-- >>> toListOf (_2 . folded) (False, S.fromList ["one", "two", "three"])
-- ["one", "two", "three"]

-- >>> [1, 2, 3] ^.. folded
-- [1, 2, 3]

-- >>> ("Light", "Dark") ^.. _1
-- ["Light"]

-- >>> [("Light", "Dark"), ("Happy", "Sad")] ^.. folded . each
-- ["Light","Dark","Happy","Sad"]

-- >>> [("Light", "Dark"), ("Happy", "Sad")] ^.. folded . _1
-- ["Light","Happy"]

-- >>> [("Light" :: String, "Dark" :: String), ("Happy", "Sad")] ^.. folded . folded . folded
-- "DarkSad"

-- >>> ("Bond", "James", "Bond") ^.. each
-- ["Bond","James","Bond"]
