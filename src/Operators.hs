{-# LANGUAGE TemplateHaskell #-}
module Operators where
import Control.Lens
import Data.Char (toUpper)

data Gate =
    Gate { _open    :: Bool
         , _oilTemp :: Float
         } deriving Show
makeLenses ''Gate

data Army =
    Army { _archers :: Int
         , _knights :: Int
         } deriving Show
makeLenses ''Army

data Kingdom =
    Kingdom { _name :: String
            , _army :: Army
            , _gate :: Gate
            } deriving Show
makeLenses ''Kingdom

duloc :: Kingdom
duloc =
    Kingdom { _name = "Duloc"
            , _army = Army { _archers = 22
                           , _knights = 14
                           }
            , _gate = Gate { _open    = True
                           , _oilTemp = 10.0
                           }
            }

-- >>> duloc & name <>~ ": a perfect place" & army . knights +~ 28 & gate . open &&~ False
-- Kingdom {_name = "Duloc: a perfect place", _army = Army {_archers = 22, _knights = 42}, _gate = Gate {_open = False, _oilTemp = 10.0}}

-- >>> duloc & name <>~ "instein" & army . knights +~ 12 & gate . oilTemp +~ 90 & army . archers -~ 5
-- Kingdom {_name = "Dulocinstein", _army = Army {_archers = 17, _knights = 26}, _gate = Gate {_open = True, _oilTemp = 100.0}}

-- >>> duloc & gate . oilTemp //~ 2 & name <>~ ": Home" & name <<<>~ " of the talking Donkeys"
-- ("Duloc: Home",Kingdom {_name = "Duloc: Home of the talking Donkeys", _army = Army {_archers = 22, _knights = 14}, _gate = Gate {_open = True, _oilTemp = 5.0}})

-- >>> (False, "opossums") & _1 ||~ True
-- (True,"opossums")

-- >>> 2 & id *~ 3
-- 6

-- >>> ((True, "Dudley"), 55.0) & _1 . _2 <>~ " - the worst" & _2 -~ 15 & _2 //~ 2 & _1 . _2 %~ map toUpper & _1 . _1 .~ False
-- ((False,"DUDLEY - THE WORST"),20.0)

-- (%~) :: Lens s t a b -> (a -> b) -> s -> t
