module Composition where
import Control.Lens

-- >>> view (_2 . _1 . _2) ("Ginerva", (("Galileo", "Waldo"), "Malfoy"))
-- "Waldo"

-- fiveEightDomino :: Lens' Five Eight
-- mysteryDomino   :: Lens' Eight Two
-- twoThreeDomino  :: Lens' Two Three

-- dominoTrain :: Lens' Five Three
-- dominoTrain = fiveEightDomino . mysteryDomino . twoThreeDomino

-- Functor f => (Armadillo -> f Hedgehog) -> (Platypus -> f BabySloth)
-- Lens Platypus BabySloth Armadillo Hedgehog

-- spuzorktrowmble   ::  Lens Chumble      Spuzz      Gazork       Trowlg
-- gazorlglesnatchka ::  Lens Gazork       Trowlg     Bandersnatch Yakka
-- zinkattumblezz    ::  Lens Zink         Wattoom    Chumble      Spuzz
-- gruggazinkoom     ::  Lens Grug         Pubbawup   Zink         Wattoom
-- banderyakoobog    ::  Lens Bandersnatch Yakka      Foob         Mog
-- boowockugwup      ::  Lens Boojum       Jabberwock Grug         Pubbawup
-- snajubjumwock     ::  Lens Snark        JubJub     Boojum       Jabberwock


-- Lens Snark        JubJub  Foob         Mog
