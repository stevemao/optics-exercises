module SelfCorrecting where
import Control.Lens

data ProducePrices =
    ProducePrices { _limePrice  :: Float
                 , _lemonPrice :: Float
                 }
    deriving Show

limePrice :: Lens' ProducePrices Float
limePrice = lens getter setter
    where getter price = _limePrice price
          setter (ProducePrices limep lemonp) newValue
            | lemonp - newValue' > 0.5 = ProducePrices newValue' (newValue' + 0.5)
            | newValue' - lemonp > 0.5 = ProducePrices newValue' (newValue' - 0.5)
            | otherwise = ProducePrices newValue' lemonp
                where newValue' = if newValue < 0 then 0 else newValue

-- >>> let prices = ProducePrices 1.50 1.48
-- >>> set limePrice 2 prices
-- ProducePrices {_limePrice = 2.0, _lemonPrice = 1.5}
-- >>> set limePrice 1.8 prices
-- ProducePrices {_limePrice = 1.8, _lemonPrice = 1.48}
-- >>> set limePrice 1.63 prices
-- ProducePrices {_limePrice = 1.63, _lemonPrice = 1.48}
-- >>> set limePrice  (-1.00) prices
-- ProducePrices {_limePrice = 0.0, _lemonPrice = 0.5}

