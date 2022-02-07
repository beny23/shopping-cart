module Cart where

import qualified Data.Map.Strict as M

type Product = (String, Double)
type Cart = M.Map Product Int

empty :: Cart
empty = M.empty

add :: Int -> Product -> Cart -> Cart
add quantity product cart = M.insertWith (+) product quantity cart

subTotal :: Cart -> Double
subTotal = sum . map (\((_, p), q) -> p * fromIntegral q) . M.toList

tax :: Double -> Cart -> Double
tax rate cart = roundCents $ rate * subTotal cart

-- Rounding in Haskell can be a bit strange: https://stackoverflow.com/questions/10738569/round-to-nearest-integer-strange-results
roundCents :: Double -> Double
roundCents x = (/100.0) $ fromIntegral $ floor $ (+0.5) $ x * 100.0

total :: Double -> Cart -> Double
total rate cart = subTotal cart + tax rate cart