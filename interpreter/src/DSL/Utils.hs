module DSL.Utils
  where

import Prettyprinter
import Data.Ratio

-- | This is for the purpose of pretty printing with parentheses
class Pretty a => Nesting a where
  prettyNested :: Int -- | Precendence
                  -> a
                  -> Doc ann

instance Nesting Int where prettyNested _ = pretty
instance Nesting Double where prettyNested _ = pretty
instance Nesting Char where prettyNested _ = pretty

parensWhen :: Bool -> Doc ann -> Doc ann
parensWhen True d = parens d
parensWhen False d = d

instance (Eq a, Num a, Show a) => Pretty (Ratio a) where
  pretty x
    | numerator x == 0 = pretty "0"
    | numerator x == denominator x = pretty "1"
    | otherwise = pretty (show x)

ratioPrecedence :: Int
ratioPrecedence = 4

instance (Eq a, Num a, Show a) => Nesting (Ratio a) where
  prettyNested prec x
    | numerator x == 0 = pretty "0"
    | numerator x == denominator x = pretty "1"
    | otherwise = parensWhen (prec < ratioPrecedence) $ pretty x

