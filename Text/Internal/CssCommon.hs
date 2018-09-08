{-# OPTIONS_HADDOCK hide #-}
-- | This module is only being exposed to work around a GHC bug, its API is not stable

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Text.Internal.CssCommon where

import Text.Internal.Css
import Text.MkSizeType
import qualified Data.Text as TS
import Text.Printf (printf)
import Language.Haskell.TH
import Data.Word (Word8)
import Data.Bits
import qualified Data.Text.Lazy as TL

renderCssUrl :: (url -> [(TS.Text, TS.Text)] -> TS.Text) -> CssUrl url -> TL.Text
renderCssUrl r s = renderCss $ s r

data Color = Color Word8 Word8 Word8
    deriving Show
instance ToCss Color where
    toCss (Color r g b) =
        let (r1, r2) = toHex r
            (g1, g2) = toHex g
            (b1, b2) = toHex b
         in fromText $ TS.pack $ '#' :
            if r1 == r2 && g1 == g2 && b1 == b2
                then [r1, g1, b1]
                else [r1, r2, g1, g2, b1, b2]
      where
        toHex :: Word8 -> (Char, Char)
        toHex x = (toChar $ shiftR x 4, toChar $ x .&. 15)
        toChar :: Word8 -> Char
        toChar c
            | c < 10 = mkChar c 0 '0'
            | otherwise = mkChar c 10 'A'
        mkChar :: Word8 -> Word8 -> Char -> Char
        mkChar a b' c =
            toEnum $ fromIntegral $ a - b' + fromIntegral (fromEnum c)

colorRed :: Color
colorRed = Color 255 0 0

colorBlack :: Color
colorBlack = Color 0 0 0

-- CSS size wrappers

-- | Create a CSS size, e.g. $(mkSize "100px").
mkSize :: String -> ExpQ
mkSize s = appE nameE valueE
  where [(value, unit)] = reads s :: [(Double, String)]
        absoluteSizeE = varE $ mkName "absoluteSize"
        nameE = case unit of
          "cm" -> appE absoluteSizeE (conE $ mkName "Centimeter")
          "em" -> conE $ mkName "EmSize"
          "ex" -> conE $ mkName "ExSize"
          "in" -> appE absoluteSizeE (conE $ mkName "Inch")
          "mm" -> appE absoluteSizeE (conE $ mkName "Millimeter")
          "pc" -> appE absoluteSizeE (conE $ mkName "Pica")
          "pt" -> appE absoluteSizeE (conE $ mkName "Point")
          "px" -> conE $ mkName "PixelSize"
          "%" -> varE $ mkName "percentageSize"
          _ -> error $ "In mkSize, invalid unit: " ++ unit
        valueE = litE $ rationalL (toRational value)

-- | Absolute size units.
data AbsoluteUnit = Centimeter
                  | Inch
                  | Millimeter
                  | Pica
                  | Point
                  deriving (Eq, Show)

-- | Not intended for direct use, see 'mkSize'.
data AbsoluteSize = AbsoluteSize
    { absoluteSizeUnit :: AbsoluteUnit -- ^ Units used for text formatting.
    , absoluteSizeValue :: Rational -- ^ Normalized value in centimeters.
    }

-- | Absolute size unit convertion rate to centimeters.
absoluteUnitRate :: AbsoluteUnit -> Rational
absoluteUnitRate Centimeter = 1
absoluteUnitRate Inch = 2.54
absoluteUnitRate Millimeter = 0.1
absoluteUnitRate Pica = 12 * absoluteUnitRate Point
absoluteUnitRate Point = 1 / 72 * absoluteUnitRate Inch

-- | Constructs 'AbsoluteSize'. Not intended for direct use, see 'mkSize'.
absoluteSize :: AbsoluteUnit -> Rational -> AbsoluteSize
absoluteSize unit value = AbsoluteSize unit (value * absoluteUnitRate unit)

instance Show AbsoluteSize where
  show (AbsoluteSize unit value') = printf "%f" value ++ suffix
    where value = fromRational (value' / absoluteUnitRate unit) :: Double
          suffix = case unit of
            Centimeter -> "cm"
            Inch -> "in"
            Millimeter -> "mm"
            Pica -> "pc"
            Point -> "pt"

instance Eq AbsoluteSize where
  (AbsoluteSize _ v1) == (AbsoluteSize _ v2) = v1 == v2

instance Ord AbsoluteSize where
  compare (AbsoluteSize _ v1) (AbsoluteSize _ v2) = compare v1 v2

instance Num AbsoluteSize where
  (AbsoluteSize u1 v1) + (AbsoluteSize _ v2) = AbsoluteSize u1 (v1 + v2)
  (AbsoluteSize u1 v1) * (AbsoluteSize _ v2) = AbsoluteSize u1 (v1 * v2)
  (AbsoluteSize u1 v1) - (AbsoluteSize _ v2) = AbsoluteSize u1 (v1 - v2)
  abs (AbsoluteSize u v) = AbsoluteSize u (abs v)
  signum (AbsoluteSize u v) = AbsoluteSize u (abs v)
  fromInteger x = AbsoluteSize Centimeter (fromInteger x)

instance Fractional AbsoluteSize where
  (AbsoluteSize u1 v1) / (AbsoluteSize _ v2) = AbsoluteSize u1 (v1 / v2)
  fromRational x = AbsoluteSize Centimeter (fromRational x)

instance ToCss AbsoluteSize where
  toCss = fromText . TS.pack . show

-- | Not intended for direct use, see 'mkSize'.
data PercentageSize = PercentageSize
    { percentageSizeValue :: Rational -- ^ Normalized value, 1 == 100%.
    }
                    deriving (Eq, Ord)

-- | Constructs 'PercentageSize'. Not intended for direct use, see 'mkSize'.
percentageSize :: Rational -> PercentageSize
percentageSize value = PercentageSize (value / 100)

instance Show PercentageSize where
  show (PercentageSize value') = printf "%f" value ++ "%"
    where value = fromRational (value' * 100) :: Double

instance Num PercentageSize where
  (PercentageSize v1) + (PercentageSize v2) = PercentageSize (v1 + v2)
  (PercentageSize v1) * (PercentageSize v2) = PercentageSize (v1 * v2)
  (PercentageSize v1) - (PercentageSize v2) = PercentageSize (v1 - v2)
  abs (PercentageSize v) = PercentageSize (abs v)
  signum (PercentageSize v) = PercentageSize (abs v)
  fromInteger x = PercentageSize (fromInteger x)

instance Fractional PercentageSize where
  (PercentageSize v1) / (PercentageSize v2) = PercentageSize (v1 / v2)
  fromRational x = PercentageSize (fromRational x)

instance ToCss PercentageSize where
  toCss = fromText . TS.pack . show

-- | Converts number and unit suffix to CSS format.
showSize :: Rational -> String -> String
showSize value' unit = printf "%f" value ++ unit
  where value = fromRational value' :: Double

mkSizeType "EmSize" "em"
mkSizeType "ExSize" "ex"
mkSizeType "PixelSize" "px"
