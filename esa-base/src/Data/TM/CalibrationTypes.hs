{-|
Module      : Data.TM.CalibrationTypes
Description : General data types for calibrations
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module provides some types used in calibrations
-}
{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
    , BinaryLiterals
    , NumericUnderscores
    , FlexibleInstances
    , GADTs
    , ExistentialQuantification
#-}
module Data.TM.CalibrationTypes
  ( Calibrate(..)
  , CalibInterpolation(..)
  , toCalibInterpolation
  )
where

import           RIO
import           Codec.Serialise
import           Data.Aeson

import           Data.TM.Value


-- | Specifies what a calibration should do when the
-- given value falls out of the range of the calibration
-- (only applicable to 'NumericCalibration').
data CalibInterpolation =
    -- | The calibration will extrapolate the value from the last 2 points within the range
    CalibExtrapolate
    -- | The calibration will indicate an error
    | CalibFail
    deriving (Eq, Ord, Enum, Show, Read, Generic)

instance NFData CalibInterpolation
instance Serialise CalibInterpolation
instance FromJSON CalibInterpolation
instance ToJSON CalibInterpolation where
  toEncoding = genericToEncoding defaultOptions

-- | Converts from a Charactor to the interpolation type.
-- Specified in the SCOS-2000 MIB ICD 6.9
toCalibInterpolation :: Char -> CalibInterpolation
toCalibInterpolation 'P' = CalibExtrapolate
toCalibInterpolation _   = CalibFail


-- | This class provides the function to calibrate a value.
-- The first value is the calibration used.
class Calibrate a where
    calibrate :: a -> TMValue -> TMValue

