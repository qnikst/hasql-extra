{-# LANGUAGE RankNTypes #-}
module Hasql.Decoders
  ( module HD
  , enumP
  , jsonbDecode
  ) where

import Control.Lens
import Data.Bifunctor
import Data.Aeson
import qualified Data.Text as T
import "hasql" Hasql.Decoders as HD

-- | Decode json value with a standard parser.
jsonbDecode :: FromJSON a => HD.Value a
jsonbDecode = jsonbBytes (first T.pack . eitherDecodeStrict)

-- | Enum using Prism value.
enumP :: Prism' T.Text a -> HD.Value a
enumP p = HD.enum (^? p)
