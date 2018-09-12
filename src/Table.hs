{-# LANGUAGE DeriveGeneric #-}

module Table where

import qualified GHC.Generics as Generics (Generic)
import qualified Data.Aeson as Aeson (FromJSON, ToJSON)

data Table
  = Table {
    tableId :: Integer,
    tableText :: String
  }
  deriving (Eq, Show, Generics.Generic)

instance Aeson.ToJSON Table
instance Aeson.FromJSON Table