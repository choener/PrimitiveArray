
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- | Serialization code for repa data types. These are all orphan
-- instances.

module Data.Array.Repa.Bytes where

import Data.Aeson
import Data.Array.Repa.Index ( (:.) (..), Z (..) )
import Data.Binary
import Data.Serialize
import GHC.Generics



deriving instance Generic Z
deriving instance Generic (a:.b)

instance Binary Z
instance (Binary a, Binary b) => Binary (a:.b)

instance Serialize Z
instance (Serialize a, Serialize b) => Serialize (a:.b)

instance ToJSON Z
instance (ToJSON a, ToJSON b) => ToJSON (a:.b)

instance FromJSON Z
instance (FromJSON a, FromJSON b) => FromJSON (a:.b)

