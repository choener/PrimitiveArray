
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | A general wrapper to transform @Inside@-style indices into
-- @Outside@-style indices.
--
-- We can't automate this completely, as we want to fill tables in
-- a different, for example. Therefor, each index module needs to import
-- the @Outside@ module, then derive the required @Shape@ instances.

module Data.Array.Repa.Index.Outside where

import           Data.Aeson
import           Data.Binary
import           Data.Serialize
import           Data.Vector.Unboxed.Deriving
import           Data.Vector.Unboxed (Unbox(..))
import           GHC.Generics



-- | An outside index is specified by the max bounds 'oMax' and the current
-- index 'oO'.
--
-- TODO It is not quite clear if we actually need the max bounds, if we can
-- extract this information from each syntactic or terminal symbol.

data Outside z = O
  { oMax  :: {-# UNPACK #-} !z
  , oO    :: {-# UNPACK #-} !z
  }
  deriving (Eq,Ord,Show,Generic)

derivingUnbox "Outside"
  [t| forall z . Unbox z => Outside z -> (z,z) |]
  [| \ (O m o) -> (m,o) |]
  [| \ (m,o) -> O m o |]

instance Binary    z => Binary    (Outside z)
instance Serialize z => Serialize (Outside z)
instance ToJSON    z => ToJSON    (Outside z)
instance FromJSON  z => FromJSON  (Outside z)

