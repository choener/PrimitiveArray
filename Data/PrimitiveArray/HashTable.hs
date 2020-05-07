
-- | A table representation that internally uses a hashtable from the @hashtables@ library. The
-- implementation is currently a testbed on which idea makes the most sense.
--
-- In particular, once a hashtable has been created with, say, @newWithPA@, it will be completely
-- void of any entries. To prime the system, call @setValidKeys@ which will setup all keys that are
-- vaild, as well as setup an additional data structure to help with @streamUp@ and @streamDown@.
--
-- This table does not store default values, since it is assumed that lookups are only done on valid
-- keys, and @ADPfusion@ as the default consumer should have rules "jump over" missing keys.
--
-- Currently the idea is that any write to an undeclared key will just fail SILENTLY!
--
-- TODO this also forces rethinking @inBounds@, as this will now depend on the internal structure
-- given via @setValidKeys@.

module Data.PrimitiveArray.HashTable where

import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Data.HashTable.Class as HT
import Data.HashTable.IO as HTIO
import Unsafe.Coerce

import Data.PrimitiveArray.Class
import Data.PrimitiveArray.Index.Class



data Hashed ht sh e = Hashed
  { _hashedUpperBound :: !(LimitType sh)
    -- ^ Explicitly store the upper bound.
  , _hashedTable      :: !(IOHashTable ht sh e)
    -- ^ The hashtable to be updated / used.
  , _hashedUpDown     :: !()
    -- ^ Helper structure for the @streamUp@ / @streamDown@ functionality.
    --
    -- TODO this should be a recursively constructed hashtable, based on the shape of @sh@.
  }



-- | Sets valid keys, working within a primitive Monad. The valid keys should be a hashtable with
-- all correct keys, but values set to something resembling a default value. A good choice will be
-- akin to @mzero@.
--
-- Internally, this function uses @unsafeCoerce@ to change the @PrimState@ token held by the hash
-- table to @RealWord@, from whatever it is.
--
-- TODO setup the @hashedUpDown@ part, once it is clear what to do.

setValidKeys
  :: (PrimMonad m, HashTable h)
  => LimitType sh
  -> h (PrimState m) k v
  -> m (Hashed ht sh e)
{-# Inline setValidKeys #-}
setValidKeys ub ks = return $ Hashed
    { _hashedUpperBound = ub
    , _hashedTable      = unsafeCoerce ks
    , _hashedUpDown     = ()
    }

