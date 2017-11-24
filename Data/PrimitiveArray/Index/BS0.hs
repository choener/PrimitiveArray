
module Data.PrimitiveArray.Index.BS0 where

import           Data.Aeson (FromJSON,ToJSON,FromJSONKey,ToJSONKey)
import           GHC.Generics (Generic)
import           Data.Binary (Binary)
import           Data.Bits
import           Data.Bits.Extras
import           Data.Hashable (Hashable)
import           Data.Serialize (Serialize)
import           Data.Vector.Unboxed.Deriving
import           Data.Vector.Unboxed (Unbox(..))
import           Debug.Trace
import           Control.Lens

import           Data.PrimitiveArray.Index.IOC
import           Data.Bits.Ordered



-- | Newtype for a bitset. We'd use @Word@s but that requires more shape
-- instances.
--
-- TODO can we use @Word@s now?

newtype BitSet t = BitSet { _bitSet :: Word }
  deriving (Eq,Ord,Read,Show,Generic,FiniteBits,Ranked,Num,Bits)
makeLenses ''BitSet

instance FromJSON     (BitSet t)
instance FromJSONKey  (BitSet t)
instance ToJSON       (BitSet t)
instance ToJSONKey    (BitSet t)

