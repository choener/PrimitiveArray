
module Data.Array.Repa.Index.Lens where



class IndexLens i where
  _from :: Functor f => (Int -> f Int) -> i -> f i
  _to   :: Functor f => (Int -> f Int) -> i -> f i

