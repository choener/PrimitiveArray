
-- | A set index structure, where the underlying set allows (up to) three
-- states: @0,1,2@ are allowed for each element of the set. These sets come
-- with boundary information attached as well.
--
-- The access semantics of these sets are somewhat tricky. The important
-- question is if the states @0,1,2@ need to always be accessed in order or
-- not.

module Data.PrimitiveArray.Index.TernarySet where



-- | 

