0.6.0.0

- moved primitive array classes to Data.PrimitiveArray.Class
- added _from / _to lenses
- Field1 .. Field6 lenses for indices (Z:.a:.b...) (with Z being Field0)
  - lens stuff currently commented out; aiming to have an extra package [lens is fairly heavy]
- FillTables should work now (with PointL, Subword)
- freezing of whole stacks of (Z:.mutarr:.mutarr:. ...) tables
- explicit 'Shape Subword'; this allows for simpler code in a number of places
  and is especially useful for CYK-style algorithms that have a
  single-dimensional upper-triangular matrix.
- rangeStream of Extshape is new and used by the FillTables module
- Binary, Cereal, Aeson instances for indices and immutable tables
- orphan instances of Binary, Cereal, Aeson for Z, and (:.)
- topmostIndex returns the final index position for CYK-style (bottom to top)
  parsing

0.5.4.0

- actually implemented PointR

- added the rather important strictness annotation for mutable arrays in .Zero

0.5.3.0

- fixed vector-th-unbox problem
