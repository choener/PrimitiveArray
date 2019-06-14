0.9.1.1
-------

- OrderedBits version bump

0.9.1.0
-------

- Arbitrary instance(s), field lenses that are probably not a good idea (don't use them!)

0.9.0.0
-------

- large-scale changes
- associated data families for bounds

0.8.1.0
-------

- inclusion of Upperlimit data family to simplify declaration of upper limits

0.8.0.1
-------

- PointL delays inlining to phase 0 for table filling. This is part of the
  close-to-C optimization effort for linear languages.
- disabling smallcheck until I fix how things are generated

0.8.0.0
-------

- renamed Interface (Iter) to Boundary (Boundary)
- Typeable instances for Dense primitive arrays
- EdgeBoundary index structure
- changes and fixes to quickcheck/smallcheck
- added ScoreMatrix module with simple score and distance matrix structure
  (requires log-domain)

0.7.2.0
-------

- JSONKey (To/From) for index types.

0.7.1.0
-------

- minor updates to dependencies
- tasty framework
- Subword/upper triangular indexing provided by DPutils

0.7.0.1
-------

- Data.PrimitiveArray.Checked to capture index out-of-bounds problems

0.7.0.0
-------

- vector <= 0.11 support; including compatibility layer
- redesigned Index structures (for dealing with Inside/Outside/Complement)

0.6.1.1
-------

- Hashable instances for all index structures
- Hashable instances for Unboxed and Boxed arrays. *These require Hashable
  instances for vectors, which are not available by default*

0.6.1.0
-------

- OrderedBits < 0.0.1
- travis.yml update

0.6.0.0
-------

- moved primitive array classes to Data.PrimitiveArray.Class
- added from / to lenses
- Field1 .. Field6 lenses for indices (Z:.a:.b...) (with Z being Field0)
  - lens stuff currently commented out; aiming to have an extra package [lens
    is fairly heavy]
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
- removed Data.Array.Repa.Index.Point (we have PointL, PointR in Points.hs)
- added   Data.Array.Repa.Index.Set (for sets with an interface, used by
  Hamiltonian path problems)
- Data.Array.Repa.Index.Outside is now just a newtype wrapped around other
  Index types. We want to be able to say "a Subword, but for Outside
  algorithms"
- travis-ci integration

0.5.4.0
-------

- actually implemented PointR

- added the rather important strictness annotation for mutable arrays in .Zero

0.5.3.0
-------

- fixed vector-th-unbox problem
