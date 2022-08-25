rec-def - Pure recursive definition
===================================

This library provides safe APIs that allow you to define and calculate values
recursively, and still get a result out:

    >>> :{
      let s1 = rInsert 23 s2
          s2 = rInsert 42 s1
      in getr s1
     :}
    fromList [23,42]

See the [`examples.hs`](examples.hs) file for more examples.

It also provides (unsafe) building blocks to build such APIs, see `Data.Recursive.R.Internal`.

Related work
------------

Edward Kmett's [`Data.Propagator.Prop` module](https://github.com/ekmett/propagators/blob/master/src/Data/Propagator/Prop.hs) achieves something similar, and allows to construct more the graphs more flexibly, but requires a stricter phase control akin to `runST`.
