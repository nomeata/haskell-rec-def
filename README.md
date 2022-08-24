rec-val - Pure recursive definition
===================================

This library provides safe APIs that allow you to define and calculate values
recursively, and still get a result out:

    >>> :{
      let s1 = pInsert 23 s2
          s2 = pInsert 42 s1
      in getPSet s1
     :}
    fromList [23,42]

See the <./examples.hs> file for more example.

It also provides (unsafe) building blocks to build such APIs, see `Data.Recursive`.

Related work
------------

Edward Kmett's [`Data.Propagator.Prop` module](https://github.com/ekmett/propagators/blob/master/src/Data/Propagator/Prop.hs) achieves something similar, and allows to construct more the graphs more flexibly, but requires a stricter phase control akin to `runST`.
