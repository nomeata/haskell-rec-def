# Revision history for rec-def

## 0.2.1 -- 2023-02-18

* Add `Data.Recursive.Set.when`

## 0.2 -- 2022-09-22

* The naive propagator does not use `(==)` to detect changes, but a custom
  operator, to allow faster and less restricted operators.
* Module structure refactoring
* No more `R` type constructor, instead individual `RBool` etc. types
* Addition of `Data.Recursive.Map`
* A space leak is fixed

## 0.1 -- 2022-09-03

* First version. Released on an unsuspecting world.
