3.3.1 [2022.11.30]
------------------
* Add `Boring` and `Absurd` instances for infinite streams.
* Add a `head :: Stream a -> a` function to `Data.Stream.Infinite`.

3.3
---
* Removed a number of redundant parts of the API. If a method you were using has been removed, consider the classes available. No functionality was lost.
* Better support for GHC 7.10, the `Foldable (length, null)` members are now defined directly leading to asymptotic improvements and helping to further shrink the API.
* Added `prepend` and `concat` functions to `Data.Stream.Infinite`
* Allow `comonad-5`

3.2.2
-----
* Bug fix in `Data.Stream.Infinite.Skew` and removed `fromList`.

3.2.1
-----
* Add support for `semigroupoids` 5 and GHC 7.10

3.2
---
* Switched to `tabulate` and `index` from `adjunctions`. Note: this reversed the argument order to `index`.
* Proper upper bounds on dependencies.

3.1.1
-----
* Marked modules appropriately Trustworthy

3.0.1
-----
* Removed intra-package dependency bounds for my packages
* Build system improvements
* IRC Buildbot notification

3.0
---
* Version sync with the rest of my packages

0.7
--
* "Data.Stream.NonEmpty" renamed to "Data.List.NonEmpty" and pushed upstream into the semigroups package.

0.6.3
-----
* Removed a redundant UNPACK pragma

0.5.1
-----
* Data.Stream.Supply added

Since 0.1:
----------
* A number of strictness issues with 'NonEmpty' were fixed
* More documentation
