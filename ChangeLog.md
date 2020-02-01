# Changelog for safe-decimal

## 0.2.0

* Make rounding dependent on precision type (extra type argument to the `Round` type class)
* Rename `RoundFloor` to `RoundDown` (with `Floor` as synonym)
* Rename `Truncate` to `RoundToZero` (with `Truncate` as synonym)
* Fix conversion `fromRational` to throw `Underflow` instead of rounding. Without this fix
  there was unexpected behavior on negative numbers.
* Addition of `scaleUp` and `scaleUpBounded`
* Addition of `toFixed`, `fromFixed` and `fromFixedBounded`
* Fix `RoundHalfUp` algorithm
* Addition of `RoundHalfDown` and `RoundHalfEven`

## 0.1.0

Initial release
