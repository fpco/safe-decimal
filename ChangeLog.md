# Changelog for decimal64

## 0.2.0

* Make rounding dependent on precision type (extra type argument to the `Round` type class)
* Fix conversion `fromRational` to throw `Underflow` instead of rounding. Without this fix
  there was unexpected behavior on negative numbers.
* Addition of `scaleUp` and `scaleUpBounded`
* Addition of `toFixed`, `fromFixed` and `fromFixedBounded`

## 0.1.0

Initial release
