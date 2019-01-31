# safe-decimal

An implementation of a decimal point data type, that is backed by any custom integral type. It is
safe, because all runtime exceptions and integer overflows are prevented on arithmetic operations,
namely things like integer overflows, underflows, division by zero etc. are checked for during the
runtime.
