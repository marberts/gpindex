# Changes in version 0.1.2

- Functions that say the input should be a vector now check if the input is a vector. This prevents non-name attributes from causing problems in the calculations (like dim).

- weights_change() now allows 'M' to be NA/NaN, returning a vector of NAs.
