# is.na() tests for NA
# is.nan() tests for NAN
# NA values have class like: integer NA, character NA...
# NAN values ia also NA but NA is not NAN
x = c(1, 2, NA, 10, 3)
is.na(x)
is.nan(x)

x = c(1, 2, NaN, NA, 4)
is.na(x)
is.nan(x)
