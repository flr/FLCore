# helper-expect.R - DESC
# ~/Projects/FLR/code/FLCore/tests/testthat/helper-expect.R

# Copyright (c) WMR, 2026.
# Author: Iago MOSQUEIRA <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# expect_validclass {{{

expect_validclass <- function(object, class) {
  
  # CAPTURE object and label
  act <- quasi_label(rlang::enquo(object), arg="object")

  # EXPECTATIONS

  # CLASS
  expect(
    is(act$val, class),
    sprintf("%s is not of class '%s'", act$lab, class)
    )

  # VALIDITY
  act$valid <- validObject(act$val)
  expect(
    all(act$valid),
    sprintf("%s is not a valid %s object", act$lab, class)
    )

  # RETURN value
  invisible(act$val)
} # }}}

# expect_equal_values {{{

expect_equal_values <- function(object, expected, ...) {
  # CAPTURE object and label
  act <- quasi_label(rlang::enquo(object), arg="object")

  # EXPECTATIONS
  expect(
    is(act$val, "FLQuant"),
    sprintf("%s is not of class 'FLQuant'", act$lab)
    )
  expect(
    is(expected, "FLQuant"),
    sprintf("expected is not of class 'FLQuant'")
    )
  expect_equal(
    c(act$val),
    c(expected),
    ...
    )

  # RETURN value
  invisible(act$val)
}
# }}}


