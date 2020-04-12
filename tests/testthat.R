# testthat.R - DESC
# /testthat.R

# Copyright 2003-2020 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira (WMR) <iago.mosqueira@wur.nl>


library(testthat)

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


library(FLCore)

test_check("FLCore")
