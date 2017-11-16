library(testthat)
library(Resonance)


expect_streams_equal <- function(a, b, ..., info = NULL, label = NULL, expected.label = NULL){
  lab_act <- make_label(object, label)
  lab_exp <- make_label(expected, expected.label)

  comp <- compare(object, expected, ...)
  
  expect(comp$equal, sprintf("%s not equal to %s.\n%s", lab_act, 
                               lab_exp, comp$message), info = info)
  invisible(a)
}

test_check("Resonance")
