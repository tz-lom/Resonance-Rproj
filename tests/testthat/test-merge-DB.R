test_that("DBcombine.DB.channels", {

  si <- SI.channels(5, 20)
  
  A <- DB.channels(si, 3, 1:25)
  B <- DB.channels(si, 3+10*5E4, 26:75)
  C <- DB.channels(si, 3+19*5E4, 76:120)
  
  M <- DBcombine(A,B,C)
  O <- DB.channels(si, 3+19*5E4, 1:120)
  
  expect_equal(O, M)
})

test_that("merge.DB.event", {
  si <- SI.event()
  
  A <- DB.event(si, 3 , 'a')
  B <- DB.event(si, 5 , 'b')
  C <- DB.event(si, 12, 'c')
  
  M <- DBcombine(A,B,C)
  O <- list('a','b','c')
  attr(O[[1]], 'TS') <- 3 
  attr(O[[2]], 'TS') <- 5
  attr(O[[3]], 'TS') <- 12
  SI(O) <- si
  class(O) <- 'DB.event'
  
  expect_equal(O, M)
})

