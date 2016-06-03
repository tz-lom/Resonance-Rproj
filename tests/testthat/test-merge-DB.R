test_that("merge.DB.channels", {

  si <- SI.channels(5, 20)
  
  A <- DB.channels(si, 3, 1:25)
  B <- DB.channels(si, 3+10*5E4, 26:75)
  C <- DB.channels(si, 3+19*5E4, 76:120)
  
  M <- merge.DB.channels(A,B,C)
  O <- DB.channels(si, 3+19*5E4, 1:120)
  
  expect_equal(O, M)
})

