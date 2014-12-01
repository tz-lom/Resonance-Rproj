test_that("source.channels", {
  s <- source.channels(1:20, blockSize=10, samplingFrequency=25)
  d <- drain.debug(s)
  
  pump(s)
  
  expect_equal(
    d$all_received(),
    list(
      DataBlock(matrix(1:10), 1E9/25),
      DataBlock(matrix(11:20), 1E9/25*2)
    )
  )
})