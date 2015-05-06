test_that("correct Epoch collection", {
  
  data <- source.channels(matrix(1:3000, ncol=3, byrow = T), samplingRate = 20)
  events <- source.events(c(F,T,F,F,T,T,F), c(0,1,5,8,12,14,16)*1E6 )
  
  epoched <- cross.epochByEvent(data, events)
  
  
  expect_identical(
    c(20:99, 240:319)*20,
    attr(epoched, TS)
  )
})
