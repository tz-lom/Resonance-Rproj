test_that("cross.windowizeByEvents", {  
  data <- block.processor("channels", channels=1, samplingRate=7)
  events <- block.processor("event")
  
  wbe <- cross.windowizeByEvents(
    data, events, 10
    )
  
  rec <- drain.debug(wbe)
  
  sample <- as.integer64(1E9/7)
  
  #sending some data
  data$emit(DataBlock(matrix(1:5), (7+5)*sample))
  data$emit(DataBlock(matrix(6:10), (7+10)*sample))
  data$emit(DataBlock(matrix(11:21), (7+21)*sample))
  
  events$emit(DataBlock(T, (7+2)*sample+9))
  
  expect_equal(
    rec$last_received(),
    DataBlock(matrix(3:12), (7+12)*sample)
  )
  expect_less_than(
    abs(attr(rec$last_received(), 'timestamp')-(7+12)*sample),
    sample/2
    )
  
  events$emit(DataBlock(T, (7+18)*sample+9))
  
  data$emit(DataBlock(matrix(22:29), (7+29)*sample))
  
  expect_equal(
    rec$last_received(),
    DataBlock(matrix(19:28), (7+28)*sample)
  )
  expect_less_than(
    abs(attr(rec$last_received(), 'timestamp')-(7+28)*sample),
    sample/2
  )
  
  events$emit(DataBlock(T, (7+31)*sample+9)) 
  
  data$emit(DataBlock(matrix(30:56), (7+56)*sample))
  
  expect_equal(
    rec$last_received(),
    DataBlock(matrix(32:41), (7+41)*sample)
  )
  expect_less_than(
    abs(attr(rec$last_received(), 'timestamp')-(7+41)*sample),
    sample/2
  )
  
})