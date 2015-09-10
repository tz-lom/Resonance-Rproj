test_that("cross.mergeChannels", {  
  data1 <- source.channels(matrix(as.double(1:30), ncol=3, byrow = T), samplingRate = 20)
  data2 <- source.channels(matrix(as.double(1:10), ncol=1, byrow = T), samplingRate = 20)
  data3 <- source.channels(matrix(as.double(1:20), ncol=2, byrow = T), samplingRate = 20)
  
  wnd <- cross.mergeChannels(data1, data2, data3)
  
  expect_identical(
    dim(wnd),
    c(6,10)
    )
  
})
  