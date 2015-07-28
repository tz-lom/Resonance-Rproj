test_that("pipe.windowizer", {  
  data <- source.channels(matrix(1:1000, ncol=1, byrow = T), samplingRate = 20)
  
  wnd <- pipe.windowizer(input = data, size = 50,shift = 5)
  
  expect_identical(
    length(wnd),
    191
    )
  
})
  