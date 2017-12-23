
test_that("eegData online", {
  
  load('data.Rdata')
  
  newResult <- run.online(inputs = data$streams, data$blocks, classifier)

  expect_equal(newResult, result)  
  
})