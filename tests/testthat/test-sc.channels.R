context("sc.channels")

test_that("Small time deviations are ignored", {
  
  si <- SI.channels(1, 120)
  streams <- list(si)
  blocks <- list(
    DB.channels(si, 1E6/120*6+5, 1:6),
    DB.channels(si, 1E6/120*12-4000, 1:6),
    DB.channels(si, 1E6/120*18+4000, 1:6)
  )
  
  code <- "
process=function(){
  createOutput(sc.channels(input(1)), 'out')
}"
  
  reference <- list(out=DB.channels(si, 1E6/120*18+5, c(1:6,1:6,1:6)))
  
  online <- run.online(streams, blocks, code)
  offline <- run.offline(streams, blocks, code)
  expect_equal(online, offline)
  expect_equal(online, reference)
})

test_that("Large time deviations can be asserted", {
  
})

test_that("Large time deviations assert can be turned off", {
  
})
