context("In is out")

test <- function(si, blocks){
  code <- "process <- function() { createOutput(input(1), 'out') }"
  online <- run.online(list(si), blocks, code)
  offline <- run.offline(list(si), blocks, code)

  origin <- if(length(blocks)>0){
    list(out=do.call(DBcombine, blocks))
  } else {
    list(out=makeEmpty(si))
  }
  expect_identical(online, origin)
  expect_identical(online, offline)
}

test_that("channels", {
  si <- SI.channels(5, 20)
  blocks <- list()
  test(si, blocks)
  
  blocks <- list(
    DB.channels(si, timeoption2ts(si, 105), 1:25),
    DB.channels(si, timeoption2ts(si, 115), 26:75),
    DB.channels(si, timeoption2ts(si, 124), 76:120)
  )
  test(si, blocks)
})

test_that("events", {
  si <- SI.event()
  blocks <- list()
  test(si, blocks)
  
  blocks <- list(
    DB.event(si, 10, 'one'),
    DB.event(si, 40, 'two'),
    DB.event(si, 80, 'three')
  )
  test(si, blocks)
})

# @todo: make this tests passing
#
# test_that("epoch", {
#   si <- SI.epoch(3, 12.1)
#   blocks <- list()
#   test(si, blocks)
#   
#   blocks <- list(
#     DB.epoch(si, 10, 1:21),
#     DB.epoch(si, 80, 1:21),
#     DB.epoch(si, 110, 1:21)
#   )
#   test(si, blocks)
# })
# 
# test_that("window", {
#   si <- SI.window(3, 9, 12.1)
#   blocks <- list()
#   test(si, blocks)
# 
#   blocks <- list(
#     DB.window(si, 10, 1:27),
#     DB.window(si, 80, 1:27),
#     DB.window(si, 110, 1:27)
#   )
#   test(si, blocks)
# })
