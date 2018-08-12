context("DBcombine")

# @todo: empty tests for channels
test_that("channels", {

  si <- SI.channels(5, 20)

  A <- DB.channels(si, 3, 1:25)
  B <- DB.channels(si, 3+10*5E4, 26:75)
  C <- DB.channels(si, 3+19*5E4, 76:120)

  M <- DBcombine(A,B,C)
  O <- DB.channels(si, 3+19*5E4, 1:120)

  expect_equal(O, M)
})

test_that("event", {
  si <- SI.event()

  blocks <- list(
    DB.event(si, 3 , 'a'),
    DB.event(si, 5 , 'b'),
    makeEmpty(si),
    DB.event(si, 12, 'c')
  )

  result <- do.call(DBcombine, blocks)
  
  target <- list('a','b','c')
  attr(target[[1]], 'TS') <- 3
  attr(target[[2]], 'TS') <- 5
  attr(target[[3]], 'TS') <- 12
  SI(target) <- si
  class(target) <- 'DB.event'

  expect_equal(result, target)
})

# test_that("epoch", {
#   si <- SI.epoch(2, 30)
#   
#   blocks <- list(
#     DB.epoch(si, 3 , 1:30),
#     DB.epoch(si, 5000 , 31:60),
#     makeEmpty(si),
#     DB.epoch(si, 12000, 61:90)
#   )
#   
#   result <- do.call(DBcombine, blocks)
#   
#   target <- list('a','b','c')
#   attr(target[[1]], 'TS') <- 3
#   attr(target[[2]], 'TS') <- 5
#   attr(target[[3]], 'TS') <- 12
#   SI(target) <- si
#   class(target) <- 'DB.event'
#   
#   expect_equal(result, target)
# })

test_that("event empty", {
  si <- SI.event()
  
  blocks <- list(
    makeEmpty(si),
    makeEmpty(si)
  )
  
  result <- do.call(DBcombine, blocks)
  
  expect_equal(result, makeEmpty(si))
})

test_that("event empty 1", {
  si <- SI.event()
  
  blocks <- list(
    makeEmpty(si)
  )
  
  result <- do.call(DBcombine, blocks)
  
  expect_equal(result, makeEmpty(si))
})

test_that("event", {
  si <- SI.event()
  
  blocks <- list(
    makeEmpty(si),
    makeEmpty(si)
  )
  
  result <- do.call(DBcombine, blocks)
  
  expect_equal(result, makeEmpty(si))
})

