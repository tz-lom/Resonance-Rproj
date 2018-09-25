context("DBcombine")

# @todo: empty tests for channels
test_that("channels", {

  si <- SI.channels(5, 20)

  A <- DB.channels(si, timeoption2ts(si, 205), 1:25)
  B <- DB.channels(si, timeoption2ts(si, 215), 26:75)
  C <- DB.channels(si, timeoption2ts(si, 224), 76:120)

  M <- DBcombine(A,B,C)
  O <- DB.channels(si, timeoption2ts(si, 224), 1:120)

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
  attr(target[[1]], 'TS') <- nanotime(3)
  attr(target[[2]], 'TS') <- nanotime(5)
  attr(target[[3]], 'TS') <- nanotime(12)
  SI(target) <- si

  expect_equal(result, target)
})

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

test_that("epoch", {
  si <- SI.epoch(2, 30)
  
  blocks <- list(
    DB.epoch(si, 1E9 , 1:30),
    DB.epoch(si, 4E9 , 31:42),
    makeEmpty(si),
    DB.epoch(si, 12E9, 61:90)
  )
  
  result <- do.call(DBcombine, blocks)
  
  target <- list(
    matrix(as.double(1:30),  ncol=2, byrow=T),
    matrix(as.double(31:42), ncol=2, byrow=T),
    matrix(as.double(61:90), ncol=2, byrow=T)
  )
  TS(target[[1]]) <- nanotime(seq(to=1E9, by=1E9/30, length.out=15))
  TS(target[[2]]) <- nanotime(seq(to=4E9, by=1E9/30, length.out=6))
  TS(target[[3]]) <- nanotime(seq(to=12E9, by=1E9/30, length.out=15))
  SI(target) <- si

  expect_equal(result, target)
})

test_that("epoch empty", {
  si <- SI.epoch(2, 30)
  
  blocks <- list(
    makeEmpty(si),
    makeEmpty(si)
  )
  
  result <- do.call(DBcombine, blocks)
  
  expect_equal(result, makeEmpty(si))
})

test_that("epoch empty 1", {
  si <- SI.epoch(2, 30)
  
  blocks <- list(
    makeEmpty(si)
  )
  
  result <- do.call(DBcombine, blocks)
  
  expect_equal(result, makeEmpty(si))
})
