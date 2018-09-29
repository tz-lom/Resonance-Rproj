context("pipe.eventsEquals")

test_that("simple", {
  si <- SI.event()
  
  blocks <- list(
    DB.event(si, 30, 'test'),
    DB.event(si, 40, 'A'),
    DB.event(si, 50, 'test'),
    DB.event(si, 60, 'B')
  )
  
  code <- "
  process <- function(){
  createOutput(pipe.eventsEquals(input(1), 'test'), 'out')
  }
  "
  
  online <- run.online(list(si), blocks, code)
  offline <- run.offline(list(si), blocks, code)
  
  reference <- list(out=DBcombine( 
    DB.event(si, 30, TRUE),
    DB.event(si, 40, FALSE),
    DB.event(si, 50, TRUE),
    DB.event(si, 60, FALSE)
  ))
  
  expect_identical(online, offline)
  expect_identical(online, reference)
})

