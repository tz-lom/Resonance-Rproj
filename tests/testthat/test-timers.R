context("Timers")

timer_test <- function(start, pass){
  processor(
    start, pass, 
    prepare = function(env){
      SI.event()
    },
    online = function(input, pass, onTimeout=NULL){
      
      for(i in input){
        args <- list(
          timeout = 500,
          singleShot = T,
          data = TRUE,
          call = 'onTimeout'
        )
        for(n in names(i)){
          args[[n]] <- i[[n]]
        }
        do.call(startTimer, args)
      }
      
      if(is.null(onTimeout)){
        pass
      } else {
        list(onTimeout)
      }
    }
  )
}

si1 <- SI.event(id=1)
si2 <- SI.event(id=2)
siO <- SI.event()

code = "
  process = function(){
    createOutput(timer_test(input(1), input(2)), 'out')
  }
  "

do_test <- function(blocks, reference){
  online <- run.online(list(si1, si2), blocks, code, env=environment())
  expect_identical(list(out=reference), online)
}

test_that("Timer triggers after last block", {
  do_test(
    list(
      DB.event(si1, timeoption2ts(si1, seconds(1)), list())
    ),
    DBcombine(
      DB.event(siO, timeoption2ts(siO, seconds(1.5)), TRUE)
    )
  )
})

test_that("Timer triggers after between blocks", {
  do_test(
    list(
      DB.event(si1, timeoption2ts(si1, seconds(1)), list()),
      DB.event(si2, timeoption2ts(si2, seconds(2)), 'B')
    ),
    DBcombine(
      DB.event(siO, timeoption2ts(siO, seconds(1.5)), TRUE),
      DB.event(siO, timeoption2ts(siO, seconds(2)), 'B')
    )
  )
})

test_that("Timers reorder correctly", {
  do_test(
    list(
      DB.event(si1, timeoption2ts(si1, seconds(1)), list(data='a')),
      DB.event(si1, timeoption2ts(si1, seconds(1.01)), list(data='b', timeout=400)),
      DB.event(si2, timeoption2ts(si2, seconds(2)), 'Z')
    ),
    DBcombine(
      DB.event(siO, timeoption2ts(siO, seconds(1.41)), 'b'),
      DB.event(siO, timeoption2ts(siO, seconds(1.5)), 'a'),
      DB.event(siO, timeoption2ts(siO, seconds(2)), 'Z')
    )
  )
})

test_that("Timers reorder correctly after last block", {
  do_test(
    list(
      DB.event(si1, timeoption2ts(si1, seconds(1.01)), list(data='a')),
      DB.event(si1, timeoption2ts(si1, seconds(1.02)), list(data='b', timeout=400)),
      DB.event(si1, timeoption2ts(si1, seconds(1.03)), list(data='c', timeout=221))
    ),
    DBcombine(
      DB.event(siO, timeoption2ts(siO, seconds(1.251)), 'c'),
      DB.event(siO, timeoption2ts(siO, seconds(1.420)), 'b'),
      DB.event(siO, timeoption2ts(siO, seconds(1.510)), 'a')
    )
  )
})

test_that("Timers reorder correctly separated by block", {
  do_test(
    list(
      DB.event(si1, timeoption2ts(si1, seconds(1.01)), list(data='a')),
      DB.event(si1, timeoption2ts(si1, seconds(1.02)), list(data='b', timeout=400)),
      DB.event(si1, timeoption2ts(si1, seconds(1.03)), list(data='c', timeout=221)),
      DB.event(si2, timeoption2ts(si2, seconds(1.31)), 'Z')
    ),
    DBcombine(
      DB.event(siO, timeoption2ts(siO, seconds(1.251)), 'c'),
      DB.event(siO, timeoption2ts(siO, seconds(1.310)), 'Z'),
      DB.event(siO, timeoption2ts(siO, seconds(1.420)), 'b'),
      DB.event(siO, timeoption2ts(siO, seconds(1.510)), 'a')
    )
  )
})

test_that("Timers can be repeative", {
  do_test(
    list(
      DB.event(si1, timeoption2ts(si1, seconds(1.01)), list(data='a', singleShot=F)),
      DB.event(si2, timeoption2ts(si2, seconds(1.31)), 'A'),
      DB.event(si2, timeoption2ts(si2, seconds(1.61)), 'B'),
      DB.event(si2, timeoption2ts(si2, seconds(2.12)), 'C'),
      DB.event(si2, timeoption2ts(si2, seconds(2.25)), 'D'),
      DB.event(si2, timeoption2ts(si2, seconds(4.31)), 'E')
    ),
    DBcombine(
      DB.event(siO, timeoption2ts(siO, seconds(1.31)), 'A'),
      DB.event(siO, timeoption2ts(siO, seconds(1.51)), 'a'),
      DB.event(siO, timeoption2ts(siO, seconds(1.61)), 'B'),
      DB.event(siO, timeoption2ts(siO, seconds(2.01)), 'a'),
      DB.event(siO, timeoption2ts(siO, seconds(2.12)), 'C'),
      DB.event(siO, timeoption2ts(siO, seconds(2.25)), 'D'),
      DB.event(siO, timeoption2ts(siO, seconds(2.51)), 'a'),
      DB.event(siO, timeoption2ts(siO, seconds(3.01)), 'a'),
      DB.event(siO, timeoption2ts(siO, seconds(3.51)), 'a'),
      DB.event(siO, timeoption2ts(siO, seconds(4.01)), 'a'),
      DB.event(siO, timeoption2ts(siO, seconds(4.31)), 'E'),
      DB.event(siO, timeoption2ts(siO, seconds(4.51)), 'a')
    )
  )
})
