context("Timers")

timer_test <- function(start, pass){
  processor(
    start, pass, 
    prepare = function(env){
      SI.event()
    },
    online = function(input, pass, onTimeout=NULL){
      
      for(i in input){
        startTimer(timeout = 500, singleShot = T)
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
  expect_identical(online, list(out=reference))
}

test_that("Timed events happens", {
  
  do_test(
    list(
      DB.event(si1, timeoption2ts(si1, seconds(1)), 'A')
    ),
    list(
      DB.event(siO, timeoption2ts(siO, seconds(1.5)), 'A')
    )
  )
})
