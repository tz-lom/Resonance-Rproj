test_that("cross.windowizeByEvents", {
  Csi <- SI.channels(2, 100)
  Esi <- SI.event()
  
  
  blocks <- list(
    DB.channels(Csi, 3+10*5E4, 26:75),
    DB.event(Esi, 3 , 'a')
  )
  
  
  code <- "
process <- function(){
  createOutput(cross.windowizeByEvents(input(1), input(2), 20, 3), 'out')
}
  "
  
  code <- "
process <- function(){
  createOutput(input(1), 'out')
}
  "

  foo <- function(x){
    processor(x,
              prepare = function(env){
                cat("======================================================> prepare\n")
                print(env)
                print(x)
                
                SI(x)
              },
              online = function(Y){
                cat("------------------------------------------------------> online\n")
                print(x)
                print(Y)
              }
              )
  }
 
  A <- run.online(list(Csi, Esi), blocks, code)
  B <- run.offline(list(Csi, Esi), blocks, code)
   
  expect_equal(A, B)
})