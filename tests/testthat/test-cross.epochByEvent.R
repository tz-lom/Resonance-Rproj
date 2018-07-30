test_that("cross.epochByEvent", {
  Csi <- SI.channels(2, 100)
  Esi <- SI.event()

  blocks <- list(
    DB.channels(Csi, 3, 1:2),
    DB.channels(Csi, 3+10*5E4, 26:75),
    DB.event(Esi, 30 , TRUE),
    DB.event(Esi, 3E4 , FALSE)
  )


  code <- function(){
    
    process <- function(){
      createOutput(cross.epochByEvent(input(1), input(2)), 'out')
    }
  
  }

  A <- run.online(list(Csi, Esi), blocks, code)
  B <- run.offline(list(Csi, Esi), blocks, code)

  expect_equal(A, B)
})
