context("pipe.FFTFilter")

test_that("basic", {

  Csi <- SI.channels(5, 100)

  blocks <- list(
    DB.channels(Csi, 3, 1:500),
    DB.channels(Csi, 3+1E6, 501:1000),
    DB.channels(Csi, 3+2E6, 1001:2000)
  )

  code <- "
sel <- data.frame(
channel=c(1,3,2,1),
frequency=c(5,12,4,7)
)

process = function(){
  S1 <- pipe.windowizer(input(1), 100, 100)
  createOutput(pipe.FFTFilter(S1, sel), 'out')
}
  "

  online <- run.online(list(Csi), blocks, code)
  offline <- run.offline(list(Csi), blocks, code)

  #@todo: add validation of results
  
  expect_equal(online, offline)
})
