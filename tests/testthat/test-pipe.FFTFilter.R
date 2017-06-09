test_that("pipe.FFTFilter", {
  
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
  
  parallelCodeOffline <- "
sel <- data.frame(
channel=c(1,3,2,1),
frequency=c(5,12,4,7)
)

process = function(){
  S1 <- pipe.windowizer(input(1), 100, 100)
  createOutput(pipe.FFTFilter(S1, sel, cluster=3), 'out')
}
"

  parallelCodeOnline <- "
sel <- data.frame(
channel=c(1,3,2,1),
frequency=c(5,12,4,7)
)

cl <- makeCluster(2)

process = function(){
  S1 <- pipe.windowizer(input(1), 100, 100)
  createOutput(pipe.FFTFilter(S1, sel, cluster=cl), 'out')
}
"

  
  A <- run.online(list(Csi), blocks, code)
  B <- run.offline(list(Csi), blocks, code)
  
  C <- run.online(list(Csi), blocks, parallelCodeOffline)
  D <- run.offline(list(Csi), blocks, parallelCodeOffline)
  
  E <- run.online(list(Csi), blocks, parallelCodeOnline)
  F <- run.offline(list(Csi), blocks, parallelCodeOnline)
  
  expect_equal(A$out, B$out, check.attributes = F)
  expect_equal(C$out, D$out, check.attributes = F)
  expect_equal(E$out, F$out, check.attributes = F)
  expect_equal(A$out, C$out, check.attributes = F)
  expect_equal(A$out, E$out, check.attributes = F)
  
  
})