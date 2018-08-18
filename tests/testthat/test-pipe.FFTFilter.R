context("pipe.FFTFilter")

test_that("basic", {

  Csi <- SI.channels(5, 100)

  blocks <- list(
    DB.channels(Csi, 3, 1:500),
    DB.channels(Csi, 3+1E6, 501:1000),
    DB.channels(Csi, 3+3E6, 1001:2000)
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
  ";

  online <- run.online(list(Csi), blocks, code)
  offline <- run.offline(list(Csi), blocks, code)
  
  si <- SI.channels(4, 1)
  reference <- list(
    out = DB.channels(si, 3+3E6, c(15.9811330537492, 6.79117972916468, 19.9468243888987, 11.4603596425684, 
                                   15.9811330537492, 6.79117972916467, 19.9468243888987, 11.4603596425684,
                                   15.9811330537492, 6.79117972916468, 19.9468243888987, 11.4603596425684,
                                   15.9811330537492, 6.79117972916467, 19.9468243888987, 11.4603596425684))
  )
  
  expect_equal(online, offline)
  expect_equal(reference, online);
})
