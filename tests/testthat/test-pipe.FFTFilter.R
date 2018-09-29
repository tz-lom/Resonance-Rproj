context("pipe.FFTFilter")

test_that("basic", {

  Csi <- SI.window(5, 100, 2)
  
  set.seed(1337)
  pseudo_random_input <- rnorm(2000)

  blocks <- list(
    DB.window(Csi, timeoption2ts(Csi, 2), pseudo_random_input[1:500]),
    DB.window(Csi, timeoption2ts(Csi, 3), pseudo_random_input[501:1000]),
    DB.window(Csi, timeoption2ts(Csi, 4), pseudo_random_input[1001:1500])
  )

  code <- "
sel <- data.frame(
channel=c(1,3,2,1),
frequency=c(5,12,4,7)
)

process = function(){
  createOutput(pipe.FFTFilter(input(1), sel), 'out')
}
  ";

  online <- run.online(list(Csi), blocks, code)
  offline <- run.offline(list(Csi), blocks, code)
  
  si <- SI.channels(4, 2)
  reference <- list(
    out = DB.channels(
      si, 
      timeoption2ts(Csi, 4), 
      c(0.19083362557658468, 0.050832575068065794, 0.049000837743308295, 0.051800814793282531, 
        0.16183061760788886, 0.10191230584688295, 0.094325432774268345, 0.066603271903211358,
        0.074338613502755502, 0.021387163363998939, 0.008787565969400439, 0.11614993381326849
        ))
  )
  
  expect_identical(online, offline)
  expect_identical(reference, online);
})
