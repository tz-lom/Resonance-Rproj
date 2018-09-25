context("pipe.FFTFilter")

test_that("basic", {

  Csi <- SI.window(5, 100, 2)
  
  set.seed(1337)
  pseudo_random_input <- rnorm(2000)

  blocks <- list(
    DB.window(Csi, timeoption2ts(Csi, 211), pseudo_random_input[1:500]),
    DB.window(Csi, timeoption2ts(Csi, 311), pseudo_random_input[501:1000]),
    DB.window(Csi, timeoption2ts(Csi, 411), pseudo_random_input[1001:1500])
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
      timeoption2ts(Csi, 411), 
      c(0.190833625576585, 0.0508325750680658, 0.0490008377433083, 0.0518008147932825,
        0.161830617607889, 0.101912305846883, 0.0943254327742683, 0.0666032719032114,
        0.0743386135027555, 0.0213871633639989, 0.00878756596940044, 0.116149933813269))
  )
  
  expect_equal(online, offline)
  expect_equal(reference, online);
})
