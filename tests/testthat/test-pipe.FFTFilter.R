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
      c(0x1.86d3c7a6a0d9dp-3, 0x1.a06ba2efa44f8p-5, 0x1.916a3472b10b3p-5, 
        0x1.a85a2eae2f866p-5, 0x1.4b6dd9d0f05ccp-3, 0x1.a16ecc4ac1ca1p-4, 
        0x1.825b628f24fbdp-4, 0x1.10ce97aa181e7p-4, 0x1.307daf9d3068fp-4, 
        0x1.5e6843ccf725fp-6, 0x1.1ff372399a2a6p-7, 0x1.dbc00872919fdp-4
        ))
  )
  
  expect_identical(online, offline)
  expect_identical(reference, online);
})
