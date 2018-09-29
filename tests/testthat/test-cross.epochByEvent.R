context("cross.epochByEvent")

doTest <- function(streams, blocks, reference){
  code <- "
  process <- function(){
    createOutput(cross.epochByEvent(input(1), input(2)), 'out')
  }
  "
  online <- run.online(streams, blocks, code)
  offline <- run.offline(streams, blocks, code)
  expect_identical(online, offline)
  expect_identical(online, reference)
}

test_that("empty data", {
  # in
  Csi <- SI.channels(2, 100)
  Esi <- SI.event()
  streams <- list(Csi, Esi)
  blocks <- list(
    DB.channels(Csi, timeoption2ts(Csi, 205), 26:75)
  )
  # out
  epsi <- SI.epoch(2, 100)
  reference <- list(out=makeEmpty(epsi))
  # test
  doTest(streams, blocks, reference)
})

test_that("test1", {
  # in
  Csi <- SI.channels(1, 100)
  Esi <- SI.event()
  streams <- list(Csi, Esi)
  blocks <- list(
    DB.channels(Csi, timeoption2ts(Csi, 201), 1),
    DB.channels(Csi, timeoption2ts(Csi, 225), 2:25),
    DB.event(Esi, 2.119E9 , TRUE),
    DB.event(Esi, 2.185E9 , FALSE)
  )
  # out
  EPsi <- SI.epoch(1, 100)
  reference <- list(out=DB.epoch(EPsi, timeoption2ts(Csi, 218), 12:18))
  # test
  doTest(streams, blocks, reference)
})
