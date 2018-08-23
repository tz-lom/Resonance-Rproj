context("cross.epochByEvent")

doTest <- function(streams, blocks, reference){
  code <- "
  process <- function(){
    createOutput(cross.epochByEvent(input(1), input(2)), 'out')
  }
  "
  online <- run.online(streams, blocks, code)
  offline <- run.offline(streams, blocks, code)
  expect_equal(online, offline)
  expect_equal(online, reference)
}

test_that("empty data", {
  # in
  Csi <- SI.channels(2, 100)
  Esi <- SI.event()
  streams <- list(Csi, Esi)
  blocks <- list(
    DB.channels(Csi, 3+10*5E4, 26:75)
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
    DB.channels(Csi, 3, 1),
    DB.channels(Csi, 3+10*5E4, 2:25),
    DB.event(Esi, 30 , TRUE),
    DB.event(Esi, 45E4 , FALSE)
  )
  # out
  EPsi <- SI.epoch(1, 100)
  reference <- list(out=DB.epoch(EPsi, 3+44E4, 2:19))
  # test
  doTest(streams, blocks, reference)
})
