context("cross.windowizeByEvents")

doTest <- function(streams, blocks, reference){
  code <- "
  process <- function(){
  createOutput(cross.windowizeByEvents(input(1), input(2), 11), 'out')
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
    DB.channels(Csi, timeoption2ts(Csi, 211), 26:75)
  )
  # out
  Wsi <- SI.window(2, 11, 100)
  reference <- list(out=makeEmpty(Wsi))
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
    DB.event(Esi, timeoption2ts(Csi, 202) , TRUE),
    DB.event(Esi, timeoption2ts(Csi, 301) , FALSE)
  )
  # out
  Wsi <- SI.window(1, 11, 100)
  reference <- list(
    out=DBcombine(
      DB.window(Wsi, timeoption2ts(Csi, 212), 2:12)
      )
  )
  # test
  doTest(streams, blocks, reference)
})
