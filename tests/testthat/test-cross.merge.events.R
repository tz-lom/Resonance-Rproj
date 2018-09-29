context("cross.merge.events")

test_that("basic", {
  
  Esi1 <- SI.event(id=1)
  Esi2 <- SI.event(id=2)
  Esi3 <- SI.event(id=3)
  
  blocks <- list(
    DB.event(Esi3, timeoption2ts(NULL, seconds(2.3)), 'A'),
    DB.event(Esi1, timeoption2ts(NULL, seconds(1.3)), 'B'),
    DB.event(Esi2, timeoption2ts(NULL, seconds(2.1)), 'C')
  )
  
  code <- "
  process = function(){
  createOutput(cross.merge.events(input(1), input(2), input(3)), 'out')
  }
  ";

  online <- run.online(list(Esi1, Esi2, Esi3), blocks, code)
  offline <- run.offline(list(Esi1, Esi2, Esi3), blocks, code)
  
  si <- SI.event()
  reference <- list(
    out = DBcombine(
      DB.event(si, timeoption2ts(NULL, seconds(1.3)), 'B'), 
      DB.event(si, timeoption2ts(NULL, seconds(2.1)), 'C'),
      DB.event(si, timeoption2ts(NULL, seconds(2.3)), 'A')
    )
  )
  
  expect_identical(online, offline)
  expect_identical(reference, online);
})
