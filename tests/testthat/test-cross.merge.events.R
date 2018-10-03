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

test_that("delay", {
  
  Esi1 <- SI.event(id=1)
  Esi2 <- SI.event(id=2)
  Esi3 <- SI.event(id=3)
  
  blocks <- list(
    DB.event(Esi1, timeoption2ts(NULL, seconds(1.31)), 'A'),
    DB.event(Esi2, timeoption2ts(NULL, seconds(1.32)), 'B'),
    DB.event(Esi3, timeoption2ts(NULL, seconds(1.33)), 'C')
  )
  
  code <- "
  process = function(){
  createOutput(cross.merge.events(input(1), input(2)), 'out1')
  createOutput(input(3), 'out2')
  }
  ";
  
  result_blocks <- run.online(list(Esi1, Esi2, Esi3), blocks, code, returnBlocks=TRUE)
  online <- run.online(list(Esi1, Esi2, Esi3), blocks, code)
  offline <- run.offline(list(Esi1, Esi2, Esi3), blocks, code)
  
  si <- SI.event()
  reference <- list(
    out1=list(
      makeEmpty(si),
      
    ),
    out2=list(
      
    )
  )
  
  expect_identical(online, offline)
  expect_identical(reference, blocks);
})
