context("cross.merge.events")

test_that("default", {
  Esi1 <- SI.event(id=1)
  Esi2 <- SI.event(id=2)
  
  blocks <- list(
    DBcombine(
      DB.event(Esi1, timeoption2ts(NULL, seconds(1.1)), 'A')
    ),
    DBcombine(
      DB.event(Esi2, timeoption2ts(NULL, seconds(2.3)), 'B'),
      DB.event(Esi2, timeoption2ts(NULL, seconds(2.4)), 'C')
    ),
    DB.event(Esi1, timeoption2ts(NULL, seconds(5.1)), 'D')
  )
  
  code <- "
  process = function(){
  createOutput(cross.merge.events(input(1), input(2)), 'out')
}
  ";
  
  online <- run.offline(list(Esi1, Esi2), blocks, code)
  offline <- run.offline(list(Esi1, Esi2), blocks, code)
  
  si <- SI.event()
  reference <- list(
    out = DBcombine(
      DB.event(si, timeoption2ts(NULL, seconds(1.1)), 'A'),
      DB.event(si, timeoption2ts(NULL, seconds(2.3)), 'B'),
      DB.event(si, timeoption2ts(NULL, seconds(2.4)), 'C'),
      DB.event(si, timeoption2ts(NULL, seconds(5.1)), 'D')
    )
  )
  
  expect_identical(reference, online)
  expect_identical(online, offline)
})

test_that("Error", {

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

  offline <- run.offline(list(Esi1, Esi2, Esi3), blocks, code)

  si <- SI.event()
  reference <- list(
    out = DBcombine(
      DB.event(si, timeoption2ts(NULL, seconds(1.3)), 'B'),
      DB.event(si, timeoption2ts(NULL, seconds(2.1)), 'C'),
      DB.event(si, timeoption2ts(NULL, seconds(2.3)), 'A')
    )
  )
  expect_identical(reference, offline);
  
  expect_error(run.online(list(Esi1, Esi2, Esi3), blocks, code))
})

test_that("Error suppressed", {
  
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
  createOutput(cross.merge.events(input(1), input(2), input(3), doTimeCheck=FALSE), 'out')
  }
  ";
  
  offline <- run.offline(list(Esi1, Esi2, Esi3), blocks, code)
  
  si <- SI.event()
  reference <- list(
    out = DBcombine(
      DB.event(si, timeoption2ts(NULL, seconds(1.3)), 'B'),
      DB.event(si, timeoption2ts(NULL, seconds(2.1)), 'C'),
      DB.event(si, timeoption2ts(NULL, seconds(2.3)), 'A')
    )
  )
  expect_identical(reference, offline);
  
  
  online <- run.online(list(Esi1, Esi2, Esi3), blocks, code)
  si <- SI.event()
  reference <- list(
    out = DBcombine(
      DB.event(si, timeoption2ts(NULL, seconds(2.3)), 'A'),
      DB.event(si, timeoption2ts(NULL, seconds(1.3)), 'B'),
      DB.event(si, timeoption2ts(NULL, seconds(2.1)), 'C')
    )
  )
  expect_identical(reference, online);
})
