test_that("cross.windowizeByEvents", {
  load('windowize.Rdata')
  
  
  code <- "
process <- function(){
  createOutput(cross.windowizeByEvents(input(1), input(2), 500, 75), 'out')
}
  "
  

  A <- run.online(list(Csi, Esi), blocks, code)
  B <- run.offline(list(Csi, Esi), blocks, code)
  
#  all.equal(A$out,B$out, check.attributes = F)
   
  expect_equal(A$out, B$out, check.attributes = F)
})