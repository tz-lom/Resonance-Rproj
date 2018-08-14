context("cross.epochByEvent")

# test_that("enly data", {
#   Csi <- SI.channels(2, 100)
#   Esi <- SI.event()
# 
#   blocks <- list(
#     DB.channels(Csi, 3+10*5E4, 26:75)
#   )
# 
#   code <- "
#     process <- function(){
#       createOutput(cross.epochByEvent(input(1), input(2)), 'out')
#     }
#   "
# 
#   online <- run.online(list(Csi, Esi), blocks, code)
#   offline <- run.offline(list(Csi, Esi), blocks, code)
#   
#   epsi <- SI.epoch(2, 100)
#   reference <- list(out=makeEmpty(epsi))
# 
#   expect_equal(online, offline)
#   expect_equal(online, reference)
# })

# test_that("test1", {
#   Csi <- SI.channels(2, 100)
#   Esi <- SI.event()
# 
#   blocks <- list(
#     DB.channels(Csi, 3, 1:2),
#     DB.channels(Csi, 3+10*5E4, 26:75),
#     DB.event(Esi, 30 , TRUE),
#     DB.event(Esi, 3E4 , FALSE)
#   )
# 
# 
#   code <- "
#     process <- function(){
#       createOutput(cross.epochByEvent(input(1), input(2)), 'out')
#     }
#   "
# 
#   online <- run.online(list(Csi, Esi), blocks, code)
#   offline <- run.offline(list(Csi, Esi), blocks, code)
# 
#   expect_equal(online, offline)
# })
