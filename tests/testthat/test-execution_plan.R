# context("execution plan")
# 
# plain_op <- function(inp){
#   processor(
#     inp,
#     prepare=function(env){
#       SI.event()
#     },
#     online=function(inp){
#       inp
#     }
#   )
# }
# 
# split_op <- function(inp){
#   processor(
#     inp,
#     prepare = function(env){
#       multipleStreams(SI.event(), SI.event())
#     },
#     online=function(inp){
#       multipleStreams(inp, inp)
#     }
#   )
# }
# 
# merge_op <- function(a, b){
#   processor(
#     a, b,
#     prepare = function(env){
#       SI.event()
#     },
#     online = function(a,b){
#       a
#     }
#   )
# }
# 
# test_that("linear", {
# 
#   Esi <- SI.event()
#   
#   blocks <- list()
#   
#   blocks <- list(
#     DB.event(Esi, timeoption2ts(NULL, seconds(3)), 'A'),
#     DB.event(Esi, timeoption2ts(NULL, seconds(4)), 'B'),
#     DB.event(Esi, timeoption2ts(NULL, seconds(5)), 'C')
#   )
# 
#   code = "
# process = function(){
#   a <- input(1)
#   b <- plain_op(a)
#   c <- split_op(b)
#   d <- plain_op(c[[1]])
#   createOutput(d, 'out1')
#   createOutput(c[[2]], 'out2')
# 
#   e <- merge_op(b, d)
#   createOutput(e, 'out3')
# }
#   "
# 
#   code = "
# process = function(){
#   a <- input(1)
#   b <- split_op(a)
#   c <- merge_op(b[[1]], b[[2]])
#   createOutput(c, 'out')
# }
#   "
# 
#   online <- run.online(list(Esi), blocks, code)
# 
# })
