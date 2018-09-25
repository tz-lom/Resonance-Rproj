# #test_that("Dynamic change of parameters", {
#   
#   Csi <- SI.channels(1, 100)
#   Esi <- SI.event()
#   
#   blocks <- list(
#     DB.channels(Csi, 3, rep(1, 100)),
#     DB.channels(Csi, 3+1E6, rep(1, 100)),
#     DB.event(Esi, 2+1E6, '{"p": 4}'),
#     DB.channels(Csi, 3+2E6, rep(1, 100))
#   )
#   
#   code <- "
#   
#   process = function(){
#   
#   inp <- input(1)
#   cmd <- input(2)
#   
#   p <- split.pid.ctrl(inp, cmd)
#   
#   createOutput(p$ctrl, 'result')
#   createOutput(p$logs, 'logs')
#   
#   }
#   "
#   
#   A <- run.online(list(Csi, Esi), blocks, code)
# #  B <- run.offline(list(Csi, Esi), blocks, code)
#   
#   
# #})
