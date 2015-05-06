#'
#'
source.channeledEvents <- function(evData, samplingRate){
  # split evData to event blocks
  
  # find edges
  ev <- diff(evData)
  pos <- which(ev!=0)
  
  blocks <- lapply(pos, function(t){
    ret <- evData[[t+1]]
    attr(ret, 'TS') <- (t+1)*1E6/samplingRate
    ret
  })
  
  SI(blocks) <- SI.event()
  
  blocks
}
