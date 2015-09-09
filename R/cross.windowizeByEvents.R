#' @todo: not production ready
#' Split data stream to windows using event stream
#' 
#' @param data Data stream
#' @param events Events stream
#' @param windowSize Size of resulting window
#' @param backBuffer Size of buffer for data, may be increased in case of big delay in events arrival
#' @return pipe with type=window
n.cross.windowizeByEvents <- function(data, events, windowSize, shift=0){
  
  ifWindowReady <- function(){
    if(grabSampleQueue[[1]] <= lastSample){
      last <- nrow(backBuffer)-(lastSample-grabSampleQueue[[1]])
      block <- backBuffer[ (last-windowSize+1):last,, drop=F]
      ts <- lastTS - (lastSample-grabSampleQueue[[1]])*1E9/data$samplingRate
      
      bp$emit(DataBlock(block, ts))
      
      grabSampleQueue <<- grabSampleQueue[2:length(grabSampleQueue)]
      ifWindowReady()
    }
  }
  
  processor(
    data, events,
    prepare = function(env){
      SI.is.channels(data) || stop("Data stream must be Channels")
      
      env$signal <- matrix(0.0, ncol=SI(data)$channels, nrow=2^5)
      env$pointer <- 0L
      env$si.times <- c()
      env$lastTS <- NA
      env$lastSample <- 0
      env$grabSampleQueue <- c(Inf)      
      
      SI.window(channels = SI(data)$channels, samples = windowSize, samplingRate = SI(data)$samplingRate)
    },
    online = function(si, events){
      if(!is.null(data)){
        
        if(nrow(si)+pointer >= nrow(signal))
        {
          tmp <- matrix(0.0, ncol=ncol(si), nrow=(nrow(signal)+nrow(si))*1.5)
          Resonance:::rowsCopy(tmp,0, signal, 0, -1)
          signal <<- tmp
        }
        
        Resonance:::rowsCopy(signal, pointer, si, 0, -1)
        pointer <<- pointer+nrow(si)
        
        si.times <<- append(si.times, attr(si, 'TS'))
      }
      
      if(!is.null(events)){
        time <- sapply(events, attr,'TS')
        gs <- lastSample + floor((time-lastTS)*data$samplingRate/1E6) + windowSize + shift 
        grabSampleQueue <<- sort(c(grabSampleQueue, gs))
      }
      
      if(grabSampleQueue[[1]] <= lastSample){
        last <- nrow(backBuffer)-(lastSample-grabSampleQueue[[1]])
        block <- backBuffer[ (last-windowSize+1):last,, drop=F]
        ts <- lastTS - (lastSample-grabSampleQueue[[1]])*1E9/data$samplingRate
        
        bp$emit(DataBlock(block, ts))
        
        grabSampleQueue <<- grabSampleQueue[2:length(grabSampleQueue)]
        ifWindowReady()
      }
      
    })
}