#' @todo: not production ready
#' Split data stream to windows using event stream
#' 
#' @param data Data stream
#' @param events Events stream
#' @param windowSize Size of resulting window
#' @param backBuffer Size of buffer for data, may be increased in case of big delay in events arrival
#' @return pipe with type=window
cross.windowizeByEvents <- function(data, events, windowSize, shift=0){
  processor(
    data, events,
    prepare = function(env){
      SI.is.channels(data) || stop("Data stream must be Channels")
      
      env$signal <- matrix(0.0, ncol=SI(data)$channels, nrow=2^5)
      env$pointer <- 0L
      env$si.times <- c()
      env$lastTS <- NA
      env$lastSample <- 0
      env$grabSampleQueue <- c()      
      env$windowSelector <- 1:windowSize-1
      env$samplingRate <- SI(data)$samplingRate
      
      SI.window(channels = SI(data)$channels, samples = windowSize, samplingRate = SI(data)$samplingRate)
    },
    online = function(si, events){
      
      # combin signal
      if(nrow(si)>0){
          
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
      
      # combine events
      if(length(events)>0){
        time <- sapply(events, attr,'TS')
        grabSampleQueue <<- c(grabSampleQueue, time)
      }
      
      ret <- list()
      
      while(length(grabSampleQueue)>0){
        gs <- grabSampleQueue[[1]]
        
        # @todo: when which.max will be fixed use it 
        moar <- which(si.times >= gs)  
        
        if(length(moar)>0){
          
          pos <- moar[1] + shift
          
          if(pos < 1) {
            # early event, drop it
            grabSampleQueue <<- grabSampleQueue[-1]
            next
          }
          
          if(length(si.times) >= pos+windowSize){
            # get window and move on
            wnd <- signal[pos + windowSelector, , drop=F]
            attr(wnd, 'TS') <- si.times[pos + windowSelector]
            
            grabSampleQueue <<- grabSampleQueue[-1]
            
            ret <- c(ret, list(wnd))
            next
          }
          break  # wait until full window arrives
          
        }else{
          break # waiting for samples
        }
        
      }
      
      ret
      
    })
}