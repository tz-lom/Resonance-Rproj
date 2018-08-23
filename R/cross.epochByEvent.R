#' Filters data stream cutting marked blocks
#'
#' @param data data stream
#' @param events event stream
cross.epochByEvent <- function(data, events, shiftT=0, shiftF=0){
  processor(
    data, events,
    
    prepare = function(env){
      SI.is.channels(data) || stop("Input must be channels")
      SI.is.event(events) || stop("events must be event")
      
      
      env$signal <- matrix(0.0, nrow = 2^5, ncol = SI(data)$channels)
      env$pointer <- 0L
      env$si.times <- c()
      env$evs <- list()
      env$index <- 0L
      
      SI.epoch(SI(data)$channels, SI(data)$samplingRate)
    },
    online = function(data, events){
      res <- list()
      #assign temporary signals and events
      if(nrow(data)>0)
      {
        if(nrow(data)+pointer >= nrow(signal))
        {
          tmp <- matrix(0.0, ncol=ncol(data), nrow=(nrow(signal)+nrow(data))*1.5)
          Resonance:::rowsCopy(tmp,0, signal, 0, -1)
          signal <<- tmp
        }
        
        Resonance:::rowsCopy(signal, pointer, data, 0, -1)
        pointer <<- pointer+nrow(data)
        
        si.times <<- append(si.times, attr(data, 'TS'))
      }
      
      if(length(events))
      {
        newEvs <- lapply(
          events, 
          function(e){
            r <- list(type = all(e))
            r$time <- attr(e, 'TS')+(if(r$type) shiftT else shiftF)
            r
          })
        
        # we want to remove repetitions of the evens
        # need to take to account if the last event if buffer T or F
        newEvs <- newEvs[diff(c(
          length(evs) %% 2==1, # last event in the buffer
          sapply(newEvs, `[[`, 'type')
        )) != 0 ]
        
        evs <<- c(evs, sapply(newEvs, `[[`, 'time'))
      }
      
      while( length(evs)>1 && length(si.times))
      {
        begin <- findInterval(evs[[1]], si.times)
        end <- findInterval(evs[[2]], si.times)

        if((begin>0) && (end < length(si.times)))
        {
          range <- (begin+1):end
          ins <- signal[range,, drop=F]
          attr(ins, 'TS') <- si.times[range]
          res <- c(res, list(ins))
          
          evs <<- evs[-(1:2)]
        }
      }
      
      res
    }
  )
}