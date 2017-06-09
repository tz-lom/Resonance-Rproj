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
        evs <<- c(evs, 
                  lapply(events, function(e){
                    r <- list(type = all(e))
                    if(r$type){
                      r$time <- attr(e, 'TS')+shiftT
                    } else {
                      r$time <- attr(e, 'TS')+shiftF
                    }
                    r
                  })
        )
        
        # normalize event sequence: must be T F T F T ...
        filt <- c(evs[[1]]$type,
                  diff(sapply(evs, `[[`, 'type'))!=0)
        evs <<- evs[filt]
      }
      
      
      while( length(evs) && length(si.times) &&
             evs[[1]]$time < si.times[[length(si.times)]]
      )
      {
        current <- which(si.times >= evs[[1]]$time)[[1]]
        
        if(evs[[1]]$type)
        {
          if(index)
          {
            ins <- signal[index:(current-1), ]
            attr(ins, 'TS') <- si.times[index:(current-1)]
            res <- c(res, list(ins))
          }
          index <<- current
        }
        else
        {
          if(index)
          {
            ins <- signal[index:(current-1), ]
            attr(ins, 'TS') <- si.times[index:(current-1)]
            res <- c(res, list(ins))
            index <<- 0L
          }
        }
        evs <<- evs[-1]
      }
      
      res
    }
  )
}