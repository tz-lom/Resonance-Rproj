#' Cut input by event marks, return 'channels' as result of concatenating
#'
cross.cutByEvent <- function(data, events, shiftT=0, shiftF=0){
  
  processor(
    data, events,
    
    prepare = function(env){
      SI.is.channels(data) || stop("Input must be channels")
      SI.is.event(events) || stop("events must be event")
      
  
      env$signal <- matrix(0.0, nrow = 2^5, ncol = SI(data)$channels)
      env$pointer <- 0L
      env$si.times <- c()
      env$evs <- list()
      env$index <- 0
      
      SI(data)
    },
    
    online =  function(si = NULL, events = NULL)
    {
      
      res <- NULL
      timestamps <- NULL
      #assign temporary signals and events
      if(!is.null(si))
      {
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
      
      if(!is.null(events))
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
            res <- rbind(res, signal[index:(current-1), ])
            timestamps <- append(timestamps, si.times[index:(current-1)])
          }
          index <<- current
        }
        else
        {
          if(index)
          {
            res <- rbind(res, signal[index:(current-1), ])
            timestamps <- append(timestamps, si.times[index:(current-1)])
            index <<- 0
          }
        }
        evs <<- evs[-1]
      }
      
      if(!is.null(res))
      {
        attr(res, 'TS') <- timestamps
      }
      
      res
      
    }
  )
}