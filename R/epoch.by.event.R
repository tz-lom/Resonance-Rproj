signal <- matrix(0.0, nrow = 2^5, ncol = 62)
pointer <- 0L
si.times <- c()
evs <- list()
index <- integer64(1)

epoch.by.event <- function(si = NULL, events = NULL)
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
    
    si.times <<- append(si.times, attr(si, 'time'))
  }
  
  if(!is.null(events))
  {
    evs <<- c(evs, events)
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
        res <- rbind(res, signal[index:current, ])
        timestamps <- append(timestamps, si.times[index:current])
      }
      index <<- current
    }
    else
    {
      if(index)
      {
        res <- rbind(res, signal[index:current, ])
        timestamps <- append(timestamps, si.times[index:current])
        index <<- integer64(1)
      }
    }
    evs <<- evs[-1]
  }
  
  if(!is.null(res))
  {
    attr(res, 'time') <- timestamps
  }
  
  res
  
}