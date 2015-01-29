cross.epochByEvent <- function(input, events, shiftT=0, shiftF=0){
  signal <- matrix(0.0, nrow = 2^5, ncol = input$channels)
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
      
      si.times <<- append(si.times, attr(si, 'timestamps'))
    }
    
    if(!is.null(events))
    {
      evs <<- c(evs, 
                lapply(events, function(e){
                  if(e$type){
                    e$time <- e$time+shiftT
                  } else {
                    e$time <- e$time+shiftF
                  }
                  e
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
      attr(res, 'timestamps') <- timestamps
    }
    
    res
    
  }
  
  bp <- block.processor(input)
  
  bpcb <- function(d,e){
    out <- epoch.by.event(d,e)
    if(!is.null(out))
    {
      attr(out, 'timestamp') <- attr(out, 'timestamps')[[1]]
      attr(out, 'timestamps') <- NULL
      bp$emit(out)
    }
  }
  
  input$connect(function(db){
    attr(db, 'timestamps') <- attr(db, 'timestamp')+ seq(from = 0, length.out = nrow(db), by = 1E9/input$samplingRate)
    bpcb(db, NULL)
  })
  
  events$connect(function(db){
    ev <- list(list(type=db, time=attr(db, 'timestamp')))
    bpcb(NULL, ev)
  })
  
  bp
}