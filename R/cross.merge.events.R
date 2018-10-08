cross.merge.events <- function(..., maxTimeDeviation=0.1){
  processor(
    ... ,
    prepare = function(env){
      all(sapply(list(...), function(x){
        SI.is.event(x)
      })) || stop("Every stream must be event")
      
      env$eventBuffer <- lapply(1:...length(), function(x){ list() })
      env$timeDeviation <- bit64::as.integer64(maxTimeDeviation*1E9)
      
      SI.event()
    },
    online = function(..., onTimeout=NULL){
      requestTimeout <- FALSE
      mapply(function(evt, id){
        if(length(evt)>0){
          eventBuffer[[id]] <<- c(eventBuffer[[id]], evt)
          requestTimeout <<- TRUE
        }
      }, list(...), 1:...length())
      
      ret <- list()
      
      times <- lapply(eventBuffer, function(x){ do.call(c, lapply(x, TS)) } )
      times <- c(list(TS(onTimeout)), times)
      times <- times[!sapply(times, is.null)]
      
      if(length(times)==0) return(list())
      
      times <- do.call(c, times)
      
      now <- max(times)
      
      for(i in seq_along(eventBuffer)){
        times <- do.call(c, lapply(eventBuffer[[i]], TS))
        if(length(times)>0){
          pass <- (times + timeDeviation) <= now 
          addRet <- lapply(eventBuffer[[i]][pass], function(x){
            TS(x) <- now
            x
          })
          ret <- c(ret, addRet) 
          eventBuffer[[i]] <<- eventBuffer[[i]][!pass]
        }
      }
      
      if(any(sapply(eventBuffer, length)>0))
      {
        startTimer(data=TRUE, timeout=maxTimeDeviation*1000)
      }
      
      if(length(ret)>0){
        ret[order(do.call(c, lapply(ret, TS)))]
      } else {
        ret
      }
    },
    offline = function(...){
      # merge all of them into one list
      all <- c(...)
      TS <- do.call(c, lapply(all, TS))
      
      # return ordered vector of events
      all[order(TS)]
    }
  )
}
