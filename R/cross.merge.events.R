#' Combines several event streams to one
#' Take a note that no syncronisation is performed, if evens has different time source there could be runtime/logick error.
#' 
#' @param ... Event streams
#' @param doTimeCheck Check resulting timestamps.
#' @return Event stream
cross.merge.events <- function(..., doTimeCheck=TRUE){
  processor(
    ... ,
    prepare = function(env){
      all(sapply(list(...), function(x){
        SI.is.event(x)
      })) || stop("Every stream must be event")
      env$lastTS <- nanotime(bit64::lim.integer64()[[1]])
      SI.event()
    },
    online = function(...){
      # merge all of them into one list
      all <- c(...)
      if(length(all)==0) return(all)
      TS <- do.call(c, lapply(all, TS))
      
      tsSorted <- order(TS)
      
      if(doTimeCheck){
        if(tsSorted[[1]] < lastTS){
          stop("Merge of input events produced incorrect event stream")
        }
        lastTS <<- TS[[ tsSorted[[length(tsSorted)]] ]]
      }
      
      # return ordered vector of events
      all[tsSorted]
    }
  )
}
