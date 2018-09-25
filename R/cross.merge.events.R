cross.merge.events <- function(...){
  processor(
    ... ,
    prepare = function(env){
      all(sapply(list(...), function(x){
        SI.is.event(x)
      })) || stop("Every stream must be event")
      
      env$other
      
      SI.event()
    },
    online = function(...){
      
    },
    offline = function(...){
      # merge all of them into one list
      all <- c(...)
      TS <- sapply(all, attr, 'TS')
      
      # return ordered vector of events
      all[order(TS)]
    }
  )
}
