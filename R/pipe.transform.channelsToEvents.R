pipe.transform.channelsToEvents <- function(input, transform){
  processor(
    input,
    prepare = function(env){
      SI.is.channels(input) || stop("Input must be channels")
      SI.event()
    },
    online = function(input){
      if(nrow(input)>0) {
        ind <- 1
        apply(input,1, function(x){
          y <- transform(x)
          attr(y, 'TS') <- attr(input, 'TS')[[ind]]
          ind <<- ind+1
          y
        })
      } else {
        list()
      }
    }
  )
}