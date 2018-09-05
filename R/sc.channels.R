sc.channels <- function(input, maxDeviance=10){
  processor(
    input,
    prepare=function(env){
      if(!SI.is.channels(input)) return("input must be 'channels' type");
      
      env$firstTime <- NA
      env$step <- 1E9/SI(input)$samplingRate

      SI.channels(SI(input)$channels, SI(input)$samplingRate)
    },
    online=function(input){
      if(is.na(firstTime)){
        ts <- TS(input)
        firstTime <<- ts[[1]]
      }
      ts <- nanotime(seq(from=firstTime, by=step, length.out=nrow(input)+1))
      TS(input) <- ts[-length(ts)]
      firstTime <<- ts[[length(ts)]]
      input
    }
  )
}
