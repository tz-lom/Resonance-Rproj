pipe.downsample = function(input, dec){
  processor(
    input,
    prepare = function(env){
      SI.is.channels(input) || stop('Must be channels in input')
      
      env$count <- 0
      
      SI.channels(channels = SI(input)$channels, samplingRate = SI(input)$samplingRate/dec)
    },
    online = function(db){
      n <- nrow(db)
      take <- which((1:n - count) %% dec == 0)
      
      count <<- count+n
      
      ret <- db[take,, drop=FALSE]
      attr(ret, 'TS') <- attr(db,'TS')[take]
      ret
    }
  )
}