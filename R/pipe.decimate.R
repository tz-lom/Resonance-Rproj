
pipe.decimate <- function(input, inc, dec, coef){  
  processor(
    input,
    prepare = function(env){
      SI.is.channels(input) || stop('Must be channels in input')
      
      
      decs <- list()
      for(i in 1:SI(input)$channels)
        decs[[i]] <- upFirDown(inc, dec, coef)
      
      env$decs <- decs
      
      SI.channels(channels = SI(input)$channels, samplingRate = SI(input)$samplingRate*inc/dec)
    },
    online = function(db){
      res <- decs[[1]](db[,1])
      
      result <- matrix(nrow = length(res), ncol=ncol(db))
      result[,1] <- res
      
      if(ncol(db)>1) {
        for(i in 2:ncol(db)){
          result[,i] <- decs[[i]](db[,i])
        }
      } 
      
      attr(result, 'TS') <- attr(db, 'TS')[floor(seq(from=1, by=dec/inc, length.out = nrow(result)))]
      result
    }
  )
}
