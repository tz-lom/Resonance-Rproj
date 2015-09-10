#' Merge several similar channels in one
#' 
cross.mergeChannels <- function(...){
  channels <- list(...)
  processor(
    ...,
    prepare = function(env){
      all(sapply(channels, function(x){
        SI.is.channels(x) && SI(x)$samplingRate==SI(channels[[1]])$samplingRate
      })) || stop("Every stream must be channels with equal samplingRate")
      
      ch <- sapply(channels, function(x) SI(x)$channels)
      totalChannels <- sum(ch)
      sr <- SI(channels[[1]])$samplingRate
      
      env$buffer <- matrix(.0, ncol=totalChannels, nrow=sr*length(channels))
      env$rowCounts <- integer(length(channels))
      env$colCounts <- sapply(0:(length(ch)-1), function(n) sum(ch[0:n]))
      
      SI.channels(channels = totalChannels, samplingRate = sr)
    },
    online = function(...){
      streams <- list(...)
      
      for(i in 1:length(streams)){
        if(!is.null(streams[[i]])){
          replace_columns_block(buffer, rowCounts[[i]], colCounts[[i]], streams[[i]])
          rowCounts[[i]] <<- rowCounts[[i]]+nrow(streams[[i]])
        }
      }
      ready <- min(rowCounts)
      if(ready > 0){
        # we have some buffer to send
        ret <- forceCopy(buffer[1:ready,])
        shiftRows(buffer, ready)
        rowCounts[] <<- rowCounts-ready
        return(ret);
      } else {
        return(NULL);
      }
    }
  )
}