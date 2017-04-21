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
      
      env$totalChannels <- totalChannels
      env$buffer <- matrix(.0, ncol=totalChannels, nrow=sr*length(channels))
      env$TS <- vector('double', sr*length(channels))
      env$rowCounts <- integer(length(channels))
      env$colCounts <- sapply(0:(length(ch)-1), function(n) sum(ch[0:n]))
      
      env$si <- SI.channels(channels = totalChannels, samplingRate = sr)
      env$si
    },
    online = function(...){
      streams <- list(...)
      
      maxSize <- max(sapply(streams, nrow)) + max(rowCounts)
      
      if(nrow(buffer)< maxSize){
        bf <- matrix(.0, ncol=totalChannels, nrow=maxSize)
        bf[1:nrow(buffer),] <- buffer
        buffer <<- bf
        
        tmp <- double(maxSize)
        tmp[1:length(TS)] <- TS
        TS <<- tmp
      }
      
      for(i in 1:length(streams)){
        if(nrow(streams[[i]])>0){
          if(i==1){
            TS[rowCounts[[i]] + (1:nrow(streams[[i]])) ] <- attr(streams[[i]], 'TS')
          }
          replace_columns_block(buffer, rowCounts[[i]], colCounts[[i]], streams[[i]])
          rowCounts[[i]] <<- rowCounts[[i]]+nrow(streams[[i]])
        }
      }
      ready <- min(rowCounts)
      if(ready > 0){
        # we have some buffer to send
        ret <- forceCopy(buffer[1:ready,,drop=F])
        attr(ret, 'TS') <- TS[1:ready]
        
        shiftRows(buffer, ready)
        if(ready<nrow(buffer)){
          TS[1:(nrow(buffer)-ready)] <<- TS[(ready+1):nrow(buffer)]
        }
        
        rowCounts <<- rowCounts-ready
        
        return(ret);
      } else {
        return(makeEmpty(si));
      }
    }
  )
}