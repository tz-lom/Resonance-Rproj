#' Extracts powers of frequencies in specified channels
#'
#' @param input Input pipe
#' @param choose DataFrame with columns 'channel' and 'frequency' 
#' @param normalize If enabled returns percentage of power instead of absolute value
#' @return Row where each element corresponds to row from choose and equals to power of channel
pipe.FFTFilter <- function(input, choose, normalize=F) {

  # @todo: it is poor style to do such things like FFT extraction, it is better to create separate procedure
  
  processor(
    input,
    prepare=function(env){
      if(!SI.is.window(input)) return('Input must have type Window');
      
      env$channels <- unique(choose$channel)
      env$window <- SI(input)$samples
      env$freq <- c()
      
      # @todo: fix window-base frequency relation
      for(i in 1:nrow(choose)){
        col <- which(env$channels==choose[[i,'channel']])
        fq <- choose[[i, 'frequency']]
        
        stopifnot(fq<env$window/2)
                
        env$freq <- c(env$freq, (col-1)*env$window+fq+1)
      }
      
      env$prealloc <- matrix(0.0, ncol=length(env$channels), nrow=env$window)
      
      SI.channels(channels = length(env$freq), samplingRate = SI(input)$samplingRate)
    },
    online=function(data){
      ret <- matrix(ncol = length(freq), nrow=length(data))
      for(i in 1:length(data)){
        copyColumns(prealloc, data[[i]], channels)
        spectre <- mvfft(prealloc)
        if(normalize){
          ret[i,] <- Mod(spectre[freq])/window/sum(Mod(spectre))
        } else {
          ret[i,] <- Mod(spectre[freq])/window
        }
      }
      #extract times from ret
      attr(ret, 'TS') <- sapply(data, function(d){
        ts <- attr(d, 'TS')
        ts[[length(ts)]]
      })
      ret
    }
  )
}