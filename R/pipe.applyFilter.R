#' Applyes filter to channels signal
#'
pipe.applyFilter <- function(input, filt){  
  
  processor(
    input,
    prepare = function(env){
      if(!(inherits(filt,'Arma') || inherits(filt, 'Ma'))) return('Only Arma and Ma filters accepted');
      if(!SI.is.channels(input)) return('Must be channels in input');
      
      if(inherits(filt,'Arma')){
        flen <- length(filt$b)-1
      }else{
        flen <- length(unclass(filt))-1
      }
      
      env$init.x <- matrix(0.0, ncol=SI(input)$channels, nrow=flen)
      env$init.y <- matrix(0.0, ncol=SI(input)$channels, nrow=flen)
      
      env$flen <- flen
      
      SI(input)
    },
    online = function(input){
      result = matrix(0, ncol = ncol(input), nrow = nrow(input))
      for(i in 1:ncol(input)){
        result[,i] <- filter(filt, input[,i], init.x=init.x[,i], init.y=init.y[,i])
      }
      
      if(nrow(input)<flen){
        push_slice_rows_back(init.x, input, 0, nrow(input))
        push_slice_rows_back(init.y, result, 0, nrow(result))
      }else{
        push_slice_rows_back(init.x, input, nrow(input)-flen, flen)
        push_slice_rows_back(init.y, result, nrow(result)-flen, flen)
      }
      
      attr(result, 'TS') <- attr(input, 'TS')
      
      return(result);
    },
    offline = function(input){
      result <- matrix(0, ncol=ncol(input), nrow=nrow(input))
      
      for(i in 1:ncol(input)){
        result[,i] <- filter(filt, input[,i])
      }

      attr(result, 'TS') <- attr(input, 'TS')
      
      return(result);
    }
    )
}