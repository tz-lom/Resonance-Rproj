#' Applyes filter to channels signal
#'
pipe.applyFilter <- function(input, filt){  
  
  processor(
    input,
    prepare = function(env){
      (inherits(filt,'Arma')) || stop('Only Arma filters accepted');
      SI.is.channels(input) || stop('Must be channels in input');
      
      flen <- length(filt$b)-1
      
      
      env$init.x <- matrix(0.0, ncol=SI(input)$channels, nrow=flen)
      env$init.y <- matrix(0.0, ncol=SI(input)$channels, nrow=flen)
      env$flen <- flen
      
      env$stage1 <- filt$b/filt$a[1]
      env$stage2 <- -filt$a[-1]/filt$a[1]
      
      env$ind1 <- -seq_len(flen)
      env$ind2 <- -seq_along(env$stage2)
      
      env
      
      SI(input)
    },
    online = function(input){
      result = matrix(0, ncol = ncol(input), nrow = nrow(input))
      for(i in 1:ncol(input)){
        x <- .Call(stats:::C_cfilter, c(init.x[,i], input[,i]), stage1, 1L, FALSE)[ind1]
        result[,i] <- .Call(stats:::C_rfilter, x, stage2, c(init.y[,i], double(length(x))))[ind2]
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