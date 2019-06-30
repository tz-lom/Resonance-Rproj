pipe.windowizer <- function(input, size, shift){
  
  processor(
    input,
    prepare = function(env){
      size>0 || stop("size must be greater than 0")
      shift>0 || stop("shift must be greater than 0")
      shift<=size || stop("can't shift more than to size elements")
      
      SI.is.channels(input) || stop("input must be Channels")
      
      env$window <- matrix(as.double(0), ncol=SI(input)$channels, nrow=size)
      env$TS <- double(SI(input)$channels);
      env$unfilled <- size
      env$stime <- 1E9/SI(input)$samplingRate
      
      SI.window(channels = SI(input)$channels, samples = size, samplingRate = SI(input)$samplingRate/shift)
    },
    online = function(db){
      add <- nrow(db)
      
      ret <- list()
      
      while(add>0){
        if(add>=unfilled){
          #push first unfilled
          push_slice_rows_back(window, db, nrow(db)-add, unfilled)
          
          #time <- attr(db, 'TS') - (add-unfilled)*1E9/input$samplingRate

          block <- window
          #attr(block, 'TS') <- attr(db, 'TS')[1:size + nrow(db)-add + unfilled - size]
          attr(block, 'TS') <- seq(to=attr(db, 'TS')[[nrow(db)-add + unfilled]], length.out = nrow(block), by = stime)
          ret <- c(ret, list(block))
          
          add <- add-unfilled
          unfilled <<- shift
          
        }else{
          push_slice_rows_back(window, db, nrow(db)-add, add)
          unfilled <<- unfilled-add
          add <- 0
        }
      }
      ret
    })
}
