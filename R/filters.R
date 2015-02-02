pipe.spatial <- function(input, matrix){
  input$type=='channels' || stop('Input must be channels')
  
  bp <- block.processor(input, channels=nrow(matrix));
  
  transformation <- function(db){
    bp$emit(DataBlock(db %*% t(matrix), db))
  };
  
  input$connect(transformation)
  bp
}

#' Extracts powers of frequencies in specified channels
#'
#' @param input Input pipe
#' @param choose DataFrame with columns 'channel' and 'frequency' 
#' @return Row where each element corresponds to row from choose and equals to power of channel
pipe.FFTFilter <- function(input, choose){
  input$type=="window" || stop("FFT filter must receive window")
  
  
  # parce channelsAndFrequencies
  bp <- block.processor(input, type="channels", size=1)
  
  window <- input$size
  channels <- unique(choose$channel)
  freq <- c()
  # @todo: fix window-base frequency relation
  for(i in 1:nrow(choose)){
    col <- which(channels==choose[[i,'channel']])
    fq <- choose[[i, 'frequency']]
    
    stopifnot(fq<window/2)
    
    freq <- c(freq, (col-1)*window+fq+1)
  }
  
  bp$channels <- length(freq)
  
  prealloc <- matrix(0.0, ncol=length(channels), nrow=window)
    
  input$connect(function(db){
    copyColumns(prealloc, db, channels)
    
    spectre <- mvfft(prealloc)
    ret <- matrix(Mod(spectre[freq])/window, nrow=1)
    
    bp$emit(DataBlock(ret, db))
  })
  
  bp
}

#' Extracts spectrograms for windows
#'
#' @param input Input pipe
#' @param choose DataFrame with columns 'channel' and 'frequency' 
#' @return Row where each element corresponds to row from choose and equals to power of channel
pipe.spectrogram <- function(input){
  input$type=="window" || stop("FFT filter must receive window")
  
  # parce channelsAndFrequencies
  bp <- block.processor(input, type="window", size=input$size, channels=input$channels)
    
  input$connect(function(db){    
    spectre <- mvfft(db)
    bp$emit(DataBlock(Mod(spectre)/input$size, db))
  })
  
  bp
}

pipe.windowizer <- function(input, size, shift){
  size>0 || stop("size must be greater than 0")
  shift>0 || stop("shift must be greater than 0")
  shift<=size || stop("can't shift more than to size elements")
  
  input$type=="channels" || stop("There must be channels in input")
  
  bp <- block.processor(input, type="window", channels=input$channels, size=size, shift=shift, samplingRate=input$samplingRate*size/shift) #@todo: recalc frequency
  
  window <- matrix(as.double(0), ncol=input$channels, nrow=size)
  unfilled <- size
  
  input$connect(function(db){
    add <- nrow(db)
    
    while(add>0){
    
      if(add>=unfilled){
        #push first unfilled
        push_slice_rows_back(window, db, nrow(db)-add, unfilled)
        time <- attr(db, 'timestamp') - (add-unfilled)*1E9/input$samplingRate
        bp$emit(DataBlock(window, time))
        add <- add-unfilled
        unfilled <<- shift
        
      }else{
        push_slice_rows_back(window, db, nrow(db)-add, add)
        unfilled <<- unfilled-add
        add <- 0
      }
    }
  })
  
  bp
}

pipe.linearApproximation <- function(input, filters){
  input$type=="channels" || stop("There must be channels in input")
  
  bp <- block.processor(input, channels=length(filters))
  
  input$connect(function(db){
    data <- as.matrix(db)
    
    ret <- sapply(filters, function(filter){
      channel <- data[,filter[[1]]]            
      
      pairs <- simplify2array(filter[2:length(filter)])
      
      
      pairs <- cbind(
        
        as.matrix(c(-Inf, round(pairs[2,1]))),
        
        pairs,
        
        as.matrix(c(Inf, round(pairs[2,ncol(pairs)])))
      )
      
      interp1(pairs[1,], pairs[2, ],channel)
    })
    
    
    bp$emit(DataBlock(t(ret), db))
  })
  
  bp
}

pipe.applyFilter <- function(input, filt){  
  inherits(filt,'Arma') || inherits(filt, 'Ma') || stop('Only Arma and Ma filters accepted')
  input$type=='channels' || stop('Must be channels in input')
  
  bp <- block.processor(input)
  
  if(inherits(filt,'Arma')){
    flen <- length(filt$b)-1
  }else{
    flen <- length(unclass(filt))-1
  }
  
  init.x <- matrix(0.0, ncol=input$channels, nrow=flen)
  init.y <- matrix(0.0, ncol=input$channels, nrow=flen)
  
  input$connect(function(db){
    result = matrix(0, ncol = ncol(db), nrow = nrow(db))
    for(i in 1:ncol(db)){
      result[,i] <- filter(filt, db[,i], init.x=init.x[,i], init.y=init.y[,i])
    }
    
    if(nrow(db)<flen){
      push_slice_rows_back(init.x, db, 0, nrow(db))
      push_slice_rows_back(init.y, result, 0, nrow(result))
    }else{
      push_slice_rows_back(init.x, db, nrow(db)-flen, flen)
      push_slice_rows_back(init.y, result, nrow(result)-flen, flen)
    }
    
    bp$emit(DataBlock(result, db))
  })
  
  bp
}


pipe.decimate <- function(input, inc, dec, coef){  
  input$type=='channels' || stop('Must be channels in input')
  
  bp <- block.processor(input, samplingRate=input$samplingRate*inc/dec)
  
  decs <- list()
  
  for(i in 1:input$channels)
    decs[[i]] <- upFirDown(inc, dec, coef)
  
  input$connect(function(db){
    res <- decs[[1]](db[,1])
    
    result <- matrix(nrow = length(res), ncol=ncol(db))
    result[,1] <- res
    
    if(ncol(db)>1) {
      for(i in 2:ncol(db)){
        result[,i] <- decs[[i]](db[,i])
      }
    }
    
    bp$emit(DataBlock(result, db))
  })
  
  bp
}

to.channels <- function(input){
  if(input$type == 'channels') return(input)
  
  if(input$type == 'window'){
    bp <- block.processor(input, type="channels", channels=input$channels)
    input$connect(function(x) bp$emit(x))
    return(bp)
  }
  if(input$type == 'input'){
    bp <- block.processor(input, type="channels", channels=input$channels)
    input$connect(function(db) bp$emit(db))
    return(bp)
  }
}

pipe.rescale <- function(input, shift, mult){
  bp <- block.processor(input)
  
  input$connect(function(db){
    bp$emit((db-shift)*mult)
  })
  
  bp
}

encapsulateProcessors <- function(input, output){
  bp <- output
  bp$emit <- input$emit
  bp
}

simulationSource <- function(channels, frequency, block.length, ...){
  bp <- block.processor("channels", channels=channels, samplingRate=frequency, ...)
  
  out <- to.channels(
    pipe.windowizer(
      bp,
      size=block.length,
      shift = block.length
      )
    )
  
  return(encapsulateProcessors(bp, out))
}

pipe.references <- function(input, refs){
  input$type=='channels' || stop('Must be channels in input')
  
  bp <- block.processor(input)
  
  cout <- 1:input$channels
  bp$channels <- bp$channels - length(refs)
  cout <- cout[-refs]
  
  input$connect(function(db){
    rdata <- db[,refs, drop=F]
    out <- db[,cout, drop=F]
    
    rdata <- apply(rdata,1, mean)
    
    bp$emit(DataBlock(sweep(out,1,rdata), db))
  })
  
  bp
}

#' Simple filter pipe that applies high-pass, low-pass and notch filters
#'
#' @param input Pipe connected to
#' @param lowFreq Frequency of high-pass filter
#' @param highFreq Frequency of low-pass filter
#' @param notch Frequency for notch filter
#' @return Constructed pipe
pipe.bandFilter <- function(input, lowFreq, highFreq, notch, order=2){
  out <- input
  if(lowFreq){
    out <- pipe.applyFilter(
      out,
      butter(order, lowFreq/input$samplingRate*2, type = 'high')
    )
  }
  if(highFreq){
    out <- pipe.applyFilter(
      out,
      butter(order, highFreq/input$samplingRate*2, type='low')
    )
  }
  if(notch){
    out <- pipe.applyFilter(
      out,
      butter(order, c(notch-1, notch+1)/input$samplingRate*2, type = 'stop')
    )
  }
  
  out
}

#' Combine pipes to chain, returns it's output
#'
#' Allows nice descriptive specification of pipeline.
#' Iterates over arguments and constructs pipeline where output from \emph{n-1} filter directed to input of \emph{n} filter.
#'
#' @param ... List of pipes, I recommend to skip first argument for pipe
#' @param .env Environment for evaluating pipes, for experts only
#' @return Output of last pipe
#' 
#' @examples
#' pipeline(
#'  pipe.applyFilter(input, f1),
#'  pipe.applyFilter(, f2)
#' )
pipeline <- function(... , .env = parent.frame()){
  arguments <- eval(substitute(alist(...)))
  
  piped <- NULL
  
  piped <- eval(arguments[[1]], envir = .env) 
  arguments <- arguments[-1]
  
  for(filter in arguments){
    is.call(filter) || stop('only filters must be in pipeline')
    
    filter[[2]] <- piped
    
    piped <- eval(filter, envir = .env)
  }
  
  piped
}
