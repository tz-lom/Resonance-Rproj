source.channels <- function(data, samplingRate, blockSize, timestamps=NULL){
  data <- as.matrix(data)
  if(blockSize==-1) blockSize <- nrow(data)
  
  bp <- block.processor('channels', samplingRate=samplingRate, channels=ncol(data))
  
  if(is.null(timestamps)){
    timestamps <- seq(as.integer64(1E9/samplingRate)*blockSize, by=1E9/samplingRate*blockSize, length.out=ceiling(nrow(data)/blockSize))
  }
  
  J <- 0
  
  block <- matrix(data[[1]], ncol=ncol(data), nrow=blockSize)
  
  bp$nextBlock <- function(){
    J <<- J+1
    
    if(J*blockSize < nrow(data)){
      push_slice_rows_back(block, data, (J-1)*blockSize, blockSize)
      return(DataBlock(
        block,
        timestamps[[J]]
      ))
    }else if((J-1)*blockSize < nrow(data)){
      return(DataBlock(
        data[ ((J-1)*blockSize+1):nrow(data) ,, drop=F],
        timestamps[[J]]
      ))
    }else{
      return(NULL)
    }
  }

  
  bp
}

source.dataBlocks <- function(blockList, ...){
  bp <- block.processor(...)
  
  J <- 0
  
  bp$nextBlock <- function(){
    J <<- J+1
    if(J>length(blockList)) return(NULL)
    blockList[[J]]
  }
  
  bp
}

source.channeledEvents <- function(evData, samplingRate, blockSize){
  # split evData to event blocks
  
  # find edges
  ev <- diff(evData)
  pos <- which(ev!=0)
  
  blocks <- lapply(pos, function(t){
    DataBlock(
      evData[[t+1]],
      as.integer64(t+1)*1E9/samplingRate
    )
  })
  
  # returning sender
  source.dataBlocks(blocks, 'events')
}

pump <- function(src){
  db <- src$nextBlock()
  while(!is.null(db)){
    src$emit(db)
    db <- src$nextBlock()
  }
}

#' Pumps multiple sources simultaineously
pumpTogether <- function(...){
  sources <- list(...)
  
  times <- rep(as.integer64(0), length(sources))
  
  while(!all(is.na(times))){
    i <- which(times==min(times, na.rm = T))[[1]]
    db <- sources[[i]]$nextBlock()
    if(is.null(db)){
      times[[i]] <- NaN
    }else{
      times[[i]] <- attr(db, 'timestamp')
      sources[[i]]$emit(db)
    }
  }
  
}