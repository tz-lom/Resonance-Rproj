
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