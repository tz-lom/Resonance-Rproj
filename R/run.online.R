truncateClass <- function(block, si){
  class(block) <- class(block)[class(block)!=paste0("DB.",si$type)]
  block
}

run.online <- function(inputs, blocks, code){
  
  require(Resonate)
  
  #empty <- lapply(inputs, makeEmpty)
  
  inputs <- lapply(inputs, function(x){
    x$online <- T
    x
  })
  
  blocks <- lapply(blocks, function(x){
    SI(x)$online <- T
    x
  })
  
  onPrepare(inputs, code)
  
  blockToId <- function(b){
    which(sapply(inputs, identical, SI(b)))
  }
  
  
  nextBlock <- function(x){
    do.call(paste("nextBlock", class(x)[[1]], sep="."), list(x))
  }
  
  nextBlock.DB.channels <- function(b){
    ts <- attr(b, 'TS')
    onDataBlock.double(id = blockToId(b), vector = t(b), samples = nrow(b), timestamp = ts[[length(ts)]])
  }
  
  nextBlock.DB.event <- function(b){
    onDataBlock.message(id = blockToId(b), msg = as.character(b[[1]]), timestamp = attr(b[[1]], 'TS'))
  }
  
  nextBlock.DB.window <- function(b){
    onDataBlock.window(id = blockToId(b), vector = b, timestamp=attr(b, 'TS'))
  }
  
  nextBlock.DB.epoch <- function(b){
    onDataBlock.epoch(id=blockToId(b), vector=b, timestamp=attr(b, 'TS'))
  }
  
  nextBlock.default <- function(b){
    stop('Unknown block type')
  }
  
  
  lapply(blocks, nextBlock)
  
  Q <- popQueue()
  
  sis <- list()
  datas <- list()
  siNames <- list()
  
  lapply(Q, function(x){
    if(x$cmd == 'createOutputStream'){
      L <- x$args
      sis[[L$id]] <<- L[names(L)!='id' & names(L)!='name' & names(L)!='online']
      siNames[[L$id]] <<- L$name
      datas[[L$name]] <<- list()
    }
    if(x$cmd == 'sendBlockToStream'){
      si <- sis[[x$args$id]]
      
      data <- x$args$data
      
      if(is.matrix(data)) data <- as.vector(t(data))
      
      datas[[siNames[[x$args$id]]]] <<- c(
        datas[[siNames[[x$args$id]]]], 
        list(
          DB.something(
            si,
            attr(x$args$data, 'TS'),
            data
            )
          )
      )
    }
  })
  
  if(length(datas)>0){
    lapply(datas, function(bl) {
      if(length(bl)>0){
        merged <- do.call(DBcombine, bl)
        truncateClass(merged, SI(merged))
      } else {
        list()
      }
    })
  } else {
    list()
  }
}
