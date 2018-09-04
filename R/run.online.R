run.online <- function(inputs, blocks, code){
  
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
    do.call(paste("nextBlock", SI(x)$type, sep="."), list(x))
  }
  
  nextBlock.channels <- function(b){
    ts <- TS(b)
    onDataBlock.channels(id = blockToId(b), b, timestamp = ts[[length(ts)]])
  }
  
  nextBlock.event <- function(b){
    onDataBlock.message(id = blockToId(b), msg = b[[1]], timestamp = TS(b[[1]]))
  }
  
  nextBlock.window <- function(b){
    onDataBlock.window(id = blockToId(b), vector = b, timestamp=TS(b))
  }
  
  nextBlock.epoch <- function(b){
    onDataBlock.epoch(id=blockToId(b), vector=b, timestamp=TS(b))
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
      sis[[L$id]] <<- L[!(names(L) %in% c('id', 'name', 'online'))]
      siNames[[L$id]] <<- L$name
      datas[[L$name]] <<- list(makeEmpty(sis[[L$id]]))
    }
    if(x$cmd == 'sendBlockToStream'){
      si <- sis[[x$args$id]]
      
      data <- x$args$data
      
      datas[[siNames[[x$args$id]]]] <<- c(
        datas[[siNames[[x$args$id]]]], 
        list(
          DB.something(
            si,
            TS(x$args$data),
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
      } else {
        list()
      }
    })
  } else {
    list()
  }
}
