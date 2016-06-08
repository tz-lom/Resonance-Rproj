run.online <- function(inputs, blocks, code){
  
  require(Resonate)
  
  #empty <- lapply(inputs, makeEmpty)
  
  inputs <- lapply(inputs, function(x){
    x$online <- T
    x
  })
  
  onPrepare(inputs, code)
  
  
  nextBlock <- function(x){
    do.call(paste("nextBlock", class(x)[[1]], sep="."), list(x))
  }
  
  nextBlock.DB.channels <- function(b){
    ts <- attr(b, 'TS')
    onDataBlock.double(id = 1, vector = t(b), samples = nrow(b), timestamp = ts[[length(ts)]])
  }
  
  nextBlock.DB.event <- function(b){
    onDataBlock.message(id = 2, msg = as.character(b), timestamp = attr(b, 'TS'))
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
      
      datas[[siNames[[x$args$id]]]] <<- c(datas[[siNames[[x$args$id]]]], list(DB.something(si, attr(x$args$data, 'TS'), data)))
    }
  })
  
  lapply(datas, function(bl) do.call(merge, bl))
}