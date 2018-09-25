run.online <- function(inputs, blocks, code){
  
  old_inputs <- inputs
  
  # @todo: check if all input id can be assigned properly
  
  inputs <- mapply(FUN=function(x, id){
    x$online <- T
    if(is.null(x$id) || x$id == -1){
      x$id = id
    }
    x
  }, inputs, seq_len(length(inputs)), SIMPLIFY = FALSE)
  
  blocks <- lapply(blocks, function(x){
    SI(x) <- inputs[[match(SI(x), old_inputs)]]
    x
  })
  
  onPrepare(inputs, code)
  
  blockToId <- function(b){
    which(sapply(inputs, identical, SI(b)))
  }
  
  sis <- list()
  datas <- list()
  siNames <- list()
  
  processQueue <- function(){
    Q <- popQueue()
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
  }
  
  lapply(blocks, function(x){
    onDataBlock(blockToId(x), x)
    processQueue()
  })
  
  Q <- popQueue()
  
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
