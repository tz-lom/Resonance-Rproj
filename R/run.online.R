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
    SI(x) <- inputs[sapply(old_inputs, identical, SI(x))][[1]]
    x
  })
  
  sis <- list()
  datas <- list()
  siNames <- list()
  timers <- data.frame(id=integer(), time=bit64::integer64(), timeout=numeric(), singleShot=logical())
  currentTime <- nanotime(0)
  
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
      if(x$cmd == 'startTimer'){
        timers <<- rbind(
          timers,
          data.frame(
            id = x$args$id,
            time = currentTime + x$args$timeout*1E6,
            timeout = x$args$timeout*1E6,
            singleShot = x$args$singleShot
          ))
      }
    })
  }
  
  # actual execution
  
  onPrepare(inputs, code)
  processQueue()
  
  onStart()
  processQueue()
  
  lapply(blocks, function(x){
    currentTime <<- lastTS(x)
    # maybe some timers will trigger before this data block
    if(nrow(timers)>0){
      toProcess <- which(timers$time<currentTime)
      toProcess <- toProcess[order(timers$time[toProcess])]
      for(i in toProcess){
        timer <- timers[i, ]
        currentTime <<- timer$time
        onTimer(timer$id, timer$time)
        processQueue()
        if(!timer$singleShot){
          timers[i, 'time'] <- currentTime + timer$timeout
        }
      }
      currentTime <<- lastTS(x)
      toRemove <- which(timers$time<currentTime)
      if(length(toRemove)>0){
        timers <<- timers[-toRemove, ]
      }
    }
    
    onDataBlock(x)
    processQueue()
  })
  
  # process timers
  toProcess <- order(timers$time)
  for(i in toProcess){
    timer <- timers[i, ]
    currentTime <<- timer$time
    onTimer(timer$id, timer$time)
    processQueue()
  }

  onStop()
  processQueue()
  
  # finished
  
  
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
