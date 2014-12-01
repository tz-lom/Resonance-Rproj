#' Filters data stream cutting marked blocks
#'
#' @param data data stream
#' @param events event stream
#' @param backBuffer optional size of backtracking buffer, in DataBlock s
cross.filterByTimestamps <- function(data, events, backBuffer=100){
  # @todo: maybe mark that sampling is broken
  bp <- block.processor(data)
  
  lastTS <- 0

  buffer <- replicate(backBuffer, NULL, simplify = F)
  
  data$connect(function(db){
    if(attr(db, 'timestamp') < lastTS){
      bp$emit(db)
    }
    
    buffer[1:(backBuffer-1)] <<- buffer[2:backBuffer]
    buffer[[backBuffer]] <<- db
    
  })
  
  events$connect(function(db){
    if(db==F){
      lastTS <<- attr(db, 'timestamp')
    }else{
      #maybe already readed
      lastTS <<- lim.integer64()[[2]]
      
      time <- attr(db, 'timestamp')
      
      for(i in 1:backBuffer){
        ts <- attr(buffer[[i]], 'timestamp')
        if(!is.null(ts) && ts>= time) bp$emit(buffer[[i]])
      }
    }
  })
  
  bp
}