#' Split data stream to windows using event stream
#' 
#' @param data Data stream
#' @param events Events stream
#' @param windowSize Size of resulting window
#' @param backBuffer Size of buffer for data, may be increased in case of big delay in events arrival
#' @return pipe with type=window
n.cross.windowizeByEvents <- function(data, events, windowSize, backBuffer=10000, shift=0){
  bp <- block.processor(data, type="window", size=windowSize, channels=data$channels)
  
  backBuffer <- max(backBuffer, (windowSize+abs(shift))*2)
  
  backBuffer <- matrix(NA, ncol=data$channels, nrow=backBuffer)
  
  windowSize <- ceiling(windowSize)
  
  lastTS <- NA
  lastSample <- 0
  grabSampleQueue <- c(Inf)
  windowizationNotice <- list(NaN)
  
  ifWindowReady <- function(){
    if(grabSampleQueue[[1]] <= lastSample){
      last <- nrow(backBuffer)-(lastSample-grabSampleQueue[[1]])
      block <- backBuffer[ (last-windowSize+1):last,, drop=F]
      ts <- lastTS - (lastSample-grabSampleQueue[[1]])*1E9/data$samplingRate
      
      attr(block, 'windowizationEvent') <- windowizationNotice[[1]]
      
      bp$emit(DataBlock(block, ts))
      
      grabSampleQueue <<- grabSampleQueue[2:length(grabSampleQueue)]
      windowizationNotice <<- windowizationNotice[2:length(windowizationNotice)]
      ifWindowReady()
    }
  }
  
  data$connect(function(db){
    # populate backBuffer
    data <- as.matrix(db)
    backBuffer[1:(nrow(backBuffer)-nrow(data)),] <<- backBuffer[(nrow(data)+1):nrow(backBuffer),, drop=F]
    backBuffer[(nrow(backBuffer)-nrow(data)+1):nrow(backBuffer), ] <<- data
    lastTS <<- attr(db, 'timestamp')
    lastSample <<- lastSample+nrow(data)
    ifWindowReady()
  })
  
  events$connect(function(db){
    time <- attr(db,'timestamp')
    # recalc time to samples
    
    gs <- lastSample + floor((time-lastTS)*data$samplingRate/1E9) + windowSize + shift
        
    IDX <-sort(c(grabSampleQueue, gs), index.return = T)
    
    grabSampleQueue <<- IDX$x
    windowizationNotice <<- c(windowizationNotice, as.character(db))[IDX$ix]
    
    ifWindowReady()
  })
  
  bp
}