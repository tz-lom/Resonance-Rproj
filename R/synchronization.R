#' Perform synchronization of two streams using RTC timestamps
#' 
#' Simple synchronization, recommended for synchronization streams generated (not recorded) on same machine.
#' In other cases can lead to constant desynchronization.
#' 
#' @param inputA First input
#' @param inputB Second input
#' @return List with pipes 
# @todo: rename cross to proper name
cross.syncByTimestamp <- function(inputA, inputB, delayBuffer){
  bpA <- block.processor(inputA)
  bpB <- block.processor(inputB)
  
  
  
  inputA$connect(function(db){
    attr(db, 'timestamp')
  })
  
  inputB$connect(function(db){
    
  })
  
  bp
}
