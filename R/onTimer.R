onTimer <- function(id, time){
  id <- which(.execution_plan$timers$id==id)
  if(length(id)==0) {
    warning("Unknown timer was called")
    return();
  }
  timer <- .execution_plan$timers[id, ]
  if(timer$singleShot){
    .execution_plan$timers <- .execution_plan$timers[-id, ]
  }
  
  to_restore <- .execution_plan$processingPlanId
  .execution_plan$processingPlanId <- timer$planId
  target <- .execution_plan$plan[[timer$planId]]
  
  argList <- lapply(target$inputs, makeEmpty)
  
  add <- list()
  add[[timer$argName]] <- timer$data
  TS(add[[timer$argName]]) <- time
  
  argList <- c(argList, add)
  
  result <- do.call(target$online, argList)
  
  if(class(result)=='multipleStreams'){
    mapply(function(data, si){
      SI(data) <- si
      onDataBlock(data)
    }, result, target$outputs)
  } else {
    SI(result) <- target$outputs[[1]]
    onDataBlock(result)
  }
  .execution_plan$processingPlanId <- to_restore
}


startTimer <- function(timeout, data=TRUE, call='onTimeout', singleShot=TRUE){
  if(.execution_plan$processingPlanId != 0 ){
    timerId <- .execution_plan$nextTimerId
    .execution_plan$nextTimerId <- .execution_plan$nextTimerId + 1
    .execution_plan$timers <- rbind(
      .execution_plan$timers, 
      data.frame(
        id=timerId,
        planId=.execution_plan$processingPlanId,
        argName=call,
        data=data,
        singleShot=singleShot,
        stringsAsFactors = FALSE
      ))
    do.call(addToQueue, list("startTimer", id=timerId, timeout=timeout, singleShot=singleShot))
    timerId
  } else {
    stop("Can't use startTimer function without run.online context")
  }
}

stopTimer <- function(timerId){
  .execution_plan$timers <- .execution_plan$timers[.execution_plan$timers$id != timerId,]
  addToQueue("stopTimer", id=timerId)
}
