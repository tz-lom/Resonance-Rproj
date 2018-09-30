onDataBlock <- function(block) {
  to_restore <- .execution_plan$processingPlanId
  execId <- findInExectionPlan(SI(block))
  if(length(execId)>0){
    .execution_plan$processingPlanId <- execId
    target <- .execution_plan$plan[[execId]]
    
    argList <- lapply(target$inputs, function(si){
      if(identical(si, SI(block)))
        block
      else
        makeEmpty(si)
    })
    
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
}
