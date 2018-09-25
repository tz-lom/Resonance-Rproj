.execution_plan <- new.env()

.reset_execution_plan <- function(){
  rm(list = ls(.execution_plan), envir = .execution_plan)
  
  .execution_plan$queue <- list()
  .execution_plan$plan <- list()
  .execution_plan$nextOutputId <- 1
  .execution_plan$nextStreamId <- 1

  .execution_plan$inputsData <- list()
  .execution_plan$env <- new.env(parent=globalenv())
  .execution_plan$env$input <- function(id){
    .execution_plan$inputsData[[id]]
  }
}


addToQueue = function(cmd, ...){
  .execution_plan$queue <- append(
    .execution_plan$queue,
    list(
      list(
        cmd=cmd,
        args = list(...)
      )
    )
  )
}

popQueue <- function(){
  ret <- .execution_plan$queue
  .execution_plan$queue <- list()
  ret
}

findInExectionPlan <- function(si){
  which(sapply(.execution_plan$plan, function(x){
    any(sapply(x$inputs, function(y) isTRUE(all.equal(si, y))))
  }))
}

renderExecutionPlanAsGraph <- function(){
  
  Ninputs <- length(.execution_plan$inputsData)
  
  nodes <- DiagrammeR::create_node_df(
    n = Ninputs + length(.execution_plan$plan),
    label = c(
      sapply(.execution_plan$inputsData, function(x){ paste("Input", SI(x)$id) }),
      sapply(.execution_plan$plan, function(x){ gsub('"', '\\\\"', format(x$call)) })
    )
  )
  
  from <- c()
  to <- c()
  
  for(inp in seq_along(.execution_plan$inputsData)){
    for(targ in findInExectionPlan(SI(.execution_plan$inputsData[[inp]]))){
      from <- c(from, inp)
      to <- c(to, Ninputs+targ)
    }
  }
  
  for(step in seq_along(.execution_plan$plan)){
    for(out in .execution_plan$plan[[step]]$outputs){
      for(targ in findInExectionPlan(out)){
        from <- c(from, Ninputs+step)
        to <- c(to, Ninputs+targ)
      }
    }
  }
  
  edges <- DiagrammeR::create_edge_df(
    from = from,
    to = to,
    rel = "related"
  )
  
  DiagrammeR::create_graph(
    nodes_df = nodes,
    edges_df = edges,attr_theme = NULL
    )
}
