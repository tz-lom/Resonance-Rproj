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

processBlock = function(block){
  execId <- findInExectionPlan(SI(block))
  if(length(execId)>0){
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
        processBlock(data)
      }, result, target$outputs)
    } else {
      SI(result) <- target$outputs[[1]]
      processBlock(result)
    }
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

renderExecutionPlanAsGraph <- function(render=TRUE){
  if(length(find.package('DiagrammeR', quiet = TRUE)) == 0L){
    stop("Please install package 'DiagrammeR'")
  }
  
  if(length(.execution_plan$plan)==0){
    warning('No execution plan exists')
    return(NULL)
  }
  
  Ninputs <- length(.execution_plan$inputsData)
  
  nodes <- DiagrammeR::create_node_df(
    n = Ninputs + length(.execution_plan$plan),
    label = c(
      sapply(.execution_plan$inputsData, function(x){ paste("Input", SI(x)$id) }),
      sapply(.execution_plan$plan, function(x){ as.character(x$call[[1]]) })
    )
  )
  
  # gsub('"', '\\\\"', paste0(format(x$call), collapse='\n'))
  
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
  
  theme <-
    data.frame(attr = c("layout", "outputorder", "fontname", 
          "fontsize", "shape", "fixedsize", "style", "fillcolor", "color", 
          "fontcolor", "bgcolor", "fontname", "fontsize", "len", "color", 
          "arrowsize"), 
        value = c("neato", "edgesfirst", "Helvetica", "10", 
          "box", "false", "filled", "aliceblue", "gray70", "gray50", "white", 
          "Helvetica", "8", "1.5", "gray80", "0.5"), 
        attr_type = c("graph", 
          "graph", "node", "node", "node", "node", "node", "node", "node", 
          "node", "graph", "edge", "edge", "edge", "edge", "edge"),
        stringsAsFactors = FALSE)
  
  graph <- DiagrammeR::create_graph(
    nodes_df = nodes,
    edges_df = edges
  )
  graph$global_attrs <- theme
  
  if(render==FALSE){
    return(graph)
  }
  
  DiagrammeR::render_graph(
    graph,
    layout="tree"
  )
}
