createOutput <- function(data, name){

  processor(
    data,
    prepare = function(env){

      id <- .execution_plan$nextOutputId
      .execution_plan$nextOutputId <- .execution_plan$nextOutputId+1

      args <- SI(data)
      args$id <- id
      args$name <- name

      do.call(addToQueue, c(list("createOutputStream"), args))

      env$id <- id

      if(SI.is.channels(data))
      {
        env$cb <- function(data){
          if(length(data)>0){
            addToQueue(
              "sendBlockToStream",
              id = id,
              data= data
            )
          }
          list()
        }
      } 
      else if(SI.is.event(data)) 
        {
        env$cb <- function(data){
          for(d in data){
            addToQueue(
              "sendBlockToStream",
              id = id,
              data= d
            )
          }
          list()
        }
      }
      else if(SI.is.window(data))
      {
        env$cb <- function(data){
          for(d in data){
            addToQueue(
              "sendBlockToStream",
              id = id,
              data= d
            )
          }
          list()
        }
      }
      else if(SI.is.epoch(data))
      {
        env$cb <- function(data){
          for(d in data){
            addToQueue(
              "sendBlockToStream",
              id=id,
              data=d
            )
          }
          list()
        }
      }
      else
      {
        stop("[createOutput] Unsupported stream type=",SI(data)$type, call.=F)
      }

      SI.outputStream(name, id)

    },
    online = function(data){
      cb(data)
    }
  )

}
