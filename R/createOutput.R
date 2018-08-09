createOutput <- function(data, name){

  processor(
    data,
    prepare = function(env){

      id <- .globals$outputId
      .globals$outputId <- .globals$outputId+1

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
      if(SI.is.event(data))
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
      if(SI.is.window(data))
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

      id

    },
    online = function(data){
      cb(data)
    }
  )

}
