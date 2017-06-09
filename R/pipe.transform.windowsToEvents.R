pipe.transform.windowsToEvents <- function(input, transform){
  processor(
    input,
    prepare = function(env){
      SI.is.window(input) || stop("Input must be window")
      SI.event()
    },
    online = function(input){
      lapply(input, function(x){
        y <- transform(x)
        attr(y, 'TS') <- attr(x, 'TS')
        y
      })
    }
  )
}