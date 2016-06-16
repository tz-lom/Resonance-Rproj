pipe.transform.events <- function(input, transform){
  processor(
    input,
    prepare = function(env){
      SI.is.event(input) || stop("Input must be event")
      SI(input)
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