pipe.channelsToEvents <- function(input, transform){
  processor(
    input,
    prepare = function(env){
      SI.is.event(input) || stop("Input must be event")
      
      SI(input)
    },
    online = function(input){
      ret <- apply(input, 1, transform)
      mask <- !sapply(ret, is.NULL)
      ret <- ret[mask]
      TS <- sapply(input[mask], attr, 'TS')
      mapply(function(ev, ts) {
        attr(ev, 'TS') <- ts
        ev
      }, ret, TS, SIMPLIFY = FALSE)
    }
  )
}