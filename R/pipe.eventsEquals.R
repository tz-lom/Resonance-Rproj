#' Transforms events to boolean
#' By rule event == match
#'
#' @param input event stream
#' @param match value to match
#'
#' @return event stream
pipe.eventsEquals <- function(input, match){
  processor(
    input,
    prepare = function(env){
      SI.is.event(input) || stop("Input must be event")
      SI(input)
    },
    online = function(input){
      lapply(input, function(x){
        ret <- isTRUE(x==match)
        attr(ret, 'TS') <- attr(x, 'TS')
        ret
      })
    })
}
