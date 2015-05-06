#' Create event stream
#' 
#' @param events List of events (will be coerced to list), must have attribute TS with timestamp or *timestamps* parameter specified
#' @param timestamps List of timestamps
source.events <- function(events, timestamps=NULL){
  events <- as.list(events)
  
  if(!is.null(timestamps)){
    if(length(events)!=length(timestamps)) stop("Amount of timestamps must be equal to amount of events")
    events <- mapply(function(e,t){
      attr(e, 'TS') <- t
      e
    }, events, timestamps, SIMPLIFY=F)
  }
  if(is.null(attr(events[[1]], 'TS'))) stop("Events must be timestamped")
  
  SI(events) <- SI.event()
  events
}