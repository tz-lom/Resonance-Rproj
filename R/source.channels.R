#' Wraps raw data stream to use withing pipelinenig functions
#' 
#' @param data matrix
#' @param samplingRate
#' @param timestamps You can specify timestamps, otherwise they will be calculated automatically starting from 0
source.channels <- function(data, samplingRate, timestamps=NULL){
  data <- as.matrix(data)

  if(is.null(timestamps)){
    timestamps <- seq(from=0, by=(1E6/samplingRate), length.out=nrow(data))
  }
  attr(data, 'TS') <- timestamps
  SI(data) <- SI.channels(channels = ncol(data), samplingRate = samplingRate)
  data
}