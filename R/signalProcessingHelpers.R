#' Typical signal preprocessing for EEG
#' 
signalPreparation <- function(input, refs=c(), low=0.1, high=40, notch=50){
  out <- input
  if(length(refs)>0){
      out <- pipe.references(input, refs)
  }
  pipe.bandFilter(out, low, high, notch)  
}