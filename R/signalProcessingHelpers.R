#' Typical signal preprocessing for EEG
#' 
signalPreparation <- function(input, refs, low=0.1, high=40, notch=50){
  pipeline(
    pipe.references(input, refs),
    pipe.bandFilter(, low, high, notch)
  )
}