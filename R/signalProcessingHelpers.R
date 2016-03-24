#' Typical signal preprocessing for EEG
#' 
signalPreparation <- function(input, channels=c(),refs=c(), low=0.1, high=40, notch=50){
  out <- input
  if(length(channels)>0){
    
    M <- diag(nrow=SI(input)$channels)[channels, ,drop=F]
    M[, refs] = -1/length(refs)
    
    out <- pipe.spatial(input, M)
  } else {
    if(length(refs)>0){
        out <- pipe.references(input, refs)
    }
  }
  pipe.bandFilter(out, low, high, notch)  
}