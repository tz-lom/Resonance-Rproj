#' Extracts spectrograms for windows
#'
#' @param input Input pipe
#' @param choose DataFrame with columns 'channel' and 'frequency' 
#' @return Row where each element corresponds to row from choose and equals to power of channel
pipe.spectrogram <- function(input){
  processor(
    input,
    prepare = function(env){
      if(!SI.is.window(input)) return("FFT filter must receive window");
      
      SI(input)
    },
    online = function(input){
      ret <- lapply(input, function(){
        spectre <- mvfft(db)
        Mod(spectre)/nrow(input)
      })
      attr(ret, 'TS') <- sapply(input, attr, 'TS')
    })
}