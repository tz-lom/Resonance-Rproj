R2EReadChannel <- function(file, channel){
  blocks <- Resonance:::blockLevelRead(file)
  
  
  sapply(blocks, function(b){
    if(!inherits(b, "DataBlock")) return(c())
    if(attr(b, "stream")!=channel) return(c())
    matrix(b, ncol=32)
  })
}