run.online <- function(blocks, code){
  
  require(Resonate)
  
  
#  dataHeader <- list(???)
  
  onPrepare(dataHeader, code)
  
  nextEeg <- function(b){
    onDataBlock.double(id = 1, vector = t(b), samples = nrow(b), timestamp = attr(b, 'created'))
  }
  
  nextClick <- function(b){
    onDataBlock.message(id = 2, msg = as.character(b), timestamp = attr(b, 'created'))
  }
  
  nextBlock <- function(b){
    if(!inherits(b, "DataBlock")) return()
    if(attr(b, 'stream')==0) nextEeg(b) else nextClick(b)
  }
  lapply(blocks, nextBlock)
  
  Q <- popQueue()
  
}