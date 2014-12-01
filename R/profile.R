testSaver <- function(){
  bp <- block.processor("channels", channels=32)
  out <- drain.channelsRecorder(bp)
  
  block <- DataBlock(matrix(as.double(1:1024), ncol=32), 124365)
  
  for(i in 1:1)
  {
    bp$emit(block)
  }
}


testProf <- function(){
  require(bcidat)
  data <- load_bcidat('~/sources/n10/unS020R04.dat')
  
  stream <- source.channels(data$signal[1:60000,], 500, 25)
  filtered <- pipe.windowizer(stream, 500, 25)
  out <- drain.windowRecorder(filtered)
  #drain.terminator(filtered)
  pump(stream)
}