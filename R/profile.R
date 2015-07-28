testSaver <- function(){
  bp <- block.processor("channels", channels=32, samplingRate=1024)
  FF <- pipe.bandFilter(bp, lowFreq = 6, highFreq = 40, notch = 0)
  out <- drain.channelsRecorder(FF)
  
  block <- DataBlock(matrix(as.double(1:(1024*32)), ncol=32), 124365)
  
  for(i in 1:1E2)
  {
    bp$emit(block)
  }
}

testProf <- function(){
  require(bcidat)
  data <- load_bcidat('~/sources/PowerClaws/TESTdataset/2-states bad case (1 3), good case (1 2)/slS038R04.dat')
  
  stream <- source.channels(data$signal, samplingRate = 500)
  flt <- pipe.bandFilter(stream, lowFreq = 6, highFreq = 40, notch = 0)
  wnd <- pipe.windowizer(flt, 500, 25)
  FF <- pipe.FFTFilter(wnd, expand.grid(channel=1:ncol(data$signal), frequency=1:40))
  #drain.terminator(filtered)
}

wtf <- function(){
  require(bcidat)
  data <- load_bcidat('~/sources/PowerClaws/TESTdataset/2-states bad case (1 3), good case (1 2)/slS038R04.dat')
  
  s <- 10000:30000
  
  dataStream <- source.channels(data$signal[s,], samplingRate = 500)
  ff <- pipe.bandFilter(dataStream, lowFreq = 6, highFreq = 40, notch = 0)
  #eventStream <- source.channeledEvents(data$states[s,'StimulusCode'], 500)
  
  #state1 <- pipe.eventsEquals(eventStream, 1)
  
  #spm1 <- cross.filterByTimestamps(dataStream ,state1)
  
  
  ff
}


