test_that("spatial", {
  src <- block.processor("channels", channels=2)
  spat <- pipe.spatial(src, matrix(c(0,1,1,0), ncol=2))
  out <- drain.debug(spat)
  
  src$emit(matrix(1:10,ncol=2))
  
  expect_equal(out$last_received(), matrix(c(6:10,1:5),ncol=2))
})


test_that("Debug drain", {
  testData <- data.frame(c=1:100)
  src <- block.processor("channels", channels=1)
  out <- drain.debug(src)
  
  src$emit(testData)
  
  expect_equal(out$last_received(), testData)
  
})

test_that("FFT filter",{
  # prepare data
  testData <- as.matrix(data.frame(c1 = 1:100, c2=100:1, c3=c(1:50,50:1)))
  
  fft1 <- fft(testData[,'c1'])
  fft2 <- fft(testData[,'c2'])
  fft3 <- fft(testData[,'c3'])
  
  r <- matrix(
    Mod(c(fft1[[11]],  fft2[[13]], fft3[[12]], fft1[[18]], fft3[[13]], fft3[[12]]))/100,
    nrow=1
    )
  
  #setup pipeline
  src <- block.processor("window", size=100)
  proc <- pipe.FFTFilter(
    src,
    choose=data.frame(
      channel=c('c1','c2','c3','c1','c3','c3'),
      frequency=c( 10 , 12 , 11 , 17 , 12 , 11 )
      ))
  out <- drain.debug(proc)
  
  #test pipeline
  src$emit(DataBlock(testData, 11234))
  
  expect_equal(out$last_received(), DataBlock(r, 11234))
})

test_that("windowizer", {
  src <- block.processor("channels", channels=3, samplingRate=112)
  wnd <- pipe.windowizer(src, 10, 3)
  out <- drain.debug(wnd)
  
  src$emit(DataBlock(matrix(1:30, ncol=3,byrow = T), 11234))
  expect_equal(out$last_received(), DataBlock(matrix(1:30, ncol=3,byrow = T), 11234))
  
  out$reset()
  
  sr <- as.integer64(1E9/112)
  
  src$emit(DataBlock(matrix(1:3, ncol=3,byrow = T), 11234+sr*3))
  expect_equal(out$last_received(), NULL)
  src$emit(DataBlock(matrix(1:9, ncol=3,byrow = T), 11234+sr*12))
  expect_equal(out$last_received(), DataBlock(matrix(c(10:30,1:3,1:6),ncol=3,byrow = T), 11234+sr*9))
  src$emit(DataBlock(matrix(1:6, ncol=3,byrow = T), 11234+sr*18))
  expect_equal(out$last_received(), DataBlock(matrix(c(19:30,1:3,1:9,1:6),ncol=3,byrow = T), 11234+sr*18))
})

test_that("linearApproximation", {
  src <- block.processor("channels", channels=3)
  la <- pipe.linearApproximation(
    src,
    list(
      list(
        1, #channel number
        c(1,1), #sequence of lines
        c(5,2),
        c(9,0)
        )
      )
    )
  out <- drain.debug(la)
  src$emit(matrix(c(0, 0.5, 1, 3, 5, 7, 9, 11, 1:16),ncol=3))
  expect_equal(out$last_received(), matrix(c(0, 0.5, 1, 1.5, 2, 1, 0, 0) ,nrow=1))
})

test_that("applyFilter", {
  require(signal)
  filt <- butter(4, .1)
  
  src <- block.processor("channels", channels=1)
  f <- pipe.applyFilter(src, filt)
  out <- drain.debug(f)
  
  s <- matrix(sin(2*pi*3*seq(0,1,length.out = 100)), ncol=1)
  src$emit(s)
  o1 <- out$last_received()
  
  #generally we are worried about how filter behaves on block concatenations, 
  # not about how it behave  
  
  src <- block.processor("channels", channels=1)
  f <- pipe.applyFilter(src, filt)
  out <- drain.debug(f)
  
  o2 <- c()  
  for(i in seq(1,100,by = 2)){
    src$emit(s[0:1+i,,drop=F])
    o2 <- rbind(o2, out$last_received())
  }

  expect_equal(o1,o2)
})

test_that("simulationSource", {
  i <- simulationSource(2, 5, 5)
  o <- drain.debug(i)
  
  
  #@todo: remaster to propper implementation
#   l <- list()
#   count <- 0
#   
#   o$set_callback(function(x){
#     l <<- rbind(l,x)
#     count <<- count+1
#   })
  
  data <- matrix(1:102, ncol=2)  
  
  i$emit(data)
  #expect_equal(count, 10)
  #expect_equivalent(l, data[1:50,])
  expect_equal(
    o$last_received(),
    DataBlock(matrix(c(46:50,97:101),ncol=2), 0)
  )
})

test_that("decimate", {
  src <- block.processor("channels", channels=10)
  f <- pipe.decimate(src, 1, 10, Resonance::coef_10000_to_500) # @todo: very bad
  out <- drain.debug(f)
  
  s <- matrix(1:10000, ncol=10)
  src$emit(s)
  src$emit(s)
  o <- out$last_received()
  expect_equal(100, nrow(o))
  expect_equal(10, ncol(o))
})

test_that("simple setup", {
  #init IO
  Resonance:::clearStreams()
  Resonance:::registerInput(name = "in", channels=32, samplingRate = 10000)
  
  #decimation filter
  asCh <- to.channels(input(1))
  decimated <- pipe.decimate(asCh, 1, 20, coef_10000_to_500)  # @todo: bad example
  bp <- butter(4,c(1*2/500,30*2/500))
  
  filtered <- pipe.applyFilter(decimated, bp)
  
  Resonance:::createOutput(name = "out", type="channels", filtered)
  
  data <- DataBlock(matrix(1:32000, ncol = 32), 0)
  input(1)$emit(data)
  input(1)$emit(data)
})

test_that("windowizer - big window", {
  src <- block.processor("channels", channels=3, samplingRate=128)
  wnd <- pipe.windowizer(src, 100, 3)
  out <- drain.debug(wnd)
  
  src$emit(DataBlock(matrix(1:30, ncol=3, byrow = T), 11234))
  expect_equal(out$last_received(), NULL)
  
  out$reset()
  
  src$emit(DataBlock(matrix(31:3000, ncol=3,byrow = T), 11234+2970/128*1E9))
  expect_equal(
    out$last_received(), 
    DataBlock(matrix(2701.0:3000, ncol=3, byrow=T), 11234+2970/128*1E9)
    )
})

test_that("reference channels", {
  src <- block.processor("channels", channels=10)
  f <- pipe.references(src, c(1,5,10))
  out <- drain.debug(f)
  
  s <- matrix(1:10000, ncol=10)
  src$emit(s)
  o <- out$last_received()

})

