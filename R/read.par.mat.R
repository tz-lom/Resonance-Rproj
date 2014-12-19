read.par.mat <- function(filename)
{
  params <- readMat(filename)$params
  names(params) <- sprintf("%s", attr(params, 'dimnames')[[1]])
  attr(params , "dimnames") <- NULL
  attr(params , "dim") <- NULL
  
  W <- as.numeric(params$W)
  th <- as.numeric(params$th)
  ufeats <- params$feats
  A1 <- as.numeric(params$A1.ch)
  A2 <- as.numeric(params$A2.ch)
  fixDur <- as.numeric(params$fixationDuration)
  sRate <- as.numeric(params$sRate)
  
  t <- fixDur / 1000 * sRate
  
  #res
  dump(c("W", "th", "ufeats", "A1", "A2", "fixDur", "sRate",
         "t"), file = "")
  cat(" ch <- to.channels(input(1))", "\n",
      "p1 <- pipe.references(ch, c(A1,A2))", "\n",
      "p2 <- pipe.centering(p1, 500)", "\n",
      "p3 <- pipe.decimate(p2, 1, 20 , coef_10000_to_500)", "\n",
      "p4 <- cross.windowizeByEvents(p3, input(2), t)", "\n",
      "p5 <- pipe.trof.classifier(p4, W, th, ufeats )", "\n",
      "output(1)$connect(p5)", "\n")
  
  
}

#' Filter that subtract average for each channel devided by standard deviation
#'
#' @param input Pipe connected to
#' @return Constructed pipe
pipe.centering <- function(input, bufferSize)
{
  bp <- block.processor(input)
  
  buffer <- matrix(0.0, ncol=input$channels, nrow=bufferSize)
  
  input$connect(function(db){
    buffer[1:(bufferSize-nrow(db)),] <<- buffer[nrow(db):bufferSize,]
    buffer[(bufferSize-nrow(db)): bufferSize,] <<- db
    
    bp$emit( DataBlock(db - colMeans(buffer), db) )
  })
  
  bp
}

#' Applying 
#'
#' @param input Pipe connected to
#' @param W array of weights from mat file
#' @param th threshold
#' @param ufeats feature matrix
#' @return Constructed pipe
pipe.trof.classifier <- function(input, W, th, ufeats)
{
  bp <- block.processor(input)
  
  input$connect(function(db){
    X <- rep(0, dim(ufeats)[[1]])
    for (i in 1:dim(ufeats)[[1]])
    {
      ts <- ufeats[i, 1]
      ch <- ufeats[i, 2]
      X[i] <- db[ts, ch]
    }
    Q = X * W
    
    if( Q < th){
      bp$emit(DataBlock(T, db))
    }
    
    bp
    
  })
  
}
