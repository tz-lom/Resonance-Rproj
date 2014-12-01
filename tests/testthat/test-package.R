
test_that("Queue preserves ordering", {
  Resonance:::popQueue()
  Resonance:::addToQueue("A")
  Resonance:::addToQueue("B", foo=42)
  
  expect_identical(
    Resonance:::popQueue(),
    list(
      list(cmd="A",
           args=list()),
      list(cmd="B",
           args=list(
             foo=42
             ))
      )
  )
  expect_identical(
    Resonance:::popQueue(),
    list()
    )
})

test_that("Simple pipeline", {
  # register streams
  Resonance:::clearStreams()
  Resonance:::registerInput("in", c("a","b"), 42)
  Resonance:::createOutput("out", 'channels', input(1))
  
  #send test data block
  Resonance:::blockReceived(1, 10, 123, 1:20)
  
  expect_equal(
    Resonance:::popQueue(),
    list(
      list(
        cmd='createOutputStream',
        args=list(
          id=1,
          name='out',
          typeName='channels',
          samplingRate=42,
          channels=c('a','b')
        )
      ),
      list(
        cmd="sendBlockToStream",
        args=list(
          id=1,
          data=DataBlock(matrix(1:20, ncol=2, byrow = T), 123)
        )
      )
    )
    )
})

test_that("Inverse pipeline", {
  # register streams
  Resonance:::clearStreams()
  Resonance:::registerInput("in", c("a","b"), 42)
  
  # setup test pipeline
  inv <- function(input){
    bp <- block.processor(input);
    
    transformation <- function(db){
      bp$emit(-db)
    };
    
    input$connect(transformation)
    bp
  }
  
  Resonance:::createOutput("out", 'channels', inv(input(1)))
  
  #send test data block
  Resonance:::blockReceived(1, 10, 123, 1:20)
  
  expect_equal(
    Resonance:::popQueue(),
    list(
      list(
        cmd='createOutputStream',
        args=list(
          id=1,
          name='out',
          typeName='channels',
          samplingRate=42,
          channels=c('a','b')
        )
      ),
      list(
        cmd="sendBlockToStream",
        args=list(
          id=1,
          data=DataBlock(-matrix(1:20, ncol=2, byrow = T), 123)
        )
      )
    )
  )
  
})

