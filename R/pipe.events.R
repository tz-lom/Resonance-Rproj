pipe.eventsEquals <- function(input, value){
  bp <- block.processor(input)
  
  input$connect(function(db){
    bp$emit(DataBlock(isTRUE(db==value), db))
  })
  
  bp
}