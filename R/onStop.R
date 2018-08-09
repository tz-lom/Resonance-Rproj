onStop <- function(){
  if(is.function(globalenv()$onStop)) {
    globalenv()$onStop()
  }
}
