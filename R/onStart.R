onStart <- function(){
  if(is.function(globalenv()$onStart)) {
    globalenv()$onStart()
  }
}
