prepare <- function(process, prepare=NULL){
  list(
    prepare = substitute(prepare),
    process = substitute(process)
  )
}
