input.is.channels <- function(input){
  attr(input, 'type') == 'channels'
}

input.is.window <- function(input){
  attr(input, 'type') == 'window'
}

input.is.message <- function(input){
  attr(input, 'type') == 'message'
}

input.times <- function(input){
  attr(input, 'timestamps')
}
