# .classifierEnv <- new.env()
# 
# resetClassifier <- function(){
#   .classifierEnv$parameters <- list()
#   .classifierEnv$classifier <- list()
# }
# 
# resetClassifier()
# 
# 
# .classifierEnv$parameters 
# 
# classifierParameter <- function(name){
#   if(.classifierEnv$parameters[[name]]){
#     .classifierParameters[[name]]$value
#   }else{
#     warning("Classifier parameter '", name, "' is not described", call. = F)
#     NULL
#   }
# }
# 
# `classifierParameter<-` <- function(name, value){
#   .classifierParameters[[name]]$value <- value
# }
# 
# classifierResult <- function(name){
#   .classifierEnv$classifier[[name]]
# }
# 
# `classifierResult<-` <- function(name, value){
#   .classifierEnv$classifier[[name]] <- value
# }
# 
# 
# wrapClassifierParameters <- function(FUN){
#   NEW <- FUN
#   
#   args <- formals(F)
#   
#   for(i in 1:length(args)){
#     name <- names(args)[[i]]
#     if(substr(name, 1, 2)=='..'){
#       args[[i]] <- call("classifierParameter", substring(name, 3))
#     }
#   }
#   formals(NEW) <- args
#   NEW
# }
# 
