filter.by.timestamp <- function(filename)
{
  data <- load_bcidat(filename)
  
  skip.stimulus <- 3
  block.size <- 25
  
  sRate <- as.numeric(gsub('Hz','', data$parameters$SamplingRate))
  
  si <- data$signal
  
  #events
  stim.code <- data$states[,'StimulusCode']
  stim.code <- replace(stim.code, which(stim.code==skip.stimulus), 0)
  
  breaks <- which(diff(stim.code)!=0)+1
  
  times <- microsec(breaks, sRate)
    
  
  events <- vector("list", length(times))
  for(i in 1:length(times)) {
    events[[i]] <- list( 
      type = as.logical(stim.code[breaks])[[i]],
      time = times[[i]]
    )
  }
  
  attr(si, 'times') <- microsec(1:nrow(si), sRate)
  
  
  list(si, events)
}

reset <- function(){
  
  signal <<- matrix(0.0, nrow = 2^5, ncol = 62)
  pointer <<- 0L
  si.times <<- c()
  evs <<- list()
  index <<- integer64(1)
  
}

split_data_to_streams <- function(data){
  eeg <- data[[1]]
  
  dataBlocks <- list()
  ii <- 1
  
  for (i in seq(1, nrow(eeg), 25))
  {
    res <- eeg[i:(i+24), ]
    attr(res,'time') <- attr(eeg,'time')[i:(i+24)]
    dataBlocks[[ii]] <- res
    ii <- ii + 1
  }
  
  eventBlocks <- data[[2]]
  
  list(dataBlocks, eventBlocks)
}

test_bulk <- function(data){
  reset()
  epoch.by.event(data[[1]], data[[2]])
}

test_data_then_events <- function(dataBlocks, eventBlocks){
  reset()
  res <- NULL
  for(i in 1:length(dataBlocks)) {
    res <- rbind(res, epoch.by.event(dataBlocks[[i]], NULL))
  }
  
  for(i in 1:length(eventBlocks)) {
    res <- rbind(res, epoch.by.event(NULL, eventBlocks[i]))
  }
  
  res
}

test_events_then_data <- function(dataBlocks, eventBlocks){
  reset()
  res <- NULL
  for(i in 1:length(eventBlocks)) {
    res <- rbind(res, epoch.by.event(NULL, eventBlocks[i]))
  }
  
  for(i in 1:length(dataBlocks)) {
    res <- rbind(res, epoch.by.event(dataBlocks[[i]], NULL))
  }
  
  res
}

test_ordered_events_and_data <- function(ordered){
  reset();
  
  res <- NULL
  
  for(i in 1:length(ordered)){
    res <- rbind(res, epoch.by.event(ordered[[i]][[1]], ordered[[i]][[2]]))
  }
  
  res
}

sort_streams <- function(data, events){
  
  all <- c(
    lapply(data, function(x) list(x, NULL)),
    lapply(events, function(x) list(NULL, list(x)))
  )
  
  times <- c(
    sapply(data, function(x) attr(x,'time')[[1]]),
    sapply(events, function(x) x$time)
    )
  
  per <- order(times)
  
  all[per]
}

quick_test <- function()
{
  data <- filter.by.timestamp("/home/mayenok/tmp/unS020R04.dat")
  streams <- split_data_to_streams(data)
  
  dataBlocks <- streams[[1]]
  eventBlocks <- streams[[2]]
  
  reset()
  
  for(i in 1:length(eventBlocks)) {
    b <- epoch.by.event(NULL, eventBlocks[i]);
    #force(b)
  }
  
  for(i in 1:length(dataBlocks)) {
    a <- epoch.by.event(dataBlocks[[i]], NULL)
    #force(a)
  }
  
}


test <- function(){
  
  data <- filter.by.timestamp("/home/mayenok/tmp/unS020R04.dat")
  streams <- split_data_to_streams(data)
  ordered <- sort_streams(streams[[1]], streams[[2]])
  
  res1 <- test_bulk(data)
  res2 <- test_data_then_events(streams[[1]], streams[[2]])
  cat("identical(res1, res2) = ", all.equal(res1, res2, check.attributes = F))
  
  res3 <- test_events_then_data(streams[[1]], streams[[2]])
  cat("\n", "identical(res1, res3) = ", all.equal(res1, res3, check.attributes = F))
  
  res4 <- test_ordered_events_and_data(ordered)
  cat("\n", "identical(res1, res4) = ", all.equal(res1, res4, check.attributes = F))
    
  c(identical(res1, res2), identical(res1, res3), identical(res1, res4))
}

bench <- function(){
  
  data <- filter.by.timestamp("/home/mayenok/tmp/unS020R04.dat")
  streams <- split_data_to_streams(data)
  ordered <- sort_streams(streams[[1]], streams[[2]])
  
  microbenchmark(
    test_bulk(data),
    test_data_then_events(streams[[1]], streams[[2]]),
    test_events_then_data(streams[[1]], streams[[2]]),
    test_ordered_events_and_data(ordered),
    times=1
  )
}

microsec <- function(x,sRate)
{
  as.integer64(x)*1E9 %/% sRate
}