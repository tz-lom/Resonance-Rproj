# test_that("cross.filterByTimestamps", {
#   data <- block.processor("channels", channels=2, samplingRate=9)
#   events <- block.processor("event")
#   
#   fbt <- cross.filterByTimestamps(
#     data, events
#   )
#   
#   rec <- drain.debug(fbt)
#   
#   sample <- as.integer64(1E9/9)
#   
#   #sending some data
#   data$emit(DataBlock(matrix(1:6, ncol=2), (12+3)*sample))
#   data$emit(DataBlock(matrix(7:12, ncol=2), (12+6)*sample))
#   data$emit(DataBlock(matrix(13:18, ncol=2), (12+9)*sample))
#   
#   events$emit(DataBlock(T, (12+4)*sample))
#   
#   data$emit(DataBlock(matrix(19:24, ncol=2), (12+12)*sample))
#   data$emit(DataBlock(matrix(25:30, ncol=2), (12+15)*sample))
#   data$emit(DataBlock(matrix(31:36, ncol=2), (12+18)*sample))
#   
#   events$emit(DataBlock(T, (12+17)*sample))
#   
#   data$emit(DataBlock(matrix(37:42, ncol=2), (12+21)*sample))
#   
#   
#   expect_identical(
#     rec$all_received(),
#     list(
#       DataBlock(matrix(7:12, ncol=2), (12+6)*sample),
#       DataBlock(matrix(13:18, ncol=2), (12+9)*sample),
#       DataBlock(matrix(19:24, ncol=2), (12+12)*sample),
#       DataBlock(matrix(25:30, ncol=2), (12+15)*sample),
#       DataBlock(matrix(31:36, ncol=2), (12+18)*sample)
#     )
#   )
# })