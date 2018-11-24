# This file is used for low-level Resonance-Api test
# The test is pretty straightforward - we are expecting certain calls in certain order
# and test that they are performed in that order.

# Expected order
# onPrepare()
# onStart()
# onDataBlock()
# onStop()

onPrepare <- function(a,b,c){
  cat("onPrepare")
}
