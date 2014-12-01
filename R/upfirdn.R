upFirDown <- function(upRate, downRate, coef){
  filt <- .Call('upfirdn__new', upRate, downRate, coef, PACKAGE = 'Resonance')
  function(data){
    .Call('upfirdn__apply_multichannel', filt, data, PACKAGE = 'Resonance')
  }
}

