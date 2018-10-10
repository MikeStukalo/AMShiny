# Miscellaneous functions
updateweight = function(oldweight, new, i) {
  if (new==oldweight[i]) {
    oldweight
  } else {
    newweight = rep(0,6)
    oldweight = oldweight + 1e-6 # To avoid NaN
    newweight[-i] = oldweight[-i]/sum(oldweight[-i])*(1-new)
    newweight[i] = new
    newweight
  }
}

# suspend and resume a list of observers
suspendMany = function(observers) invisible(lapply(observers, function(x) x$suspend()))
resumeMany = function(observers) invisible(lapply(observers, function(x) x$resume()))

# function to change sliderInput
wghtsliderInput = function(inputId,value, label, submitted=FALSE) {
  if (!submitted)
    sliderInput(inputId=inputId,
                value=value,
                label=label,
                min=0,
                max=1,
                ticks=FALSE)
}
