## ---- run_NMI
run_NMI <- function(x,y) {
  # implemented scots rule to determine number of bins in continuous data
  # https://en.wikipedia.org/wiki/Histogram
  D1 = ((max(x)-min(x))/(3.5*sd(x)*length(x)^-(1/3)))
  D2 = ((max(y)-min(y))/(3.5*sd(y)*length(y)^-(1/3)))
  y2d = discretize2d(x, y, numBins1=D1, numBins2=D2)
  Entropy = entropy(y2d)
  MutualInfo = mi.empirical(y2d)
  Out <- list("Entropy" = Entropy,"MutualInformation" = MutualInfo, "Bins for X " = D1, "Bins for Y " = D2 )
  return(Out)
}
## ---- end-run_NMI
