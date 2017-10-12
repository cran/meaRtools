## ----rsetup-knitr,eval=TRUE,include=FALSE-----------------
require(knitr)
options(width=60)
opts_chunk$set(cache=TRUE)

## ---------------------------------------------------------
require(meaRtools)

## ---------------------------------------------------------
poisson_train <- function(n = 1000, rate = 1, beg = 0) {
  ## Generate a Poisson spike train with N spikes and firing rate RATE.
  ## BEG is time of start of recording
  ## Check that the histogram looks exponentially distributed.
  ## hist( diff(poisson_train()))
  x <- runif(n)
  isi <- log(x) / rate
  spikes <- beg - cumsum(isi)
  spikes
}

a = poisson_train()
b = sort(a + runif(length(a), -0.1, 0.1))
c = b + 0.8
allspikes = list(a=a, b=b, c=c)

## ---------------------------------------------------------
range = range(unlist(allspikes))
beg = range[1]
end = range[2]
sttc(a, b, dt=0.01, rec_time = c(beg, end))

## ---------------------------------------------------------
sttc_allspikes1(allspikes, 0.01, beg, end)

## ---- fig.width = 6, fig.height=6-------------------------
par(mfrow=c(2,2), las=1)
dt = 1.0;  plot(sttcp(a, b, dt = dt, beg = beg, end = end), main=sprintf("a x b: dt = %.3f",dt))
dt = 0.5;  plot(sttcp(a, c, dt = dt, beg = beg, end = end), main=sprintf("a x c: dt = %.3f",dt))
dt = 0.1;  plot(sttcp(a, c, dt = dt, beg = beg, end = end), main=sprintf("a x c: dt = %.3f",dt))
dt = 0.05; plot(sttcp(a, c, dt = dt, beg = beg, end = end), main=sprintf("a x c: dt = %.3f",dt))

