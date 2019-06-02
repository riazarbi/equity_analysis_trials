direc <- paste(getwd(), "/backtests/", sep="")
library(pbo)
set.seed(765)
n <- 50
t <- 2000
m <- data.frame(matrix(rnorm(n*t),nrow=t,ncol=n,dimnames=list(1:t,1:n)),
                check.names=FALSE)


sr_base <- 0
mu_base <- sr_base/(365.0)
sigma_base <- 1.00/(365.0)**0.5

for ( i in 1:n ) {
  m[,i] = m[,i] * sigma_base / sd(m[,i]) # re-scale
  m[,i] = m[,i] + mu_base - mean(m[,i]) # re-center
}

o <- mutate_all(m, funs("value"=(1+.)*(1+lag(.))))
o <- o[51:100]
glimpse(o)
library(lubridate)
datenow <- date(Sys.time())
o$date <- seq(from=datenow-days(nrow(o)), to=datenow-1, by="day")
write_feather(o, paste(direc, "PBO/all_total_returns.feather", sep=""))

library(lubridate)
datenow <- date(Sys.time())
n <- m
n$date <- seq(from=datenow-days(nrow(m)), to=datenow-1, by="day")
write_feather(n, paste(direc, "PBO/all_daily_returns.feather", sep=""))

sharpe <- function(x,rf=0.03/252) {
  sr <- apply(x,2,function(col) {
    er = col - rf
    return(mean(er)/sd(er))
  })
  return(sr)
}

my_pbo <- pbo(m,s=8,f=sharpe,threshold=0)

summary(my_pbo)

require(lattice)
require(latticeExtra)
require(grid)
histogram(my_pbo,type="density")