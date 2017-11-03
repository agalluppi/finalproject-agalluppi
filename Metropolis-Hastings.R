mh.beta <- function(n.sims, burnin, shape1, shape2, c) {
  theta.cur <- runif(1,0,1)
  draws <- c()
  theta.update <- function(theta.cur, shape1, shape2, c) {
    theta.can <- rbeta(1, c*theta.cur, c*(1-theta.cur))
    accept.prob <- dbeta(theta.can, shape1 = shape1, shape2 = shape2)/dbeta(theta.cur,shape1 = shape1, shape2 = shape2)
    if (runif(1) <= accept.prob) theta.can <- theta.cur
  }
  for (i in 1:n.sims) {
    draws[i] <- theta.cur <- theta.update(theta.cur, shape1 = shape1,
                                          shape2 = shape2, c=c)
  }
  return(draws[(burnin + 1):n.sims])
}

mh.draws <- mh.beta(10000, 0, 4, 6, 1)
trueB <- rbeta(10000, 4,6)

par(mfrow=c(1,4))  #1 row, 4 columns
plot(mh.draws); acf(mh.draws); hist(mh.draws); hist(trueB)  #plot commands

drawsCDF <- ecdf(mh.draws)
realCDF<- ecdf(trueB)
x <- seq(0,1,0.01)
max(drawsCDF(x)-realCDF(x))
