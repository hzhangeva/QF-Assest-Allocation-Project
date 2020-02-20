# Install libraries firstly!
library(riskParityPortfolio)
library(Matrix)


equal_weighted <- function(dataset) {
  print(0)
  ticker <- selection[,count1+1]
  ticker <- setdiff(ticker,exception)
  #print(ticker)
  
  #print(1.1)
  prices <- dataset$close
  N <- ncol(prices)
  prices_org <- dataset$close
  prices<-na.trim(prices)
  prices <- na.approx(prices)
  prices <- prices[,ticker]
  
  log_returns <- diff(log(prices))[-1]
  w <- riskParityPortfolio(cov(log_returns))$w
  
  w_t <- prices_org[1,]
  w_t[1,] = 0
  w_t[,ticker] <- w
  w_t <- as.numeric(w_t)
  count1 <<- count1 + 1
  #print(length(w_t))
  #print(sum(w_t))
  return(w_t)
}


# define portfolios to be backtested
# risk parity portfolio
risk_parity <- function(dataset) {
  print(1)
  ticker <- selection[,count1+1]
  ticker <- setdiff(ticker,exception)
  #print(ticker)
  
  #print(1.1)
  prices <- dataset$close
  prices_org <- dataset$close
  prices<-na.trim(prices)
  prices <- na.approx(prices)
  prices <- prices[,ticker]
  N <- ncol(prices)
  w <- rep(1/N,N)
  w_t <- prices_org[1,]
  w_t[1,] = 0
  w_t[,ticker] <- w
  w_t <- as.numeric(w_t)
  count0 <<- count0 + 1
  #print(length(w_t))
  #print(sum(w_t))
  return(w_t)
}

# tangency portfolio (maximum sharpe ratio)
library(quadprog)
max_sharpe_ratio <- function(dataset) {
  
  prices <- dataset$close
  prices_org <- dataset$close
  N_org <- ncol(prices_org)
  ticker <- selection[,count2+1]
  ticker <- setdiff(ticker,exception)
  print(2)
  prices <- prices[,ticker]   # get our list
  #print(sum(is.na(prices)))
  prices<-na.trim(prices)
  prices<-na.approx(prices)
  #print(length(prices[,1]))
  log_returns <- diff(log(prices))[-1]
  
  N <- ncol(prices)
  Sigma <- cov(log_returns)
  mu <- colMeans(log_returns)
  if (all(mu <= 1e-8))
    return(rep(0, N_org))
  Dmat <- 2 * Sigma
  Dmat <- nearPD(Dmat)$mat
  Amat <- diag(N)
  Amat <- cbind(mu, Amat)
  bvec <- c(1, rep(0, N))
  dvec <- rep(0, N)
  
  res <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 1)
  print(sum(is.na(prices)))
  w <- res$solution
  round(w, digit = 3)
  w <- w/sum(w)
  w_t <- prices_org[1,]
  w_t[1,] = 0
  w_t[,ticker] <- w
  w_t <- as.numeric(w_t)
  count2 <<- count2 + 1
  return(w_t)
}

