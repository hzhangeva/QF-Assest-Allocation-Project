# Install libraries firstly! "riskParityPortfolio" "fPortfolio" "xts" "portfolioBacktest"


change = 20    # change our selection every 20 days
num = 20       # select 20 stocks everytime
period = 60    # use past 60 days to select
return <- read.csv("./dailyreturn.csv", row.names=1, stringsAsFactors=FALSE)
selection = data.frame(row.names=1:num)

for (i in seq(61,1000,change)){
  mydata = return[i:(i+period),]
  sort=data.frame(name=character(0), alpha=numeric(0), stringsAsFactors=FALSE)
  for (j in 1:505){
    if (!is.na(return[i,j])){
      lm = lm(c(mydata[,j]-mkt.rf)~mkt.rf+SMB+HML, data=mydata)
    }
    sort[nrow(sort)+1,1] = names(return)[j]
    sort[nrow(sort),2]=lm$coefficients[1]
  }
  sort = sort[order(sort$alpha),]
  sort = sort[1:num,]
  selection[row.names(return)[i]] = sort[,1]
}

# test optimize_weight
# x<-return[selection[,1]]
# #直接刪除有缺失值的列、行
# a = t(na.omit(t(x)))#列
# b = t(na.omit(matrix))#行
# optimize_weight(a,method = 'sharpe')
# optimize_weight(a,method = 'parity')





# Do backtest using "portfolioBacktest"
library(xts)
library(portfolioBacktest)
library(riskParityPortfolio)
source("Optimization.R")

# Prepare backtest data of SP500 using our ticker list
# Transfrom our ticker list into following data type, which is atomic.
# SP500_symbols
ticker_list <- read.csv("~/Desktop/MLE_r_project/Gupta_ML_Project/ticker_list.csv", header=FALSE, stringsAsFactors=FALSE)
ticker_list <- c(t(ticker_list))
ticker_list[114]='F'
# Get sp500 stock data of our ticker list
exception <<- c('BRK.B.Close', 'BF.B.Close','T.Close' ,'CTVA.Close', 'FOXA.Close', 'FOX.Close', 'DOW.Close')

SP500_data <- stockDataDownload(ticker_list,
                                from = "2017-01-01", to = "2017-12-31",rm_stocks_with_na = FALSE)
#selection <<- read.csv("~/Desktop/MLE_r_project/Gupta_ML_Project/selection2017.csv", stringsAsFactors=FALSE)
selection <<- read.csv("~/Desktop/MLE_r_project/Gupta_ML_Project/selection1.csv", stringsAsFactors=FALSE)
count0 <<- 0
count1 <<- 0
count2 <<- 0
bt <- portfolioBacktest(list("risk parity portfolio" = risk_parity,
                             "equal weighted portfolio" = equal_weighted,
                             "tangency portfolio"    = max_sharpe_ratio),
                        list(SP500_data),
                        T_rolling_window = 20, 
                        optimize_every = 1*20, rebalance_every = 1*20)


backtestSummary(bt)$performance
summaryTable(res_sum, type = "grid.table", order_col = "Sharpe ratio", order_dir = "desc")
backtestChartCumReturns(bt)

#backtestChartStackedBar(bt, portfolio = "tangency portfolio" , legend = TRUE)


res_sum <- backtestSummary(bt)
res_sum$performance_summary 

summaryBarPlot(res_sum, measures = c("Sharpe ratio", "max drawdown"))
backtestBoxPlot(bt, measure = "Sharpe ratio")
backtestChartCumReturns(bt, c("risk parity portfolio", "tangency portfolio", "equal weighted portfolio"))
backtestChartDrawdown(bt, c("risk parity portfolio", "tangency portfolio","equal weighted portfolio"))


data("SP500_symbols")  # load the SP500 symbols
# download data from internet
SP500 <- stockDataDownload(stock_symbols = SP500_symbols, 
                           from = "2017-01-01", to = "2017-12-31")

