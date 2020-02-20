library(glmnet)

res <- cor(whole_fct2[,-c(1,2)])
high_cor_idx <- c(4,5,13,16,17,18,24,25,26,30,33,38,39)
whole_fct3_redcor = whole_fct2[,-high_cor_idx]
res <- cor(whole_fct3_redcor[,-c(1,2)])
high_cor_idx <- c(5,6,25,26)
whole_fct4_redcor = whole_fct3_redcor[,-high_cor_idx]

training_set4 = subset(whole_fct4_redcor,Dates<="2016-12-31")
x = model.matrix(training_set4$dailyReturn~.,training_set4)[,-c(1,2)]
y = training_set4$dailyReturn

grid = 10^seq(10,-2,length=100)
set.seed(1)
train = sample(1:nrow(x),nrow(x)/2)
test = -train
y.text = y[test]

lasso.mod = glmnet(x[train,], y[train], alpha=1, lambda=grid)
cv.out =  cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out)

bestlam = cv.out$lambda.min
lasso.pred = predict(lasso.mod, s = bestlam, newx = x[test,])
out = glmnet(x,y,alpha = 1)
lasso.coef = predict(out, type="coefficients", s=bestlam) 
lasso.coef


for(str in name_idx)
merge(stock_fct3[[str]], dailyreturn[c("Dates",str,"mkt.rf","SMB","HML")])

for (i in c(1:396)){str = names(stock_fct3)[i]stock_fct4[[I]] = merge(stock_fct3[[i]], dailyreturn[c("Dates",str,"mkt.rf","SMB","HML")])}


choose_stock <- function(d1,d2,n){
value = c()
for (i in 1:length(stock_fct4)){
data = subset(stock_fct4[[i]][,c("Dates",names(regcoef))],d1<=Dates& Dates<=d2)
data = data[,-1] 
for (j in 1:length(data)){
data[j] = scale(data[j])
}
value[i] = sum(data*regcoef)
}
choose = order(value)[1:n]
choose_name = names(stock_fct4)[choose]
return(choose_name)
}

output_stock_selection <-function(d){
date_list <- data.frame("Dates" = stock_fct4[[1]][,"Dates"])
date_list = subset(date_list,"2017-01-01"<=Dates& Dates<="2017-12-31")
selection = data.frame()
rebalance_times = nrow(date_list)%/%d
rebalance_series = c()
for (i in 1:rebalance_times){
rebalance_date = date_list$Dates[i*d+1]
last_date = date_list$Dates[(i-1)*d+1]
rebalance_series[i] <- as.character(rebalance_date)
choose_name = choose_stock(last_date, rebalance_date,d)
temp = data.frame("rebalance_date" = choose_name)
selection<-cbind(selection, temp)}
selection <-selection[,-1]
names(selection) <-rebalance_series
return selection
