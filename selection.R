##change: time interval to change the selection
##num: number of stocks in a selection
##period: time interval to look back
selection <- function(retdata, change, num, period){
  
  selection.list = data.frame(row.names=1:num)

  for (i in seq(61,1260,change)){
    mydata = retdata[i:(i+period),]
    sort=data.frame(name=character(0), alpha=numeric(0), stringsAsFactors=FALSE)
    for (j in 1:505){
      if (!is.na(retdata[i,j])){
        lm = lm((mydata[,j]-rf)~mkt.rf+SMB+HML, data=mydata)
      }
      sort[nrow(sort)+1,1] = names(return)[j]
      sort[nrow(sort),2]=lm$coefficients[1]
    }
    sort = sort[order(sort$alpha),]
    sort = sort[1:num,]
    selection.list[row.names(return)[i]] = sort[,1]
  }
  return(selection.list)
}

