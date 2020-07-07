
# library -----------------------------------------------------------------

library(data.table)
library(ggplot2)

# deng e ben JIN ----------------------------------------------------------

benJin <- function(loansAll,lYear,rateYear,cpi=0,plot=F){
  lMonth = lYear*12
  rateMonth = rateYear/12
  r=cpi/12
  principal = loansAll/lMonth
  interest = rep(0,lMonth)
  for (month in c(1:lMonth)) {
    interest[month]=(loansAll-principal*(month-1))*rateMonth
  }
  rsDt=data.table(month=c(1:lMonth),
                  principal=rep(principal,lMonth),
                  interest=interest)
  rsDt$hk=rsDt$principal+rsDt$interest
  rsDt$hkCpi=0
  for (m in c(1:lMonth)) {
    rsDt$hkCpi[m]=rsDt$hk[m]/((1+r)^m)
  }
  print(paste0("等额本金，不考虑通胀总计还款:",as.character(sum(rsDt$hk))))
  print(paste0("等额本金，不考虑通胀利息:",as.character(sum(rsDt$interest))))
  print(paste0("等额本金，考虑通胀总计还款折现值:",as.character(sum(rsDt$hkCpi))))
  if (plot==T) {
    p=ggplot(rsDt,aes(x=month,y=hk))+
      geom_point()
    print(p)
  }
  return(rsDt)
}


# deng e ben XI -----------------------------------------------------------

benXi <- function(loansAll,lYear,cpi=0,rateYear){
  lMonth = lYear*12
  rateMonth = rateYear/12
  r=cpi/12
  repay=loansAll*rateMonth*((1+rateMonth)^lMonth)/((1+rateMonth)^lMonth-1)
  rsDt=data.table(repayment=rep(repay,lMonth))
  rsDt$hkCpi=0
  for (m in c(1:lMonth)) {
    rsDt$hkCpi[m]=rsDt$repay[m]/((1+r)^m)
  }
  print(paste0("等额本息，不考虑通胀总计还款:",as.character(sum(rsDt$repay))))
  print(paste0("等额本息，不考虑通胀利息:",as.character(sum(rsDt$repay)-loansAll)))
  print(paste0("等额本息，考虑通胀总计还款折现值:",as.character(sum(rsDt$hkCpi))))
  return(rsDt)
}
