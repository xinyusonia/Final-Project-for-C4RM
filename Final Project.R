Inflation <- read.csv("https://github.com/xinyusonia/Final-Project-for-C4RM/blob/main/InflationRate.csv")
library(ggplot2)
library(MASS)
library(reshape2)

RedFlags <- function(df, target, category){
  categories    = df[,category]
  uniques       = unique(categories)
  
  bigenough     = function(u) return(mean(categories==u)>0.01)
  columnValue   = uniques[sapply(uniques, bigenough)]
  
  maxFor    = function(val) return(max(df[categories==val,target]))
  maxTarget = sapply(columnValue, maxFor)
  
  columnName    = rep(category, length(columnValue))
  newdf         = data.frame(columnName = columnName,
                             columnValue = columnValue,
                             maxTarget = maxTarget)
  return(newdf)
}

RedFlags(Inflation, 'OverallInflation', 'UnemploymentRate')

RedFlagOverview <- function(df, target){
  newdf = data.frame(columnName = c(), columnValue = c(), maxTarget = c())
  for(name in names(df)[names(df) != target])
    newdf = rbind(newdf, RedFlags(df, target, name))
  worst = order(newdf$maxTarget)[1:10]
  sorted = newdf[worst,]
  row.names(sorted) = 1:nrow(sorted)
  sorted$maxTarget = round(sorted$maxTarget, 3)
  return(sorted)
}

RedFlagOverview(Inflation, 'OverallInflation')

library('scales')

PortfolioPercentageChange = function(df){
  ncol = ncol(df)
  nrow = nrow(df)
  for (col in 2:ncol){
    for (row in nrow:2) {
      df[row,col] = (df[row,col] - df[row-1,col]) / df[row-1,col]
    }
  }
  df = df[2:nrow, 1:ncol]
  newcol = ncol(df)
  newrow = nrow(df)
  for (col in 2:newcol){
    for (row in 1:nrow) {
        df[row,col] = percent(as.numeric(df[row,col]))
    }
  }
  return(df)
}

PortfolioPercentageChange(Inflation)

# All data in one plot
mydata <- melt(Inflation,id="Date")
colnames(mydata) <- c("Date","Inflation","value")
ggplot(data = mydata,aes(x=Date,
                         y=value,
                         group = Inflation,
                         color=Inflation,
                         shape=Inflation))+geom_point()+geom_line()+ 
  theme(axis.text.x = element_text(angle = 90))

# Plot for Overall Inflation & Unemployment Rate
Inflation2<-subset(Inflation, select=c("Date", "OverallInflation","UnemploymentRate"))
mydata2 <- melt(Inflation2,id="Date")
colnames(mydata2) <- c("Date","Inflation","value")
ggplot(data = mydata2,aes(x=Date,
                         y=value,
                         group = Inflation,
                         color=Inflation,
                         shape=Inflation))+geom_point()+geom_line()+ 
  theme(axis.text.x = element_text(angle = 90))

# Plot for Overall Inflation & Federal Funds Effective Rate
Inflation3<-subset(Inflation, select=c("Date", "OverallInflation","FederalFundsEffectiveRate"))
mydata3 <- melt(Inflation3,id="Date")
colnames(mydata3) <- c("Date","Inflation","value")
ggplot(data = mydata3,aes(x=Date,
                          y=value,
                          group = Inflation,
                          color=Inflation,
                          shape=Inflation))+geom_point()+geom_line()+ 
  theme(axis.text.x = element_text(angle = 90))