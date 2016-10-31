####Reading All data(only 4th Q of each year from 2000-2015) and selecting sample 4 states(MI,AZ,CA,FL)


totalstat <- data.frame

alldataMI<- data.frame()
alldataAZ<- data.frame()

alldataCA<- data.frame()
alldataFL<- data.frame()


for(i in c(2000:2014)){
  for(j in c(4:4)){
    
    
    #path = paste("C:/Users/asanjari/Downloads/historical_data1_Q",j,i, sep="")
    #setwd(path)
    
    file <- paste("historical_data1_Q",j,i,".txt",sep="")
    
    data01 <- read.csv(file = file ,sep = "|",header = FALSE)
    
    dataAZ <- data01[data01$V17 =="AZ",]
    dataMI <- data01[data01$V17 =="MI",]
    dataFL <- data01[data01$V17 =="FL",]
    dataCA <- data01[data01$V17 =="CA",]
    
    
    alldataCA <- rbind(dataCA,alldataCA)
    alldataAZ <- rbind(dataAZ,alldataAZ)
    alldataMI <- rbind(dataMI,alldataMI)
    alldataFL <- rbind(dataFL,alldataFL)
    
    
    stats1<-data01$V17
    
    totalstat <- cbind(summary(stats1),totalstat)
    
  }
  
}

########### comparing the percentile statistics of credit score as the first possible indicator

summary(alldataAZ$V1)
summary(alldataMI$V1)
summary(alldataCA$V1)
summary(alldataFL$V1)


### the results shows that on each level(25th percentile,median,mean,76th percentile) FL < AZ = MI  < CA
### since AZ and MI has the same statistics, we look at them in more details 
### we plot the percentage of homebuyers who required to pay insurance, due to putting less than 20 downpayment


### We see two different pattern in these two states.



require(nlme)

sumMI <- gapply(object = alldataMI,FUN = length , groups = alldataMI$V9 > 80)
sumAZ <- gapply(object = alldataAZ,FUN = length , groups = alldataAZ$V9 > 80)


plot(sumAZ, xlab ="time",ylab = "percentage of those who had to pay insurrence")
plot(sumMI, xlab ="time",ylab = "percentage of those who had to pay insurrence")




