StackBar.SDP <- function(x,y,z){
SampleData <- read.table(x,header=T)
Partitions <- read.table(y,header=F)
StoolData <- cbind(Partitions[,2],SampleData[,-(1:3)])
OrderData <- StoolData[order(Partitions[,2]),]
names(OrderData)[1] <- "Partition"
PartedData <-rowsum(OrderData[,-1],OrderData$Partition)
SummedData <-rowSums(PartedData)#Total Bacteria In Each RowPartedD
PercentData <- PartedData/SummedData
PercentData[PercentData < z] <- 0
Other <- 1-rowSums(PercentData)
SemiFinalData <- cbind(PercentData,Other)
FinalData <- SemiFinalData[,!(colSums(abs(SemiFinalData)) == 0)]
par(xpd=F,mar=c(4,4,4,6))
Plot <- barplot(t(FinalData),legend=T,ylim=c(0,1),col=rainbow(length(FinalData)),space=0.1,args.legend = list(x=length(FinalData[,1])+1.1))
}