stackBarSDP         <- function(shared, design, cutoff, legend.space = 1.1){
SampleData          <- read.table(shared, header = T)
Partitions          <- read.table(design, header = F)
StoolData           <- cbind(Partitions[, 2], SampleData[, - (1:3)])  #  Combine the Data Frames 
OrderData           <- StoolData[order(Partitions[, 2]), ]            # While removing extraneous Data
names(OrderData)[1] <- "Partition"
PartedData          <- rowsum(OrderData[, -1],OrderData$Partition)
SummedData          <- rowSums(PartedData)  # Total amount In Each RowParted
PercentData         <- PartedData / SummedData
PercentData[PercentData < cutoff] <- 0
Other               <- 1 - rowSums(PercentData)
SemiFinalData       <- cbind(Other, PercentData)
FinalData           <- SemiFinalData[, !(colSums(abs(SemiFinalData)) == 0)]
par(xpd = F, mar = x <- c(4, 4, 4, 6))
barplot(t(FinalData), legend = T, ylim = c(0, 1), col = rainbow(length(FinalData)),
        space = 0.1, args.legend = list(x = length(FinalData[, 1]) + legend.space))
}
