subgroup.check <- function(design) {
  design <- read.table(design)
  count <- 1
  warnrows <- c()
  for(i in 1:nrow(design)){
    design.tester<-design[-i,]
    for(j in 1:(nrow(design)-1)){
      if(design[i,3]==design.tester[j,3]){
        if(design[i,2]!=design.tester[j,2]){
          warnrows[count] <- i
          count <- count+1

        }
      }
    }
  }
 if(length(unique(unlist(warnrows)))>0){ 
  warning(cat("Subgroup in multiple groups in rows", unique(unlist(warnrows))))
 }
}

