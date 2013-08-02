subgroup.check <- function(file) {
  count <- 1
  warnrows <- c()
  for(i in 1:nrow(file)){
    file.tester<-file[-i,]
    for(j in 1:(nrow(file.tester))){
      if(file[i,3]==file.tester[j,3]){
        if(file[i,2]!=file.tester[j,2]){
          warnrows[count] <- i
          count <- count+1
        }
      }
    }
  }
 if(length(unique(unlist(warnrows)))>0){ 
  warning(cat("Subgroup in multiple group in rows", unique(unlist(warnrows))))
 }
}
