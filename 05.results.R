################################################################################
# MCC-POLLEN
################################################################################

################################################################################
# MAIN RESULTS
################################################################################

# CREATE AN EMPTY ARRAY
arrayres <- array(NA, dim=c(length(polltype), length(cause), 6),
  dimnames=list(polltype, cause, c("RR", "low", "high", "pval", "I2", "qtest")))

# RUN THE LOOPS TO EXTRACT THE RESULTS
for(i in seq(polltype)) {
  for(j in seq(cause)) {
    sum <- metalist[[i]][[j]]
    arrayres[i,j,] <- c(exp(sum$coef[,c(1,5,6,4)]*100), sum$i2stat, sum$qstat$pvalue)
  }
}

# 
as.data.table(arrayres) |> dcast(V1 + V2 ~ V3)
