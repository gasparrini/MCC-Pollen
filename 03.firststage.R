################################################################################
# MCC-POLLEN
################################################################################

################################################################################
# FIRST-STAGE ANALYSIS: RUN THE MODEL IN EACH CITY, REDUCE AND SAVE
################################################################################

################################################################################
# RUN THE LOOP

# PRINT WARNINGS IMMEDIATELY (TO DETECT ISSUES)
options(warn=1)

# PREPARE THE PARALLELIZATION (REGISTER CLUSTERS AND WRITE TRACE FILE)
cl <- parallel::makeCluster(max(1,ncores-2))
registerDoParallel(cl)
writeLines(c(""), "temp/logstage1.txt")
cat(as.character(as.POSIXct(Sys.time())),file="temp/logstage1.txt",append=T)

# LOOP ACROSS CITIES
stage1list <- foreach(data=dlist, i=seq(dlist), .packages=pack) %dopar% {
  
  # STORE ITERATION (1 EVERY 20)
  if(i%%20==0) cat("\n", "iter=",i, as.character(Sys.time()), "\n",
    file="temp/logstage1.txt", append=T)
  
  # DEFINE CROSSBASIS FOR TEMPERATURE
  knotstmean <- quantile(data$tmean, c(10,75,90)/100, na.rm=T)
  argvartmean <- list(fun="bs", knots=knotstmean, degree=2)
  cbtmean <- crossbasis(data$tmean, lag=lagtmean, argvar=argvartmean,
    arglag=arglagtmean)
  
  # DEFINE THE SPLINE OF TIME
  spltime <- ns(data$date, df=round(dftime*nrow(data)/365.25))
  
  # LOOP ACROSS POLLEN SPECIES
  reslist <- lapply(seq(polltype), function(j) {
    
    # DEFINE CROSSBASIS FOR POLLEN
    cbpollen <- crossbasis(data[[polltype[j]]], lag=lagpollen, 
      argvar=list("lin"), arglag=arglagpollen)
    
    # DEFINE ALL-CAUSE AS NON-EXTERNAL IF NOT AVAILABLE OUTCOME
    if(!"all"%in%names(data)) data$all <- as.integer(data$nonext)
    
    # LOOP ACROSS MORTALITY CAUSES
    causelist <- lapply(seq(cause), function(k){
      
      # CHECK THE AVAILABILITY OF THE CAUSE-SPECIFIC DATA
      if(!cause[k] %in% names(data)) return(NULL)
      
      # DEFINE THE OUTCOME
      data$y <- data[[cause[k]]]
      
      # RUN THE MODEL (RE-CREATE FORMULA TO AVOID ISSUE WITH ENVIRONMENTS)
      mod <- glm(formula(deparse(fmod)), data, family=quasipoisson)
      
      # EXTRACT THE ESTIMATES
      cr <- crossreduce(cbpollen, mod, cen=0)
      coefall <- coef(cr)
      vcovall <- vcov(cr)
      
      # STORE
      list(coefall=coefall, vcovall=vcovall, conv=mod$converged, qaic=QAIC(mod))
    })

    # NAMES
    names(causelist) <- cause
    
    # STORE
    list(causelist=causelist, sumpollen=quantile(data[[polltype[j]]], 
      0:100/100, na.rm=T))
  })
  
  # NAME AND RETURN
  names(reslist) <- polltype
  reslist
}
names(stage1list) <- cities$city

# REMOVE PARALLELIZATION
stopCluster(cl)
#file.remove("temp/logstage1.txt")

# SAVE
save.image(file="temp/firststage.RData")
