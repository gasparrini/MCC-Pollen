################################################################################
# MCC-POLLEN
################################################################################

################################################################################
# PREPARE THE DATA
################################################################################

# LOAD THE MCC DATASET
load("V:/VolumeQ/AGteam/MCCdata/data/MCC_all/MCCdata_20221216.RData")

################################################################################
# SELECTION OF COUNTRIES IN EUROPE

# RESTRICT TO EUROPE
ind <- grepl("Europe", cities$Region)
cities <- cities[ind,]
dlist <- dlist[ind]

# SELECT COUNTRIES (DEPENDEND ON POLLEN DATA AVAILABILITY, SEE LATER)
selcnt <- c("cze9415","est9718","fnl9414","fra0015","ger9315","grc0110","irl8407",
  "ita0615","mld0110","net9516c","nor6918","por8018","rom9416","spa9014",
  "sui9513","swe9016","uk9020")
ind <- cities$country %in% selcnt
cities <- cities[ind,]
dlist <- dlist[ind]

################################################################################
# POLLEN DATA

# POLLEN TYPES
polltype <- c("alder","birch","mugwort","olive")

# PATHS AND FILE
dir <- "V:/VolumeQ/AGteam/FMI"
exdir <- "C:/Users/emsuagas/Downloads/"
files <- paste0(polltype, ".zip")

# RETRIEVE CITY NAMES CONSISTENT WITH MCC
lstfiles <- lapply(seq(polltype), function(j) 
  unzip(zipfile=paste(dir, files[j], sep="/"), list=T)$Name)

# RETRIEVE CITY NAMES CONSISTENT WITH MCC FROM FIRST LIST
lookup <- lstfiles[[1]] |> strsplit("_", fixed=T) |> 
  sapply(function(x) paste(x[-1], collapse="_")) |> 
  strsplit("_ALDER.txt", fixed=T) |> unlist() |> sub("_", ".", x=_)

# SELECT CITIES
ind <- cities$city %in% lookup
cities <- cities[ind,]
dlist <- dlist[ind]

# PREPARE THE PARALLELIZATION (REGISTER CLUSTERS AND WRITE TRACE FILE)
ncores <- detectCores()
pack <- c("data.table")
cl <- parallel::makeCluster(max(1,ncores-2))
registerDoParallel(cl)
writeLines(c(""), "temp/prepdata.txt")
cat(as.character(as.POSIXct(Sys.time())),file="temp/prepdata.txt",append=T)

# MERGE POLLEN DATA
dlist <- foreach(data=dlist, i=seq(dlist), .packages=pack) %dopar% {
  
  # STORE ITERATION (1 EVERY 20)
  if(i%%20==0) cat("\n", "iter=",i, as.character(Sys.time()), "\n",
    file="temp/prepdata.txt", append=T)

  # SELECT CITY
  city <- cities$city[i]

  # LOOP ACROSS POLLEN SPECIES
  dplist <- lapply(seq(polltype), function(j) {
    
    # EXTRACT THE FILE AND READ IT
    filecity <- lstfiles[[j]][which(lookup %in% city)]
    unzip(zipfile=paste(dir, files[j], sep="/"), files=filecity, exdir=exdir) 
    dpoll <- fread(paste(exdir, filecity, sep="/"), skip=4, 
      col.names=c("year","month","day","time","type",polltype[j]))
    file.remove(paste(exdir, filecity, sep="/"))
    
    # AVERAGE BY DATE
    dpoll[, date:=as.Date(paste(year,month,day,sep="-"))]
    dpoll <- dpoll[, lapply(.SD, mean, na.rm=T), by=date, .SDcol=polltype[j]]
    
    # RETURN
    dpoll
  })
  
  # MERGE WITH MAIN DATA AND RETURN
  data <- merge(as.data.table(data), Reduce(merge, dplist))
  data
}
names(dlist) <- cities$city

# REMOVE PARALLELIZATION
stopCluster(cl)
#file.remove("temp/prepdata.txt")

################################################################################
# CLEAN AND SAVE

# CLEAN
rm(list=setdiff(ls(), c("dlist","cities","polltype")))

# SAVE
save.image(file="temp/data.RData")

