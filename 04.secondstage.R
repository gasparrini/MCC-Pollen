################################################################################
# MCC-POLLEN
################################################################################

################################################################################
# SECOND-STAGE META-ANALYSIS
################################################################################

# STORE THE RESULTS

# LOOP ACROSS POLLEN SPECIES
metalist <- lapply(seq(polltype), function(j) {
  
  # LOOP ACROSS MORTALITY CAUSES
  causelist <- lapply(seq(cause), function(k){
    
    # EXTRACT ESTIMATES FROM FIRST-STAGE MODELS
    coefall <- lapply(stage1list, "[[", j) |> lapply("[[", "causelist") |>
      lapply("[[", k) |> sapply(function(x) if(is.null(x)) NA else x$coefall)
    vcovall <- lapply(stage1list, "[[", j) |> lapply("[[", "causelist") |>
      lapply("[[", k) |> sapply(function(x) if(is.null(x)) NA else x$vcovall)
    
    # PERFORM THE META-ANALYSIS
    meta <- mixmeta(coefall, vcovall, random=~1|country/city, data=cities, 
      control=list(showiter=F))
    #summary(meta)
    
    # RETURN
    summary(meta)
  })
  
  # NAME AND RETURN
  names(causelist) <- cause
  causelist
})

# RENAME
names(metalist) <- polltype

