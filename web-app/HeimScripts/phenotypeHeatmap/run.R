library(reshape2)
library(limma)
library(jsonlite)
library(gtools)

## Packages tidyr and DBI must be installed!
library(tidyr)


if (!exists("remoteScriptDir")) {  #  Needed for unit-tests
    remoteScriptDir <- "web-app/HeimScripts"
}


## Loading functions ##
utils <- paste(remoteScriptDir, "/_shared_functions/Generic/utils.R", sep="")
limmaUtils <- paste(remoteScriptDir, "/_shared_functions/GEX/limmaUtils.R", sep="")
dataFrameUtils <- paste(remoteScriptDir, "/_shared_functions/GEX/DataFrameAndGEXmatrixUtils.R", sep="")
heatmapUtils <- paste(remoteScriptDir, "/_shared_functions/Clustering/heatmapUtils.R", sep="")

source(utils)
source(limmaUtils)
source(dataFrameUtils)
source(heatmapUtils)
#######################


SUBSET1REGEX <- "_s1$"  # Regex identifying columns of subset 1.
markerTableJson <- "markerSelectionTable.json" # Name of the json file with limma outputs

## Checking if a variable called preprocessed exists in R
## workspace, else loaded_variables is used to create data frame df.
## Column names in data frame get modified by replacing matrix id
## (e.g.: n0, n1, ...) by corresponding name in fetch_params list var
parseInputLd<- function() {
  
  ## Retrieving the input data frame
  if (exists("preprocessed")) {
    
    ## Retrieving low and high dim data into separate vars
    ld = preprocessed$LD
   
  } else {
    
    ld_var.idx = grep("^(column)|(row)|(numeric)", names(loaded_variables), perl = TRUE)
  
    ## Either there is low dim data available ...
    
    if(length(ld_var.idx)>0){
      ld = loaded_variables[ld_var.idx]
    ## ... or not
    } else{
      ld = NULL
    }
  }
  
  return(ld)
}

buildLowDim <- function(ld.list) {
  if(is.null(ld.list)){
    return(NULL)
  }
   
  ld.names <- unlist(names(ld.list))
  ld.namesWOSubset <- sub("_s[1-2]{1}$", "", ld.names)
  ld.fullNames <- sapply(ld.namesWOSubset, function(el) fetch_params$ontologyTerms[[el]]$fullName)
  ld.fullNames <- as.character(as.vector(ld.fullNames))
  split <- strsplit2(ld.fullNames, "\\\\")
  ld.rownames <- apply(split, 1, function(row) paste(tail(row[row != ""], n=2), collapse="//"))
  ##ld.subsets <- as.integer(sub("^.*_s", "", ld.names))
  ld.types <- sub("_.*$", "", ld.names)

  ROWNAME.vec = character(length = 0)
  PATIENTID.vec = character(length = 0)  
  VALUE.vec = character(length = 0)
  ##COLNAME.vec = character(length = 0)
  TYPE.vec = character(length = 0)
  ##SUBSET.vec = character(length = 0)
  ##ZSCORE.vec = character(length = 0)

  for (i in 1:length(ld.names)) {
      ld.var <- ld.list[[i]]
      
      ## Modified behavior for dragged folders:
      if (length(ld.var) > 2) {
      	ld.var <- unite(ld.var, collapsed, 2:length(ld.var), sep="", remove=TRUE)
      	tmp.folderName <- strsplit2(ld.rownames, "//")[i,2]
      	ld.var[2][ld.var[2] != ""] <- tmp.folderName
      }
      
      
      for (j in 1:nrow(ld.var)) {
          ld.patientID <- ld.var[j, 1]
          ld.value <- ld.var[j, 2]
          if (ld.value == "" || is.na(ld.value)) next
          ld.type <- ld.types[i]
          ##ld.subset <- ld.subsets[i]
          ld.rowname.tmp <- ld.rownames[i]
          ##ld.colname <- paste(ld.patientID, ld.rowname.tmp, paste("s", ld.subset, sep=""), sep="_")
          ld.rowname <- ld.rowname.tmp
          ROWNAME.vec <- c(ROWNAME.vec, ld.rowname)
          PATIENTID.vec <- c(PATIENTID.vec, ld.patientID)
          VALUE.vec <- c(VALUE.vec, ld.value)
          ##COLNAME.vec <- c(COLNAME.vec, ld.colname)
          TYPE.vec <- c(TYPE.vec, ld.type)
          ##SUBSET.vec <- c(SUBSET.vec, ld.subset)
      }
  }


  res.df = data.frame(PATIENTID = as.integer(PATIENTID.vec),
                      ##COLNAME = COLNAME.vec,
                      ROWNAME = ROWNAME.vec,
                      VALUE = VALUE.vec,
                      ##ZSCORE = rep(NA, length(PATIENTID.vec)),
                      TYPE = TYPE.vec
                      ##SUBSET = as.integer(SUBSET.vec), stringsAsFactors=FALSE
                      )

  # z-score computation must be executed on both cohorts, hence it happens after all the data are in res.df
  ##rownames <- unique(res.df$ROWNAME)
  ##for (rowname in rownames) {
  ##    sub.res.df <- res.df[res.df$ROWNAME == rowname, ]
  ##    if (sub.res.df[1,]$TYPE == "numeric") {
  ##        values <- as.numeric(sub.res.df$VALUE)
  ##        ZSCORE.values <- (values - mean(values)) / sd(values)
  ##        res.df[res.df$ROWNAME == rowname, ]$ZSCORE <- ZSCORE.values
  ##    }
  ##}

  return(res.df)
}

## Check input args for heatmap run.R script
verifyInput <- function(max_rows, sorting) {
  if (max_rows <= 0) {
    stop("Max rows argument needs to be higher than zero.")
  }
  if (!(sorting == "patientnumbers" || sorting == "numericvalue")) {
    stop("Unsupported sorting type. Only nodes and subjects are allowed")
  }
}

binCategory <- function(category, binObject) {
	if (binObject$active) {
		category$VALUE <- as.numeric(category$VALUE)
		category$ROWNAME <- as.character(category$ROWNAME)
		start <- binObject$start
		end <- binObject$end
		stepSize <- binObject$step
		values <- category$VALUE
		
		rowname <- category$ROWNAME[1]
		namePos <- regexpr("//", rowname)
		nameStartIdx <- namePos[length(namePos)]
		rowname <- substr(rowname, nameStartIdx+2, nchar(rowname))
		
		if (start > end) {
			temp <- end
			end <- start
			start <- temp
		}
		
		if (binObject$procentual) {
			if (start < 0 || start > 100) start <- 0
			if (end < 0 || end > 100) end <- 100
			
			saveRDS(category, file="BUH.RDS")
			
			minVal <- min(category$VALUE)
			maxVal <- max(category$VALUE)
			
			onePercent <- diff(range(minVal, maxVal))/100
			percentStep <- onePercent * stepSize
			steps <- seq(onePercent*start, onePercent*(end-percentStep), percentStep)
			
			idx <- start
			for (step in steps) {
				categoryName <- paste(rowname, " (", step, " - ", step+percentStep, "%]", sep="")
				tryCatch({category[values > step & values <= step+percentStep,]$VALUE <- categoryName},
				error=function(e){print(paste("In value range",step,"to",step+percentStep,"no value was present."))})
				idx = idx + stepSize;
			}	
		} else {
			steps <- seq(start, end-stepSize, stepSize)
			for (step in steps) {
				categoryName <- paste(rowname, " [", step, " - ", step+stepSize, ")", sep="")
				tryCatch({category[values >= step & values < step+stepSize,]$VALUE <- categoryName},
				error=function(e){print("In one value range, no value was present.")})
			}
		}
		
		## Eliminate variables out of scope:
		category <- category[suppressWarnings(is.na(as.numeric(category$VALUE))),]
	}
	return(category)
}

main <- function(max_rows = 100, sorting = "patientnumbers", ranking = "mean", selections = list(), binnedRow = {}, binnedColumn = {}) {
    max_rows <- as.numeric(max_rows)
    if (sorting == "") sorting = "patientnumbers"
    verifyInput(max_rows, sorting)
    
    ## Returns a list containing two variables named HD and LD
    ld.list <- parseInputLd()
    extraList <- buildLowDim(ld.list)
    
    column <- binCategory(subset(extraList, TYPE=="column"), binnedColumn)
    row <- binCategory(subset(extraList, TYPE=="row"), binnedRow)
    numValues <- subset(extraList, TYPE=="numeric")
    
    columnValues <- mixedsort(as.character(unique(column["VALUE"])[[1]]))
    rowValues <- mixedsort(as.character(unique(row["VALUE"])[[1]]))

	ROWNAME.vec <- character()
	COLNAME.vec <- character()
	VALUE.vec <- numeric()
	OTHERVALUE.vec <- numeric()

    for (i in 1:length(rowValues)) {
    for (j in 1:length(columnValues)) {
    	tmp.matchedPatients <- intersect(subset(row, VALUE==rowValues[i])$PATIENTID,
								subset(column, VALUE==columnValues[j])$PATIENTID)
    	tmp.patientNumber <- length(tmp.matchedPatients)
    	tmp.numericMedian <- median(as.numeric(numValues[numValues$PATIENTID %in% tmp.matchedPatients, ]$VALUE))
    	
		ROWNAME.vec <- c(ROWNAME.vec, rowValues[i])
		COLNAME.vec <- c(COLNAME.vec, columnValues[j])
		
		if (sorting == "patientnumbers") {
			VALUE.vec <- c(VALUE.vec, tmp.patientNumber)
			OTHERVALUE.vec <- c(OTHERVALUE.vec, tmp.numericMedian)
		} else {
			VALUE.vec <- c(VALUE.vec, tmp.numericMedian)
			OTHERVALUE.vec <- c(OTHERVALUE.vec, tmp.patientNumber)
		}
		
		VALUE.vec[is.na(VALUE.vec)] <- 0
		OTHERVALUE.vec[is.na(OTHERVALUE.vec)] <- 0
    }
    }
     
    fields <- data.frame(
    	"ROWNAME" = ROWNAME.vec,
		"COLNAME" = COLNAME.vec,
		"VALUE" = VALUE.vec,
		"OTHERVALUE" = OTHERVALUE.vec,
		"ZSCORE" = (VALUE.vec - mean(VALUE.vec)) / sd(VALUE.vec),
		"SUBSET" = 1
	)
    
   write.table(fields,
                "phenotypeHeatmap_data.tsv",
                sep = "\t",
                na = "",
                row.names = FALSE,
                col.names = TRUE)
    
    COEF.vec <- numeric()
   	VAR.vec <- numeric()
   	RANGE.vec <- numeric()
   	MEAN.vec <- numeric()
   	MEDIAN.vec <- numeric()

    for (i in 1:length(rowValues)) {
    	categoryValues <- subset(fields, ROWNAME==rowValues[i])$VALUE
    	
    	VAR.vec <- c(VAR.vec, var(categoryValues))
    	RANGE.vec <- c(RANGE.vec, diff(range(categoryValues)))
    	MEDIAN.vec <- c(MEDIAN.vec, median(categoryValues))
    	
    	tmp.mean <- mean(categoryValues)
    	tmp.sd <- sd(categoryValues)
    	tmp.coef <- if(tmp.mean == 0) tmp.sd/0.0001 else tmp.sd/tmp.mean
    	
    	MEAN.vec <- c(MEAN.vec, tmp.mean)
    	COEF.vec <- c(COEF.vec, tmp.coef)	
    }
    
    statValues <- data.frame(
    	"ROWNAME"= rowValues,
    	"COEF"=COEF.vec,
    	"VARIANCE"=VAR.vec,
    	"RANGE"=RANGE.vec,
    	"MEAN"=MEAN.vec,
    	"MEDIAN"=MEDIAN.vec
    )
    
    ## Get measurements values for every field
    measurements <- matrix(nrow=length(rowValues), ncol=length(columnValues))
    for (i in 1:length(rowValues)) {
    	for (j in 1:length(columnValues)) {
    		measurements[i,j] = fields$ZSCORE[(i-1)*j + j]
    	}
    }
    colnames(measurements) <- columnValues
    rownames(measurements) <- rowValues
    
    ## Get name of the numeric value
    tmp.numericName <- as.character(numValues$ROWNAME[1])
	tmp.namePos <- regexpr("//", tmp.numericName)[1]
	tmp.numericName <- substring(tmp.numericName, tmp.namePos+2)
	
	
    
    ## The returned jsn object that will be dumped to file
    jsn <- list(
        "fields"              = fields,
       ## "patientIDs"          = c("1","2","3","4"),  ## REMOVE
        "colNames"            = columnValues,
        "rowNames"            = rowValues,
        "ranking"             = ranking,
        "extraFields"         = list(), ## PLACEHOLDER
        "features"            = list(), ## PLACEHOLDER
        "maxRows"             = 100, ## PLACEHOLDER
        "allStatValues"       = statValues, ## PLACEHOLDER
        "numericName"		  = tmp.numericName,
        "warnings"            = c() # initiate empty vector
    )
    
    
    
    saveRDS(fields, file="TEST.RDS")
    jsn <- addClusteringOutput(jsn, measurements) 

    ## To keep track of the parameters selected for the execution of the code
    writeRunParams(max_rows, sorting, ranking)
  
    ## Transforming the output list to json format
    jsn <- toJSON(jsn, pretty = TRUE, digits = I(17))
    
    write(jsn, file = "phenotypeHeatmap.json")
    # json file be served the same way
    # like any other file would - get name via
    # /status call and then /download

    msgs <- c("Finished successfuly")
    list(messages = msgs)
}

