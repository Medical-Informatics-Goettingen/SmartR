library(reshape2)
library(limma)
library(jsonlite)


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
    
    ld_var.idx = grep("^(categoric)|(numeric)", names(loaded_variables), perl = TRUE)
  
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

  PARENT.vec = character(length = 0)
  ROWNAME.vec = character(length = 0)
  PATIENTID.vec = character(length = 0)  
  VALUE.vec = character(length = 0)
  ##COLNAME.vec = character(length = 0)
  TYPE.vec = character(length = 0)
  ##SUBSET.vec = character(length = 0)
  ZSCORE.vec = character(length = 0)

  for (i in 1:length(ld.names)) {
      ld.var <- ld.list[[i]]
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
          tmpParentPos <- regexpr("//", ld.rowname)[1]
          PARENT.vec <- c(PARENT.vec, substring(ld.rowname, 1, tmpParentPos-1))
          PATIENTID.vec <- c(PATIENTID.vec, ld.patientID)
          VALUE.vec <- c(VALUE.vec, ld.value)
          ##COLNAME.vec <- c(COLNAME.vec, ld.colname)
          TYPE.vec <- c(TYPE.vec, ld.type)
          ##SUBSET.vec <- c(SUBSET.vec, ld.subset)
      }
  }


  res.df = data.frame(PATIENTID = as.integer(PATIENTID.vec),
                      ##COLNAME = COLNAME.vec,
                      PARENT = PARENT.vec,
                      ROWNAME = ROWNAME.vec,
                      VALUE = VALUE.vec,
                      ZSCORE = rep(NA, length(PATIENTID.vec)),
                      TYPE = TYPE.vec
                      ##SUBSET = as.integer(SUBSET.vec), stringsAsFactors=FALSE
                      )

  # z-score computation must be executed on both cohorts, hence it happens after all the data are in res.df
  rownames <- unique(res.df$ROWNAME)
  for (rowname in rownames) {
      sub.res.df <- res.df[res.df$ROWNAME == rowname, ]
      if (sub.res.df[1,]$TYPE == "numeric") {
          values <- as.numeric(sub.res.df$VALUE)
          ZSCORE.values <- (values - mean(values)) / sd(values)
          res.df[res.df$ROWNAME == rowname, ]$ZSCORE <- ZSCORE.values
      }
  }

  return(res.df)
}


main <- function(max_rows = 100, sorting = "nodes", ranking = "coef", selections = list(), geneCardsAllowed = FALSE) {
    print(0)
    max_rows <- as.numeric(max_rows)
    print(1)
    verifyInputHeatmap(max_rows, sorting)
    print(2)
    ## Returns a list containing two variables named HD and LD
    ld.list <- parseInputLd()
    print(3)
    ## Splitting up input into low dim and high dim vars 
    ## hd.df = data.list$HD
    ##ld.list = data.list$LD    
    print(4)
    muh <- buildLowDim(ld.list)
    print(777)
    saveRDS(muh, file="quak.rds")
    print(5)
    
    ## We need to transform the LD-Data to match our needs.
    ## Column 1: Row.Label (unique identifier)
    ## Column 2: Categoric.Marker 1
    ## Column 3-n: all Entries from Categoric Marker 2
    ## All cells in the columns of 3-n:
    ## Numeric Value for cross-over between categoric-marker 1 of row x and categoric-marker 2 of row y.
    ## Numeric Value can be: AVG(numeric_value), MEAN(numeric_value), PAT_NUMBER()
    
    categoricLists <- grep("categoric", names(ld.list), value=TRUE)
    
    lowDataDf <- data.frame(matrix(NA, ncol=1, nrow=length(ld.list[[categoricLists[1]]]$Row.Label)))
    colnames(lowDataDf) <- c("Row.Label")
    
    lowDataDf$Row.Label <- ld.list[[categoricLists[1]]]$Row.Label
    
    ## FIRST CATEGORY
    categoryName <- colnames(ld.list[[categoricLists[1]]])[2] 
    firstCategory <- substring(categoryName, 0, regexpr("\\.[^\\.]*\\.[^\\.]*$", categoryName)[1])
    
    i <- 1
    firstCategoryItems <- vector('character')
	for (list in ld.list) {
    	if (grepl(firstCategory, names(list[2]))[1]) {
    	    firstCategoryItems[[length(tmpList)+1]] = names(list[2])
    	    i <- i+1
  	  }	
	}
	
	## SECOND CATEGORY
	## We cannot use regex here: Race.Afro.American instead of Race.AfroAmerican ....
    categoryName <- colnames(ld.list[[categoricLists[i]]])[2] 
    secondCategory <- substring(categoryName, 0, regexpr("\\.[^\\.]*\\.[^\\.]*$", categoryName)[1])
    
    secondCategoryItems <- vector('character')
	for (list in ld.list) {
    	if (grepl(secondCategory, names(list[2]))[1]) {
    	    secondCategoryItems[[length(tmpList)+1]] = names(list[2])
  	  }	
	}
	
	
    
    ## For the time being we only allow the nodes sorting method
    ## if (sorting == "nodes") {
	##
 	##   } else {
 	##      colNames <- colnames(ld.list[, -c(1,2)])
  	##     subjects <- as.numeric(sub("_.+", "", colNames))
    ##    subsets <- as.numeric(substring(colNames, first=nchar(colNames), last=nchar(colNames)))
    ##   ordering <- order(as.numeric(paste(subjects, subsets, sep="")))
    ##  ld.list <- cbind(ld.list[, c(1,2)], ld.list[, -c(1,2)][, ordering])
    ##}
    
    write.table(
        ld.list,
        "phenotypeHeatmap_orig_values.tsv",
        sep = "\t",
        na = "",
        row.names = FALSE,
        col.names = TRUE
    )
    
    ## Creating the extended diff expr analysis data frame containing besides the input data,
    ## a set of statistics. The returned data frame is ranked according to provided ranking statistic
    ## hd.df          <- addStats(hd.df, ranking, max_rows)
    
    ## hd.df          <- mergeDuplicates(hd.df)
    
    ## Filtering down the hd.df to retain only the n top ranked rows
    ## ld.list          <- ld.list[1:min(max_rows, nrow(ld.list)), ]  
    
    ## if (!is.null(selections$selectedRownames) && length(selections$selectedRownames > 0)) {
    ##    ld.list <- ld.list[!ld.list$ROWNAME %in% selections$selectedRownames, ]
    ## }
    
    ## High dimensional value data frame with unpivoted data structure
    ## Providing intensity values and zscore for given patient, sample id/colname,
    ## probe id/rowname and subset
    ## fields.df      <- buildFields(hd.df)
    

    
    ## High dimensional annotation data frame with unpivoted data structure
    ## providing the information on which sample/colnames belongs to which cohort
    ## extraFieldsHighDim.df <- buildExtraFieldsHighDim(fields.df)

    
    ## Low dimensional annotation data frame  OINK OINK
    ## extraFieldsLowDim.df = buildExtraFieldsLowDim(ld.list, extraFieldsHighDim.df$COLNAME)
    

    ## ldd_rownames.vector = as.vector(unique(extraFieldsLowDim.df[, "ROWNAME"]))
    ## ldd_rownames.vector = c("Cohort", ldd_rownames.vector)
    

    
    ## rowNames reflect here the unique identifiers of the GEX matrix this means "probeID--geneSymbol"
    rowNames        <- ld.list[, 1]
    
    ## colNames should reflect here only the sample names (e.g. "67_Breast_s1")
    colNames = colnames(ld.list)[grep("^\\d+_.+_s\\d$", colnames(ld.list), perl = TRUE)]

	## OINK OINK: We need to look at the data. "significane" aka z is the numeric val.
    significanceValues <- ld.list[3][,1]
    
    
    ## A df containing the computed values for
    ## all possible statistical methods
    statistics.df = getAllStatForExtDataFrame(ld.list)

    write.table(statistics.df,
                "phenotypeHeatmap_data.tsv",
                sep = "\t",
                na = "",
                row.names = FALSE,
                col.names = TRUE)
    ## Concatenating the two extraField types (that have been generated
    ## for the low and high dim data) 
    extraFields.df = extraFieldsLowDim.df
    
    
    ## The returned jsn object that will be dumped to file
    jsn <- list(
        "fields"              = fields.df,
        "patientIDs"          = getSubject(colNames),
        "colNames"            = colNames,
        "rowNames"            = rowNames,
        "ranking"             = ranking,
        "extraFields"         = extraFields.df,
        "features"            = ldd_rownames.vector,
        "maxRows"             = max_rows,
        "allStatValues"       = statistics.df,
        "warnings"            = c() # initiate empty vector
    )
    
    ## To keep track of the parameters selected for the execution of the code
    writeRunParams(max_rows, sorting, ranking)
    
    # temporary stats like SD and MEAN need to be removed for clustering to work
    measurements.df <- cleanUp(ld.list)  

   

    ## discard rownames / probe id column
    ## in case of more samples
    if (ncol(ld.list) > 10){
        measurements.df <- measurements.df[, 2:ncol(measurements.df)]
    } else {
        ## if only one sample
        colname = colnames(ld.list)[2] 
        measurements.df <- data.frame(VALUES = ld.list[,2])
        colnames(measurements.df) = colname
        
    }

    rownames(measurements.df) = as.vector(rowNames)

    
    ## GEX intensity matrix converted to zeta scores
    ## for clustering purposes
    measurementsAsZscore.matrix <- toZscores(measurements.df)

  
    
    ## If no significanceValues are available throw a warning:
    if (all(is.na(significanceValues)))
        jsn$warnings <- append(jsn$warnings, c("Significance sorting could not be done due to insufficient data"))
    
    
    jsn <- addClusteringOutput(jsn, measurementsAsZscore.matrix) 
    

    
    ## Transforming the output list to json format
    jsn <- toJSON(jsn, pretty = TRUE, digits = I(17))
    
    
    write(jsn, file = "heatmap.json")
    # json file be served the same way
    # like any other file would - get name via
    # /status call and then /download

    msgs <- c("Finished successfuly")
    list(messages = msgs)
}




## SE: For debug purposes
#out = main(ranking = "median")



