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
    saveRDS(ld.list, file="ldList.rds")
    print(5)
    if (sorting == "nodes") {

    } else {
        colNames <- colnames(ld.list[, -c(1,2)])
        subjects <- as.numeric(sub("_.+", "", colNames))
        subsets <- as.numeric(substring(colNames, first=nchar(colNames), last=nchar(colNames)))
        ordering <- order(as.numeric(paste(subjects, subsets, sep="")))
        ld.list <- cbind(ld.list[, c(1,2)], ld.list[, -c(1,2)][, ordering])
    }
    
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



