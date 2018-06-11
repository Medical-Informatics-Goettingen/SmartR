library(Cairo)
library(vegan)
library(ggplot2)
library(pheatmap)
library(RColorBrewer)
library(reshape)
library(matrixStats)
library(viper)
library(limma)


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

# unused remains from heatmap workflow
markerTableJson <- ""
SUBSET1REGEX <- ""

main <- function(maxRows = 100, inputmode = "bray", selectedPatientIDs = integer(), ranking = "coef") {
#    save(loaded_variables,file="C:/tmp/loaded_variables.Rda")
#    inputmode='bray'

    fields <- 0
    features <- 0
    extraFields <- 0
    colNames <- 0
    rowNames <- 0
    fields_top25 <- 0
    annotations <- list()

    SoNoSu.labels <- names(loaded_variables)
    SoNoSu.labels <- sort(SoNoSu.labels) # for determinism
    matches <- grepl("s1", SoNoSu.labels)
    loaded_variables_s1 = loaded_variables[SoNoSu.labels[matches]]
    matches <- grepl("s2", SoNoSu.labels)
    loaded_variables_s2 = loaded_variables[SoNoSu.labels[matches]]

    lv <- list()
    if (length(loaded_variables_s1) > 0)
    lv$s1 <- loaded_variables_s1
    if (length(loaded_variables_s2) > 0)
    lv$s2 <-  loaded_variables_s2
    Metadaten_new = list()
    i <- 1
    for (i in 1:length(lv)) {
        loaded_variables_s <- lv[[i]]
        n <- paste("s",i, sep="")


        filtered.loaded_variables_dp <- get.loaded_variables.by.source("datapoints", loaded_variables_s)
        if (length(filtered.loaded_variables_dp) > 0) {
            merged.df_dp <- Reduce(function(...) merge(..., by='Row.Label', all=T), filtered.loaded_variables_dp)
            merged.df_dp <- merged.df_dp[, colSums(is.na(merged.df_dp)) != nrow(merged.df_dp)] # remove NA columns
        }

        # annotations part unused
        filtered.loaded_variables <- get.loaded_variables.by.source("annotations", loaded_variables_s)
        if (length(filtered.loaded_variables) > 0) {
            merged.df <- Reduce(function(...) merge(..., by='Row.Label', all=T), filtered.loaded_variables)
            merged.df <- merged.df[, colSums(is.na(merged.df)) != nrow(merged.df)] # remove NA columns
            
            annotations <- list()
            if (!is.null(dim(merged.df[,-1]))){
                annotations <- apply(merged.df[,-1], 1, function(row) {
                    row <- row[row != ""]
                    paste(row, collapse="-AND-")
                })
                cat_data <- data.frame(
                patientID=as.integer(merged.df$Row.Label),
                annotation=as.character(annotations)
                )
            }
            else {
                cat_data <- data.frame(
                patientID=as.integer(merged.df$Row.Label),
                annotation=as.character(merged.df[,-1]))
            }
        }
        else {
            # generate cat_data
            cat_data <- data.frame(
            patientID=as.integer( merged.df_dp$Row.Label),
            annotation=inputmode #default
            )
        }


        merged.df_dp <- data.frame(merged.df_dp[,-1], row.names=merged.df_dp[,1])



        colnames(merged.df_dp) <- sapply(strsplit(colnames(merged.df_dp),"\\."),tail,1)
        merged.df_dp <- merged.df_dp[,order(names(merged.df_dp))]


        #########################################
        ##PCA
        ###############################################
        input <- merged.df_dp
        Microbiom_table <- merged.df_dp
        Microbiom_table_hell <- decostand(Microbiom_table,method="hellinger")
        pca <- prcomp(Microbiom_table_hell)
        pred2 <- predict(pca)

        Metadaten <- data.frame(input[,1],input[,3])
        colnames(Metadaten) <- c("samples", "meta")

        div <- pred2[, 1:2]

        Metadaten_new$data[[n]]$PCA <- data.frame(div, cat_data)
        Metadaten_new$data[[n]]$PCA_meta <- cat_data

        #############################################

        #Distanz-Matrix wird berechnet
        input <- merged.df_dp
        Metadaten <- merged.df
        distance<-vegdist(Microbiom_table, method=inputmode)
        Dist <- as.matrix(distance)

        Ausgabe <- data.frame(rownames(Dist), Dist)
        colnames(Ausgabe) <- c("ID",rownames(Dist))

        #Heatmap wird ausgerechnet, wenn 100 oder weniger Probanden im Datensatz vorkommen
        if(length(input[,1])<=100)
        {
            if(dim(input)[2]>2)
            {
                #Top 25

                Mean <- apply(Microbiom_table,2,mean)
                Mean_sort <- sort(Mean, decreasing=TRUE)

                Mean_top25 <- Mean_sort[1:25]

                Microbiom_table_top25 <- Microbiom_table[,names(Mean_top25)]

            }else
            {
                #Top 25

                Mean <- apply(Microbiom_table,2,mean)
                Mean_sort <- sort(Mean, decreasing=TRUE)

                Mean_top25 <- Mean_sort[1:25]

                Microbiom_table_top25 <- Microbiom_table[,names(Mean_top25)]
            }
            #############################################
            dist_df <- as.data.frame(Dist)
            Metadaten_new$data[[n]]$Heatmap <- Dist#, cat_data)



            patients <- as.character(row.names(Microbiom_table_top25))
            if (i == 1){
                clusterData <- Dist
            }
            else{
                clusterData <- rbind(clusterData,Dist)
            }   
            # Dist <- as.matrix(distance) 
            Dist <- melt(Dist)
            colnames(Dist) <- c("ROWNAME", "COLNAME", "VALUE")
            Dist["SUBSET"] <- i
            Dist["PATIENTID"] <- patients
            Dist["ZSCORE"] <- Dist$VALUE


            Microbiom_table_top25 <- melt(as.matrix(Microbiom_table_top25))
            colnames(Microbiom_table_top25) <- c("ROWNAME", "COLNAME", "VALUE")
            Microbiom_table_top25["PATIENTID"] <- patients
            Microbiom_table_top25["SUBSET"] <- i
            Microbiom_table_top25["ZSCORE"] <- Microbiom_table_top25$VALUE

            Metadaten_new$data[[n]]$Heatmap_TOP25 <- as.data.frame(Microbiom_table_top25)
            Metadaten_new$data[[n]]$Heatmap_patients <- patients
            Metadaten_new$data[[n]]$annotation = annotations
            Metadaten_new$subset = c(Metadaten_new$subset,i)
            Metadaten_new$data[[n]]$Heatmap <- (Dist)#, cat_data)
            ef = data.frame(patients,patients,annotations,annotations,i,0)#TODO: type still missing
            if (i == 1){
                heatmap <- Dist
                heatmap_top25 <- as.data.frame(Microbiom_table_top25)
                extraFields = ef
            }
            else{
                heatmap <- rbind(heatmap,Dist)
                heatmap_top25 <- rbind(heatmap_top25,as.data.frame(Microbiom_table_top25))
                extraFields = rbind(extraFields,ef)
            }	    
        }
    }

    statData = clusterData
    rownames(statData) = patients
    colnames(statData) = patients


    for (i in 1:length(statData)) {
        #calculating some random statistics -> TODO: what needed?
        
        ttest <- t.test(statData[,i])
        stats = data.frame(patients[i],mean(statData[,i], na.rm = T),sd(statData[,i], na.rm = T),ttest$statistic,ttest$p.value)
        colnames(stats) = c("ROWNAME","MEAN","SD","SIGNIFICANCE","PVAL")

        if(i == 1){
            Metadaten_new$allStatValues <- stats
        } else {
            Metadaten_new$allStatValues <- rbind(Metadaten_new$allStatValues, stats)
        }
    }

    extraFields <- buildExtraFieldsLowDimBetaDiv(filtered.loaded_variables,patients)

    extraFields$COLNAME <- extraFields$PATIENTID

    Metadaten_new$mode = inputmode
    Metadaten_new$fields = heatmap
#    Metadaten_new$features = unique(annotations)
    Metadaten_new$features = unique(extraFields$ROWNAME)
    Metadaten_new$extraFields = extraFields
    Metadaten_new$colNames = patients
    Metadaten_new$rowNames = patients
    Metadaten_new$fields_top25 = heatmap_top25
    Metadaten_new <- addClusteringOutput(Metadaten_new, clusterData)
    Metadaten_new$loaded_variables = filtered.loaded_variables

    toJSON(Metadaten_new,pretty = TRUE,rownames = FALSE)


}


buildExtraFieldsLowDimBetaDiv <- function(ld.list, colnames) {
  if(is.null(ld.list)){
    return(NULL)
  }
  
  ld.names <- unlist(names(ld.list))
  ld.namesWOSubset <- sub("_s[1-2]{1}$", "", ld.names)
  ld.fullNames <- sapply(ld.namesWOSubset, function(el) fetch_params$ontologyTerms[[el]]$fullName)
  ld.fullNames <- as.character(as.vector(ld.fullNames))
  split <- strsplit2(ld.fullNames, "\\\\")
  ld.rownames <- apply(split, 1, function(row) paste(tail(row[row != ""], n=2), collapse="//"))
  ld.subsets <- as.integer(sub("^.*_s", "", ld.names))
  ld.types <- sub("_.*$", "", ld.names)

  hd.patientIDs <- colnames
  hd.subsets <- as.integer(substr(colnames, nchar(colnames), nchar(colnames)))
  split <- sub(".+?_", "", colnames)
  hd.labels <- substr(split, 1, nchar(split) - 3)

  ROWNAME.vec = character(length = 0)
  PATIENTID.vec = character(length = 0)  
  VALUE.vec = character(length = 0)
  COLNAME.vec = character(length = 0)
  TYPE.vec = character(length = 0)
  SUBSET.vec = character(length = 0)
  ZSCORE.vec = character(length = 0)

  for (i in 1:length(ld.names)) {
      ld.var <- ld.list[[i]]
      for (j in 1:nrow(ld.var)) {
          ld.patientID <- ld.var[j, 1]
          ld.value <- ld.var[j, 2]
          if (ld.value == "" || is.na(ld.value)) next
          ld.type <- ld.types[i]
          ld.subset <- ld.subsets[i]
          ld.rowname.tmp <- ld.rownames[i]
          ld.colname <- ld.patientID
          if (! ld.colname %in% colnames) {
              for (k in which(ld.patientID == hd.patientIDs & ld.subset == hd.subsets)) {
                  ld.colname <- hd.patientIDs[k]
                  ld.rowname <- paste("(matched by subject)", ld.rowname.tmp)
                  ROWNAME.vec <- c(ROWNAME.vec, ld.rowname)
                  PATIENTID.vec <- c(PATIENTID.vec, ld.patientID)
                  VALUE.vec <- c(VALUE.vec, ld.value)
                  COLNAME.vec <- c(COLNAME.vec, ld.colname)
                  TYPE.vec <- c(TYPE.vec, ld.type)
                  SUBSET.vec <- c(SUBSET.vec, ld.subset)
              }
          } else {
              ld.rowname <- paste("(matched by sample)", ld.rowname.tmp)
              ROWNAME.vec <- c(ROWNAME.vec, ld.rowname)
              PATIENTID.vec <- c(PATIENTID.vec, ld.patientID)
              VALUE.vec <- c(VALUE.vec, ld.value)
              COLNAME.vec <- c(COLNAME.vec, ld.colname)
              TYPE.vec <- c(TYPE.vec, ld.type)
              SUBSET.vec <- c(SUBSET.vec, ld.subset)
          }
      }
  }


  res.df = data.frame(PATIENTID = as.integer(PATIENTID.vec),
                      COLNAME = COLNAME.vec,
                      ROWNAME = ROWNAME.vec,
                      VALUE = VALUE.vec,
                      ZSCORE = rep(NA, length(PATIENTID.vec)),
                      TYPE = TYPE.vec,
                      SUBSET = as.integer(SUBSET.vec), stringsAsFactors=FALSE)

  # z-score computation must be executed on both cohorts, hence it happens after all the data are in res.df
  rownames <- unique(res.df$ROWNAME)
  for (rowname in rownames) {
      sub.res.df <- res.df[res.df$ROWNAME == rowname, ]
      if (sub.res.df[1,]$TYPE == "annotationsNumeric") {
          values <- as.numeric(sub.res.df$VALUE)
          ZSCORE.values <- (values - mean(values)) / sd(values)
          res.df[res.df$ROWNAME == rowname, ]$ZSCORE <- ZSCORE.values
      }
  }

  return(res.df)
}

