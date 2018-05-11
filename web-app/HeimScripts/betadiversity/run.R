library(Cairo)
library(vegan)
library(ggplot2)
library(pheatmap)
library(RColorBrewer)
library(reshape)

main <- function(inputmode = "bray", selectedPatientIDs = integer()) {
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

        filtered.loaded_variables <- get.loaded_variables.by.source("annotations", loaded_variables_s)
        if (length(filtered.loaded_variables) > 0) {
            merged.df <- Reduce(function(...) merge(..., by='Row.Label', all=T), filtered.loaded_variables)
            merged.df <- merged.df[, colSums(is.na(merged.df)) != nrow(merged.df)] # remove NA columns
            #
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
            ef = data.frame(patients,patients,patients,annotations,i,0)
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

    colnames(extraFields) = c("PATIENTID","COLNAME","ROWNAME","VALUE","SUBSET","ZSCORE")
    Metadaten_new$mode = inputmode
    Metadaten_new$fields = heatmap
    Metadaten_new$features = annotations
    Metadaten_new$extraFields = extraFields
    Metadaten_new$colNames = patients
    Metadaten_new$rowNames = patients
    Metadaten_new$fields_top25 = heatmap_top25

    toJSON(Metadaten_new,pretty = TRUE)


}


