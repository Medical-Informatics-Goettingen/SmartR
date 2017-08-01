library(Cairo)
library(vegan)
library(ggplot2)

main <- function(inputmode = "shannon", selectedPatientIDs = integer()) {

save(loaded_variables,file="C:/tmp/loaded_variables.Rda")

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

        switch(inputmode,
        shannon={
            mode.main="Alpha-diversity Shannon-Index"
            mode.ylab="Shannon-Index"
            div <- diversity(merged.df_dp, index = "shannon")
        },
        simpson={
            mode.main="Alpha-diversity Simpson-Index"
            mode.ylab="Simpson-Index"
            div <- diversity(merged.df_dp, index = "simpson")
        },
        invsimpson={
            mode.main="Alpha-diversity Inverse-Simpson-Index"
            mode.ylab="Inverse-Simpson-Index"
            div <- diversity(merged.df_dp, index = "invsimpson")
        },
        chao1={
            mode.main="Alpha-diversity Chao1-Estimator"
            mode.ylab="Chao1-Estimator"
            div <- estimateR(merged.df_dp)[2,]
        },
        obs={
            mode.main="Observed Species"
            mode.ylab="Observed Species"
            div <- estimateR(merged.df_dp)[1,]
        },
        ACE={
            mode.main="Alpha-diversity ACE-Estimator"
            mode.ylab="ACE-Estimator"
            div <- estimateR(merged.df_dp)[4,]
        },
        {
            mode.main="Alpha-diversity Shannon-Index"
            mode.ylab="Shannon-Index"
            div <- diversity(merged.df_dp, index = "shannon")
        })


        Metadaten_new$data[[n]] <- data.frame(cat_data,div,div,"1") #[,2]
        colnames(Metadaten_new$data[[n]])<-c("id","meta","alpha","beta","subset")
        Metadaten_new$subset = c(Metadaten_new$subset,i)

    }
    Metadaten_new$mode = inputmode
    toJSON(Metadaten_new)
}


