library(Cairo)
library(vegan)
library(ggplot2)

main <- function(inputmode = "shannon", selectedPatientIDs = integer()) {




filtered.loaded_variables_dp <- get.loaded_variables.by.source("datapoints", loaded_variables)
if (length(filtered.loaded_variables_dp) > 0) {
    merged.df_dp <- Reduce(function(...) merge(..., by='Row.Label', all=T), filtered.loaded_variables_dp)
    merged.df_dp <- merged.df_dp[, colSums(is.na(merged.df_dp)) != nrow(merged.df_dp)] # remove NA columns

    annotations <- apply(merged.df_dp[,-1], 1, function(row) {
        row <- row[row != ""]
        paste(row, collapse="-AND-")
    })
    cat_data <- data.frame(
    patientID=as.integer(merged.df_dp$Row.Label),
    annotation=as.character(annotations)
    )
}
rownames(merged.df_dp)<-cat_data[,1]



cat_data <- data.frame(patientID=integer(), annotation=character())
filtered.loaded_variables <- get.loaded_variables.by.source("annotations", loaded_variables)
if (length(filtered.loaded_variables) > 0) {
    merged.df <- Reduce(function(...) merge(..., by='Row.Label', all=T), filtered.loaded_variables)
    merged.df <- merged.df[, colSums(is.na(merged.df)) != nrow(merged.df)] # remove NA columns

    annotations <- apply(merged.df[,-1], 1, function(row) {
        row <- row[row != ""]
        paste(row, collapse="-AND-")
    })
    cat_data <- data.frame(
    patientID=as.integer(merged.df$Row.Label),
    annotation=as.character(annotations)
    )
}

switch(inputmode,
shannon={
    mode.main="Alpha-diversity Shannon-Index"
    mode.ylab="Shannon-Index"
    div <- diversity(merged.df_dp, index = inputmode)
},
simpson={
    mode.main="Alpha-diversity Simpson-Index"
    mode.ylab="Simpson-Index"
    div <- diversity(merged.df_dp, index = inputmode)
},
invsimpson={
    mode.main="Alpha-diversity Inverse-Simpson-Index"
    mode.ylab="Inverse-Simpson-Index"
    div <- diversity(merged.df_dp, index = inputmode)
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

Metadaten_new = list()
Metadaten_new$data <- data.frame(cat_data,div,div,1) #[,2]
colnames(Metadaten_new$data)<-c("id","meta","alpha","beta","subset")
Metadaten_new$mode = inputmode
Metadaten_new$subset = 1
toJSON(Metadaten_new)
}


