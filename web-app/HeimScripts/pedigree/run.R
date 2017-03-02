library(jsonlite)
library(kinship2)
library(Cairo)
library(png)

main <- function(selectedPatientIDs = integer()) {

save(loaded_variables, file="/tmp/loaded_variables.Rda")




plot_to_bytearray <- function(plot_data) {


}

mergePEDFile <- function(loaded_variables,label){
filtered <- get.loaded_variables.by.name(label, loaded_variables)
if (length(filtered) > 0) {
merged <- Reduce(function(...) merge(..., by='Row.Label', all=T), filtered)
merged <- merged[, colSums(is.na(merged)) != nrow(merged)] # remove NA columns

annotations <- apply(merged[,-1], 1, function(row) {
row <- row[row != ""]
paste(row, collapse="-AND-")
})
cat_data <- data.frame(
patientID=as.integer(merged$Row.Label),
annotation=as.character(annotations)
)
}
rownames(merged)<-cat_data[,1]
merged[ merged == "" ] = NA
merged
}


get.loaded_variables.by.name <- function(sourceLabel, loaded_variables) {
SoNoSu.labels <- names(loaded_variables)
j <- 1
result <- list()
for (i in loaded_variables)
{
# i
matches <- grepl(paste("*", sourceLabel, "*", sep=""),  colnames(i))
if (matches[2]){
result[[SoNoSu.labels[j]]] <- i
}
j <- j+1
}
result
}

json <- list()

mother<- mergePEDFile(loaded_variables,"Mother")
father<- mergePEDFile(loaded_variables,"Father")
family <- mergePEDFile(loaded_variables, "Family")
sex <- mergePEDFile(loaded_variables, "Sex")
id <- mergePEDFile(loaded_variables, "ID")
# FIXME
sex <- sex[1:3]


ped <- cbind.data.frame(id[1],
ID = c(na.omit(c(t(id[, -1])))),
Mother = c(na.omit(c(t(mother[, -1])))),
Father = c(na.omit(c(t(father[, -1])))),
Family = c(na.omit(c(t(family[, -1])))),
Sex = c(na.omit(c(t(sex[, -1]))))

)

pedAll <- pedigree(
id=ped$ID,
dadid=ped$Father,
momid=ped$Mother,
sex=ped$Sex,
famid=ped$Family)

test <- list()
plots <- list()
for (i in unique(pedAll$famid)){
    ped1basic <- pedAll[i]
#CairoPNG(file=paste("Bild",i,".png",sep=""), width=800, height=400,units = "px")
#plot(ped1basic, main = "First Family")
#dev.off()


Cairo(file=paste("Bild",i,".png",sep=""), width=800, height=400)
#Cairo(file=paste("Bild",i,".png",sep=""))

## Plot your data here. Plot should be the last command
plot(ped1basic, main = "First Family")
#dev.off()
## Transform to bytes, do not change
plot.img <- Cairo:::.image(dev.cur())	## Get image currently in dev.cur, your plot
plot.raw <- Cairo:::.ptr.to.raw(plot.img$ref, 0, plot.img$width*plot.img$height*4)	## Get raw data of the image
dim(plot.raw) = c(4, plot.img$width, plot.img$height)	## We need four dimensions - RGBA
plot.raw[c(1,3),,] = plot.raw[c(3,1),,]		## Red and blue are in wrong order and have to be swapped
#dev.off()
## return the raw data



plots[[i]] <- writePNG(plot.raw, raw())
}

data <- get.loaded_variables.by.source("datapoints", loaded_variables)

json$plots <- plots;
json$ped <- ped;
json$data <- data;



exportJson <- toJSON(json)
write(exportJson, "/tmp/test.json")
exportJson
}


