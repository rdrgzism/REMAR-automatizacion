#!/usr/bin/env Rscript

#---------------------------------------
# Estimating fishing effort for small-scale fisheries from vessel monitoring system
#   3/5
#---------------------------------------

# Ismael Rodriguez
# 02/05/2025

# Identify and discriminate small-scale fishing gear and assign the most probable 
# gear type used for each fishing trip based on the recorded catches.
# INPUT:
# (1) OUT.RData: Contains the weight's matrix of all recorded catches for a 
# given time window.
# (2) expertsREMAR_v0.RData: Contains the classified fishing trips by experts. 

# OUTPUT (saved as OUT.RData):
# (1) 

#----------------
# 1: data loading
#----------------
library(PCDimension)
library(mldr)
library(RWeka)

remove(list=ls())

# --- Folder paths
rdata_dir <- "../../data/rdata"
reference_dir <- "../../data/reference"


load(file.path(rdata_dir,"OUT.RData"))
load(file.path(rdata_dir,"expertsREMAR_v0.RData"))

# Matching species names
which(!species_list %in% colnames(response))
# [1] 5 -> "ALMEJA FINA"
sp_names <- make.names(species_list,unique = TRUE) 
colnames(response) <- species_list[-5] 
OUT <- OUT[,-5]
colnames(OUT) <- colnames(response)

#----------------
# 2: preliminary data exploration 
#----------------
OUT <- data.frame(OUT)
metiere <- data.frame(metiere)

# remove columns (species) with no landings & species not common to both data sets
response_clean <- response[, colSums(response) != 0]
OUT_clean <- OUT[, colSums(OUT) != 0]
common_cols <- intersect(colnames(response_clean), colnames(OUT_clean))
response <- response_clean[, common_cols]
OUT <- OUT_clean[, common_cols]

# pca (all metieres)
pca <- prcomp(log(1+response),scale=T)

#----------------
# 3: IDENTIFYING AND REMOVING ARRASTRE
#----------------
# reducing dimensionality
n.axes <- 25 # it should be bsDimension(var) but I have settinf it by trial-and-error
sco <- scale(log(1+response), pca$center, pca$scale) %*% pca$rotation
sco <- sco[,1:n.axes]
sco_OUT <- scale(log(1+OUT), pca$center, pca$scale) %*% pca$rotation
sco_OUT <- sco_OUT[,1:n.axes]

# classifier
input <- data.frame(sco,metiere)
sco_OUT <- data.frame(sco_OUT)
labelIndices <- (dim(sco)[2]+1):(dim(sco)[2]+dim(metiere)[2])
input <- mldr_from_dataframe(input , labelIndices = labelIndices )
br <- mldr_transform(input, type = "BR")
temp2 <- 1+dim(sco)[2]
i <- 1 # arrastre
temp <- br[[i]]
temp[,temp2] <- as.factor(temp[,temp2])
names(temp)[temp2] <- "metiere"
classifier <- IBk(metiere~ .,data=temp)                   
pre <- predict(classifier)
eval <- evaluate_Weka_classifier(classifier , numFolds = nrow(sco))
eval$confusionMatrix

# Predict arrastre on new data.
pre_OUT <- predict(classifier, newdata = sco_OUT,  type = "class")
temp <- which(pre==1)
temp2 <- which(pre_OUT==1)
rownames(OUT)[temp2]
response=response[-temp,] # removing arrestre
metiere=metiere[-temp,]
metiere2=metiere2[-temp]
OUT = OUT[-temp2,]
temp=which(apply(response,2,sum)==0) # species specific for arrastre
# ALATXA          ARNES         BRUIXA          CRANC CRANCA.DE.FONS      ESCAMARLA     ESPARDENYA     GAMBA.ROJA GAMBUSI.BLANCO 
# 2              5             10             21             22             25             28             33             34 
# GAMBUSI.ROJO    GBA..BLANCA    GBA..CARAB.          MAIRE        MOIXINA NEGRET.NEGRITO         ORIOLA         PELUDA     POTA.ALUDA 
# 35             37             38             50             51             61             63             70             73 
# QUISSONA          SIPIÓ          ULLAS          XERNA 
# 74             88             94             99

response=response[,-temp] # removing arrestre specific species
OUT = OUT[,-temp] # removing arrastre in the OUT set
dim(response)
dim(OUT)

#----------------
# 4: Identifying llampugueras
#----------------
# pca (after removing arrastre)
pca = prcomp(log(1+response),scale=F)
#pca = prcomp(response,scale=T)

# reducing dimensionality
vars=pca$sdev^2
plot(vars/sum(vars))
lines(brokenStick(1:length(vars), length(vars)))
bsDimension(vars)
ag=AuerGervini(vars, dd=c(nrow(response),ncol(response)))
agDimension(ag)
length(vars)
n.axes=20 # it should be bsDimension(var) but I have settinf it by trial-and-error
sco=scale(log(1+response), pca$center, pca$scale) %*% pca$rotation
sco=sco[,1:n.axes]
sco_OUT = scale(log(1+OUT), pca$center, pca$scale) %*% pca$rotation
sco_OUT=sco_OUT[,1:n.axes]
# plots 
# res.sp = get_pca_var(pca)
# par(mfrow=c(1,2))
# plot(sco[,1:2],col=metiere2,pch=19, main="Journeys")
# plot(res.sp$coord,type="n",main="Species")
# arrows(0,0,res.sp$coord[,1],res.sp$coord[,2],
#        lwd = 2, angle = 30, length = 0.05, col = 4)
# text(res.sp$coord,names(response),cex=0.3)

# classifier
input = data.frame(sco,metiere)
sco_OUT = data.frame(sco_OUT)
labelIndices =(dim(sco)[2]+1):(dim(sco)[2]+dim(metiere)[2])
input = mldr_from_dataframe(input , labelIndices = labelIndices )
# summary(input)
# length(sco_OUT)
br = mldr_transform(input, type = "BR")
temp2=1+dim(sco)[2]
i=4 # llampuguera
temp=br[[i]]
temp[,temp2]=as.factor(temp[,temp2])
names(temp)[temp2]="metiere"
classifier = IBk(metiere~ .,data=temp)                   
pre=predict(classifier)
eval=evaluate_Weka_classifier(classifier , numFolds = nrow(sco))
eval$confusionMatrix
# 31/03/2025 update classifier, n.axis = 20 -> llampugueras
# predicted
#     0  1
# 0 771  0
# 1   0 31

pre.OUT=predict(classifier, newdata = sco_OUT,  type = "class")

# Predicted llampugueras journeys.
temp=which(pre==1)
temp2 = which(pre.OUT==1)
response=response[-temp,] # removing llampuga
metiere=metiere[-temp,]
metiere2=metiere2[-temp]
OUT = OUT[-temp2,]
temp=which(apply(response,2,sum)==0) # species specific for llampuga
# PAMPOL 
# 47
response=response[,-temp] # removing llampugueras
OUT = OUT[,-temp] # removing llampugueras in the OUT set

#----------------
# 5: Identifying jonquillo
#----------------
# pca (after removing arrastre)
pca = prcomp(log(1+response),scale=F)
#pca = prcomp(response,scale=T)

# reducing dimensionality
vars=pca$sdev^2
plot(vars/sum(vars))
lines(brokenStick(1:length(vars), length(vars)))
bsDimension(vars)
ag=AuerGervini(vars, dd=c(nrow(response),ncol(response)))
agDimension(ag)
length(vars)
n.axes=15 # it should be bsDimension(var) but I have settinf it by trial-and-error
sco=scale(log(1+response), pca$center, pca$scale) %*% pca$rotation
sco=sco[,1:n.axes]
sco_OUT = scale(log(1+OUT), pca$center, pca$scale) %*% pca$rotation
sco_OUT=sco_OUT[,1:n.axes]
# plots 
# res.sp = get_pca_var(pca)
# par(mfrow=c(1,2))
# plot(sco[,1:2],col=metiere2,pch=19, main="Journeys")
# plot(res.sp$coord,type="n",main="Species")
# arrows(0,0,res.sp$coord[,1],res.sp$coord[,2],
#        lwd = 2, angle = 30, length = 0.05, col = 4)
# text(res.sp$coord,names(response),cex=0.3)

# classifier
input = data.frame(sco,metiere)
sco_OUT = data.frame(sco_OUT)
labelIndices =(dim(sco)[2]+1):(dim(sco)[2]+dim(metiere)[2])
input = mldr_from_dataframe(input , labelIndices = labelIndices )
# summary(input)
# length(sco_OUT)
br = mldr_transform(input, type = "BR")
temp2=1+dim(sco)[2]
i=8 # jonquillera
temp=br[[i]]
temp[,temp2]=as.factor(temp[,temp2])
names(temp)[temp2]="metiere"
classifier = IBk(metiere~ .,data=temp)                   
pre=predict(classifier)
eval=evaluate_Weka_classifier(classifier , numFolds = nrow(sco))
eval$confusionMatrix
# predicted
#     0  1
# 0 750  1
# 1   1 34

# 31/03/2025 update classifier with n.axes = 15 -> best results
# predicted
#     0  1
# 0 736  0
# 1   1 34
pre.OUT=predict(classifier, newdata = sco_OUT,  type = "class")

# Predicted jonquillo journeys.
temp=which(pre==1)
temp2 = which(pre.OUT==1)
response=response[-temp,] # removing jonquillo
metiere=metiere[-temp,]
metiere2=metiere2[-temp]
OUT = OUT[-temp2,]
temp=which(apply(response,2,sum)==0) # species specific for jonquillo
# CABOTI         ENFU JONQ..CABOTí    JONQUILLO 
# 9           19           29           30 
response=response[,-temp] # removing jonquillo
OUT = OUT[,-temp] # removing jonquillo in the OUT set

#----------------
# 6: Identifying palangre
#----------------
pca = prcomp(log(1+response),scale=F)
#pca = prcomp(response,scale=T)

# reducing dimensionality
vars=pca$sdev^2
plot(vars/sum(vars))
lines(brokenStick(1:length(vars), length(vars)))
bsDimension(vars)
ag=AuerGervini(vars, dd=c(nrow(response),ncol(response)))
agDimension(ag)
length(vars)
n.axes=8 # it should be bsDimension(var) but I have settinf it by trial-and-error
sco=scale(log(1+response), pca$center, pca$scale) %*% pca$rotation
sco=sco[,1:n.axes]
sco_OUT = scale(log(1+OUT), pca$center, pca$scale) %*% pca$rotation
sco_OUT=sco_OUT[,1:n.axes]

# classifier
input = data.frame(sco,metiere)
sco_OUT = data.frame(sco_OUT)
labelIndices =(dim(sco)[2]+1):(dim(sco)[2]+dim(metiere)[2])
input = mldr_from_dataframe(input , labelIndices = labelIndices )
# summary(input)
# length(sco_OUT)
br = mldr_transform(input, type = "BR")
temp2=1+dim(sco)[2]
i=2 # palangre
temp=br[[i]]
temp[,temp2]=as.factor(temp[,temp2])
names(temp)[temp2]="metiere"
classifier = IBk(metiere~ .,data=temp)                   
pre=predict(classifier)
eval=evaluate_Weka_classifier(classifier , numFolds = nrow(sco))
eval$confusionMatrix
# 31/03/2025 update classifier, n.axis = 8 -> palangre
# predicted
#     0   1
# 0 518  53
# 1  46 119

pre.OUT=predict(classifier, newdata = sco_OUT,  type = "class")

# Predicted palangre journeys.
temp=which(pre==1)
temp2 = which(pre.OUT==1)
# OUT[temp2,which(colSums(OUT[temp,] != 0) >0)]
# View(OUT[temp2,which(colSums(OUT[temp,] != 0) >0)])
response=response[-temp,] # removing palangre
metiere=metiere[-temp,]
metiere2=metiere2[-temp]
OUT = OUT[-temp2,]
temp=which(apply(response,2,sum)==0) # species specific for palangre
# CONGRE JAPUTA   RAOR  SERRA 
# 16     26     52     61
response=response[,-temp] # removing palangre
OUT = OUT[,-temp] # removing palangre in the OUT set

#----------------
# 6: Identifying trasmallo
#----------------
# pca = prcomp(log(1+response),scale=F)
# 
# # reducing dimensionality
# vars=pca$sdev^2
# plot(vars/sum(vars))
# lines(brokenStick(1:length(vars), length(vars)))
# bsDimension(vars)
# ag=AuerGervini(vars, dd=c(nrow(response),ncol(response)))
# agDimension(ag)
# length(vars)
# n.axes=9 # it should be bsDimension(var) but I have settinf it by trial-and-error
# sco=scale(log(1+response), pca$center, pca$scale) %*% pca$rotation
# sco=sco[,1:n.axes]
# sco_OUT = scale(log(1+OUT), pca$center, pca$scale) %*% pca$rotation
# sco_OUT=sco_OUT[,1:n.axes]
# 
# # classifier
# input = data.frame(sco,metiere)
# sco_OUT = data.frame(sco_OUT)
# labelIndices =(dim(sco)[2]+1):(dim(sco)[2]+dim(metiere)[2])
# input = mldr_from_dataframe(input , labelIndices = labelIndices )
# # summary(input)
# # length(sco_OUT)
# br = mldr_transform(input, type = "BR")
# temp2=1+dim(sco)[2]
# i=10 # trasmallo
# temp=br[[i]]
# temp[,temp2]=as.factor(temp[,temp2])
# names(temp)[temp2]="metiere"
# classifier = IBk(metiere~ .,data=temp)                   
# pre=predict(classifier)
# eval=evaluate_Weka_classifier(classifier , numFolds = nrow(sco))
# eval$confusionMatrix
# # 20/03/2025 update classifier, n.axis = 9 -> trasmallo
# # predicted
# #     0   1
# # 0 140  39
# # 1  34 505
# # Quitando primero palangre y despues trasmallo
# # predicted
# # 0   1
# # 0 27  10
# # 1 12 506
# 
# pre.OUT=predict(classifier, newdata = sco_OUT,  type = "class")
# 
# # Predicted trasmallo journeys.
# temp=which(pre==1)
# temp2 = which(pre.OUT==1)
# OUT[temp2,which(colSums(OUT[temp,] != 0) >0)]
# # View(OUT[temp2,which(colSums(OUT[temp,] != 0) >0)])
# response=response[-temp,] # removing trasmallo
# metiere=metiere[-temp,]
# metiere2=metiere2[-temp]
# OUT = OUT[-temp2,]
# temp=which(apply(response,2,sum)==0) # species specific for trasmallo
# # ALADROC CAVALLA.BISOS       SARDINA  TONYINA.ATUN 
# # 1            14            62            68 
# response=response[,-temp] # removing cercol
# OUT = OUT[,-temp] # removing cercol in the OUT set

#----------------
# 7: Identifying cercol
#----------------
# pca (after removing arrastre)
pca = prcomp(log(1+response),scale=F)
#pca = prcomp(response,scale=T)

# reducing dimensionality
vars=pca$sdev^2
plot(vars/sum(vars))
lines(brokenStick(1:length(vars), length(vars)))
bsDimension(vars)
ag=AuerGervini(vars, dd=c(nrow(response),ncol(response)))
agDimension(ag)
length(vars)
n.axes=20 # it should be bsDimension(var) but I have settinf it by trial-and-error
sco=scale(log(1+response), pca$center, pca$scale) %*% pca$rotation
sco=sco[,1:n.axes]
sco_OUT = scale(log(1+OUT), pca$center, pca$scale) %*% pca$rotation
sco_OUT=sco_OUT[,1:n.axes]

# classifier
input = data.frame(sco,metiere)
sco_OUT = data.frame(sco_OUT)
labelIndices =(dim(sco)[2]+1):(dim(sco)[2]+dim(metiere)[2])
input = mldr_from_dataframe(input , labelIndices = labelIndices )
# summary(input)
# length(sco_OUT)
br = mldr_transform(input, type = "BR")
temp2=1+dim(sco)[2]
i=9 # cercol
temp=br[[i]]
temp[,temp2]=as.factor(temp[,temp2])
names(temp)[temp2]="metiere"
classifier = IBk(metiere~ .,data=temp)                   
pre=predict(classifier)
eval=evaluate_Weka_classifier(classifier , numFolds = nrow(sco))
eval$confusionMatrix
# 31/03/2025 update classifier, n.axis = 20 -> cercol
# predicted
#     0 1
# 0 562 2
# 1   4 5

pre.OUT=predict(classifier, newdata = sco_OUT,  type = "class")

# Predicted llampugueras journeys.
temp=which(pre==1)
temp2 = which(pre.OUT==1)
response=response[-temp,] # removing cercol
metiere=metiere[-temp,]
metiere2=metiere2[-temp]
OUT = OUT[-temp2,]
temp=which(apply(response,2,sum)==0) # species specific for cercol
# ALADROC CAVALLA.BISOS       SARDINA  TONYINA.ATUN 
# 1            14            62            68 
response=response[,-temp] # removing cercol
OUT = OUT[,-temp] # removing cercol in the OUT set

#----------------
# 8: Identifying llagosta
#----------------
# pca (after removing arrastre)
pca = prcomp(log(1+response),scale=F)
#pca = prcomp(response,scale=T)

# reducing dimensionality
vars=pca$sdev^2
plot(vars/sum(vars))
lines(brokenStick(1:length(vars), length(vars)))
bsDimension(vars)
ag=AuerGervini(vars, dd=c(nrow(response),ncol(response)))
agDimension(ag)
length(vars)
n.axes=15 # it should be bsDimension(var) but I have settinf it by trial-and-error
sco=scale(log(1+response), pca$center, pca$scale) %*% pca$rotation
sco=sco[,1:n.axes]
sco_OUT = scale(log(1+OUT), pca$center, pca$scale) %*% pca$rotation
sco_OUT=sco_OUT[,1:n.axes]

# classifier
input = data.frame(sco,metiere)
sco_OUT = data.frame(sco_OUT)
labelIndices =(dim(sco)[2]+1):(dim(sco)[2]+dim(metiere)[2])
input = mldr_from_dataframe(input , labelIndices = labelIndices )
# summary(input)
# length(sco_OUT)
br = mldr_transform(input, type = "BR")
temp2=1+dim(sco)[2]
i=5 # llagosta
temp=br[[i]]
temp[,temp2]=as.factor(temp[,temp2])
names(temp)[temp2]="metiere"
classifier = IBk(metiere~ .,data=temp)                   
pre=predict(classifier)
eval=evaluate_Weka_classifier(classifier , numFolds = nrow(sco))
eval$confusionMatrix
# 18/03/2025 update classifier, n.axis = 10 -> llagosta
# predicted
# predicted
#     0   1
# 0 488  36
# 1  32 153

pre.OUT=predict(classifier, newdata = sco_OUT,  type = "class")

# Predicted llagosta journeys.
temp=which(pre==1)
temp2 = which(pre.OUT==1)
response=response[-temp,] # removing llagosta
metiere=metiere[-temp,]
metiere2=metiere2[-temp]
OUT = OUT[-temp2,]
temp=which(apply(response,2,sum)==0) # species specific for llagosta
# JAPUTA LLAGOSTA.BLANCA      SERRA.IMP. 
# 24              26              59
response=response[,-temp] # removing llagosta
OUT = OUT[,-temp] # removing llagosta in the OUT set

#----------------
# 9: Identifying sipia
#----------------
# pca (after removing arrastre)
pca = prcomp(log(1+response),scale=F)
#pca = prcomp(response,scale=T)

# reducing dimensionality
vars=pca$sdev^2
plot(vars/sum(vars))
lines(brokenStick(1:length(vars), length(vars)))
bsDimension(vars)
ag=AuerGervini(vars, dd=c(nrow(response),ncol(response)))
agDimension(ag)
length(vars)
n.axes=20 # it should be bsDimension(var) but I have settinf it by trial-and-error
sco=scale(log(1+response), pca$center, pca$scale) %*% pca$rotation
sco=sco[,1:n.axes]
sco_OUT = scale(log(1+OUT), pca$center, pca$scale) %*% pca$rotation
sco_OUT=sco_OUT[,1:n.axes]

# classifier
input = data.frame(sco,metiere)
sco_OUT = data.frame(sco_OUT)
labelIndices =(dim(sco)[2]+1):(dim(sco)[2]+dim(metiere)[2])
input = mldr_from_dataframe(input , labelIndices = labelIndices )
# summary(input)
# length(sco_OUT)
br = mldr_transform(input, type = "BR")
temp2=1+dim(sco)[2]
i=3 # sipia
temp=br[[i]]
temp[,temp2]=as.factor(temp[,temp2])
names(temp)[temp2]="metiere"
classifier = IBk(metiere~ .,data=temp)                   
pre=predict(classifier)
eval=evaluate_Weka_classifier(classifier , numFolds = nrow(sco))
eval$confusionMatrix
# 18/03/2025 update classifier, n.axis = 10 -> sipia
# predicted
#     0   1
# 0 423  77
# 1  76 133

pre.OUT=predict(classifier, newdata = sco_OUT,  type = "class")

# Predicted sipia journeys.
temp=which(pre==1)
temp2 = which(pre.OUT==1)
response=response[-temp,] # removing sipia
metiere=metiere[-temp,]
metiere2=metiere2[-temp]
OUT = OUT[-temp2,]
temp=which(apply(response,2,sum)==0) # species specific for sipia
# CAVEC LANGOSTINO   MORRUDAS       REIG     SABOGA 
# 12         24         37         52         53
response=response[,-temp] # removing sipia
OUT = OUT[,-temp] # removing sipia in the OUT set

#----------------
# 10: Identifying moll
#----------------
# pca (after removing arrastre)
pca = prcomp(log(1+response),scale=F)
#pca = prcomp(response,scale=T)

# reducing dimensionality
vars=pca$sdev^2
plot(vars/sum(vars))
lines(brokenStick(1:length(vars), length(vars)))
bsDimension(vars)
ag=AuerGervini(vars, dd=c(nrow(response),ncol(response)))
agDimension(ag)
length(vars)
n.axes=20 # it should be bsDimension(var) but I have settinf it by trial-and-error
sco=scale(log(1+response), pca$center, pca$scale) %*% pca$rotation
sco=sco[,1:n.axes]
sco_OUT = scale(log(1+OUT), pca$center, pca$scale) %*% pca$rotation
sco_OUT=sco_OUT[,1:n.axes]

# classifier
input = data.frame(sco,metiere)
sco_OUT = data.frame(sco_OUT)
labelIndices =(dim(sco)[2]+1):(dim(sco)[2]+dim(metiere)[2])
input = mldr_from_dataframe(input , labelIndices = labelIndices )
# summary(input)
# length(sco_OUT)
br = mldr_transform(input, type = "BR")
temp2=1+dim(sco)[2]
i=7 # moll
temp=br[[i]]
temp[,temp2]=as.factor(temp[,temp2])
names(temp)[temp2]="metiere"
classifier = IBk(metiere~ .,data=temp)                   
pre=predict(classifier)
eval=evaluate_Weka_classifier(classifier , numFolds = nrow(sco))
eval$confusionMatrix
# 18/03/2025 update classifier, n.axis = 7 -> moll
# predicted
#     0  1
# 0 261 20
# 1  23 25
# 1  19 160

pre.OUT=predict(classifier, newdata = sco_OUT,  type = "class")

# Predicted llagosta journeys.
temp=which(pre==1)
temp2 = which(pre.OUT==1)
response=response[-temp,] # removing moll
metiere=metiere[-temp,]
metiere2=metiere2[-temp]
OUT = OUT[-temp2,]
temp=which(apply(response,2,sum)==0) # species specific for moll
# BOGA     GERRET       LLUÇ MOLL.BLANC   MORRALLA 
# 5         22         26         28         32 
response=response[,-temp] # removing llagosta
OUT = OUT[,-temp] # removing llagosta in the OUT set
