#!/usr/bin/env Rscript

#---------------------------------------
# Estimating fishing effort for small-scale fisheries from vessel monitoring system
#   2/5 - Module for metiere estimation
#---------------------------------------

# Author: Miquel Palmer Vidal

# this modelue predicts the most probable metiere of a fishing journey
# from catches
# The analytical estrategy consist in an step-by-step elimination of metires:
#  1) arrosegament
#  2) llampugueres
#  3) jonquilleres
#  4) cercol
#  5) nasa pop
#  6) tresmall vs palangro
# The first 5 steps were binary calssifiers based on k-means.
# The last step (tresmall vs palangro) was a multilabel calssifier (k-means)
# In all cases the immediate input of the classifiers was the result of a 
# multivariate dimensionaly reductions (either PCA or CA).
# The paramteres of the six classifiers were obtained at "classificador.R"

# input: catches from one survey
# outpout: metiere

library(mldr)  
library(RWeka)

rm(list = ls())

# --- Folder paths
setwd(
  "C:/Users/UIB/Desktop/REMAR-automatizacion/REMAR-automatizacion/scripts/r"
)
input_dir <- "../../data/ventaslonja"
rdata_dir <- "../../data/rdata"
processed_dir <- "../../data/processed"
reference_dir <- "../../data/reference"

# Load data from sales bills and classifiers' parameters
load(file.path(rdata_dir, "sample.RData")) 
load(file.path(rdata_dir, "classificadors.RData"))

OUT <- data.frame(OUT)
results <- list()
results$class <- rep(NA, length(journey_list))

for(i in seq_along(journey_list)){
  sample <- OUT[i, ]
  classified <- FALSE
  for(class_name in names(classifiers)){
    clf <- classifiers[[class_name]]
    
    temp_names <- make.names(colnames(sample), unique = TRUE)
    selected_species <- which( temp_names %in% clf$sp.names)
    new <- sample[selected_species]
    
    # Classification
    label <- classification.function(sp.names = clf$sp.names,
                                     new = new,
                                     pca = clf$pca,
                                     n.axes = clf$n.axes,
                                     sco.names = clf$sco.names,
                                     classifier = clf$classifier)
    if(label == "1"){
      results$class[i] <- class_name
      classified <- TRUE
      break
    } else if(label == "10"){
      results$class[i] <- "tresmall"
      classified <- TRUE
      break
    } else if(label == "01"){
      results$class[i] <- "palangre"
      classified <- TRUE
      break
    }
  }
}

idx_jonquillo <- which(results$class == "jonquillera")
# Predicted JONQUILLO
OUT_jonquillo <- OUT[idx_jonquillo, , drop = FALSE]
journey_jonquillo <- journey_list[idx_jonquillo]

idx_tresmall <- which(results$class == "tresmall")
# Predicted TRASMALLO
OUT_tresmall <- OUT[idx_tresmall, , drop = FALSE]
journey_tresmall <- journey_list[idx_tresmall]

save(
  OUT_jonquillo, journey_jonquillo, OUT_tresmall, journey_tresmall,
  file = file.path(rdata_dir, "predicted.RData"))