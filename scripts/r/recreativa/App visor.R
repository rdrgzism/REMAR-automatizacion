#                                                rm(list = ls())
#                 par(mfrow = c(1, 1))

library(readxl)
library(dplyr)
library(gridExtra)
library(ggplot2)
library(stringi)
library(cowplot) 
library(patchwork)
library(tidyverse)
library(tidyr)
library(reshape2)
library(lubridate)


setwd("C:/Users/uib/OneDrive - Universitat de les Illes Balears/REMAR (REcursos MARins pesquers a Balears)/P. recreativa/App/Paper/R")

datadpr <- read_excel("C:/Users/uib/OneDrive - Universitat de les Illes Balears/REMAR (REcursos MARins pesquers a Balears)/P. recreativa/App/Paper/Declaraciones App.xlsx",
                     col_types = "text")
datadpr <- as.data.frame(datadpr)
datadpr <- datadpr %>% rename(`zona 6`= `...117`)

################################### aspectes dee format ########################################

colnames(datadpr) <- tolower(colnames(datadpr))
colnames(datadpr) <- gsub(" / ", "_", colnames(datadpr)) 
colnames(datadpr) <- gsub("/", "_", colnames(datadpr)) 
#colnames(datadpr) <- sub(".*_", "", colnames(datadpr))   #Elimina abans de _
colnames(datadpr) <- sub("_.*", "", colnames(datadpr))

remove_diacritics <- function(x) {            #Elimina accents
  stri_trans_general(x, "Latin-ASCII")
}
new_colnames <- remove_diacritics(colnames(datadpr))
colnames(datadpr) <- new_colnames

colnames(datadpr) <- gsub("verderol - sirviola", "verderol", colnames(datadpr))
colnames(datadpr) <- gsub("mare d'anfos, dento", "mare anfos", colnames(datadpr))
colnames(datadpr) <- gsub("mare d'anfos", "rei", colnames(datadpr))
colnames(datadpr) <- gsub(" ", "_", colnames(datadpr))

colnames(datadpr) <- remove_diacritics(colnames(datadpr))

datadpr <- datadpr %>% mutate(across(where(is.character), tolower))
datadpr <- datadpr %>% mutate(across(where(is.character), ~ stri_trans_general(.x, "Latin-ASCII")))

# ID column
datadpr$id <- seq.int(nrow(datadpr))


# Reserva marina consistent

datadpr$reserva_marina <- gsub("freus eif" , "Reserva Marina dels Freus d'Eivissa i Formentera", datadpr$reserva_marina)
datadpr$reserva_marina <- gsub("dragonera","Reserva marina de sa Dragonera (Reserva marina autonòmica)" , datadpr$reserva_marina)
datadpr$reserva_marina <- gsub("bahia palma", "Reserva Marina de la Badia de Palma", datadpr$reserva_marina)
datadpr$reserva_marina <- gsub("nord menorca", "Reserva Marina del Nord de Menorca", datadpr$reserva_marina)
datadpr$reserva_marina <- gsub("es vedra-vedranell", "Reserva Marina es Vedrà - Vedranell", datadpr$reserva_marina)
datadpr$reserva_marina <- gsub("migjorn", "Reserva Marina del Migjorn de Mallorca", datadpr$reserva_marina)
datadpr$reserva_marina <- gsub("toro-malgrats", "Reserva Marina de les illes del Toro i de les Malgrats", datadpr$reserva_marina)
datadpr$reserva_marina <- gsub("llevant", "Reserva Marina del Levante de Mallorca-Cala Rajada", datadpr$reserva_marina)
datadpr$reserva_marina <- gsub("tagomago", "Reserva Marina costa nord-est Eivissa Tagomago", datadpr$reserva_marina)
datadpr$reserva_marina <- gsub("punta de sa creu", "Reserva marina punta de sa Creu", datadpr$reserva_marina)
datadpr$reserva_marina <- gsub("illa de l'aire", "Reserva Marina de l’Illa de l’Aire", datadpr$reserva_marina)
datadpr$reserva_marina <- gsub("ses bledes", "Reserva Marina de ses Bledes", datadpr$reserva_marina)



######Fix zones

# My understanding is that for there to be data in zona_2 we need to have data in zona_1 first, so proceding to fix this mess
datadpr$zona_5 <- ifelse(is.na(datadpr$zona_5) & !is.na(datadpr$zona_6), datadpr$zona_6, datadpr$zona_5)
datadpr$zona_6 <- ifelse(datadpr$zona_5 == datadpr$zona_6, NA, datadpr$zona_6)

datadpr$zona_4 <- ifelse(is.na(datadpr$zona_4) & !is.na(datadpr$zona_5), datadpr$zona_5, datadpr$zona_4)
datadpr$zona_5 <- ifelse(datadpr$zona_4 == datadpr$zona_5, NA, datadpr$zona_5)

datadpr$zona_3 <- ifelse(is.na(datadpr$zona_3) & !is.na(datadpr$zona_4), datadpr$zona_4, datadpr$zona_3)
datadpr$zona_4 <- ifelse(datadpr$zona_3 == datadpr$zona_4, NA, datadpr$zona_4)

datadpr$zona_2 <- ifelse(is.na(datadpr$zona_2) & !is.na(datadpr$zona_3), datadpr$zona_3, datadpr$zona_2)
datadpr$zona_3 <- ifelse(datadpr$zona_2 == datadpr$zona_3, NA, datadpr$zona_3)

datadpr$zona_1 <- ifelse(is.na(datadpr$zona_1) & !is.na(datadpr$zona_2), datadpr$zona_2, datadpr$zona_1)
datadpr$zona_2 <- ifelse(datadpr$zona_1 == datadpr$zona_2, NA, datadpr$zona_2)

datadpr$zona_3 <- ifelse(is.na(datadpr$zona_3) & !is.na(datadpr$zona_4), datadpr$zona_4, datadpr$zona_3)
datadpr$zona_4 <- ifelse(datadpr$zona_3 == datadpr$zona_4, NA, datadpr$zona_4)

datadpr$zona_2 <- ifelse(is.na(datadpr$zona_2) & !is.na(datadpr$zona_3), datadpr$zona_3, datadpr$zona_2)
datadpr$zona_3 <- ifelse(datadpr$zona_2 == datadpr$zona_3, NA, datadpr$zona_3)

datadpr$zona_1 <- ifelse(is.na(datadpr$zona_1) & !is.na(datadpr$zona_2), datadpr$zona_2, datadpr$zona_1)
datadpr$zona_2 <- ifelse(datadpr$zona_1 == datadpr$zona_2, NA, datadpr$zona_2)


##################################### Prep data DPR ########################################
# We don't want unfinished trips
datadpr <- subset(datadpr, datadpr$duracio != "en curs")
datadpr$fin <- format(as.Date(datadpr$fin, format="%d/%m/%Y"), "%Y-%m-%d")
datadpr$inicio <- as.numeric(datadpr$inicio)
datadpr$inicio <- as.POSIXct(datadpr$inicio * 86400, origin = "1899-12-30", tz = "UTC")
datadpr$dia <- format(datadpr$inicio, "%Y-%m-%d")
datadpr$dia <- as.Date(datadpr$dia, format = "%Y-%m-%d")
datadpr$end <- format(as.POSIXct(as.numeric(datadpr$`h-fin`) * 86400, origin = "1899-12-30", tz = "UTC"), "%H:%M")
datadpr$start <- format(as.POSIXct(as.numeric(datadpr$`h-inicio`) * 86400, origin = "1899-12-30", tz = "UTC"), "%H:%M")

#Create unique ID for $usuario
datadpr$usuari <- as.integer(factor(datadpr$usuari))


colnames(datadpr) <- gsub("duracion", "duracio", colnames(datadpr))
colnames(datadpr) <- gsub("currica_de_fons", "currica_fons", colnames(datadpr))


#Change modalities
datadpr$calamars <- ifelse(datadpr$calamars == "x", 1, NA)
datadpr$currica_fons <- ifelse(datadpr$currica_fons == "x", 1, NA)
datadpr$fluixa <- ifelse(datadpr$fluixa == "x", 1, NA)
datadpr$pescasub <- ifelse(datadpr$pescasub == "x", 1, NA)
datadpr$raors <- ifelse(datadpr$raors == "x", 1, NA)
datadpr$roquer <- ifelse(datadpr$roquer == "x", 1, NA)
datadpr$serrans <- ifelse(datadpr$serrans == "x", 1, NA)
datadpr$spinning <- ifelse(datadpr$spinning == "x", 1, NA)
modality_columns <- c("calamars", "currica_fons", "fluixa", "pescasub",
                      "raors", "roquer", "serrans", "spinning")


# If we'd rather use scientific names
#list1 <- c("alatxa", "anfos", "anfos_blanc", "anfos_bord", "anfos_llis", "aranya_de_cap_negre", "aranya_fragata", "aranya_monja", "ase", "bacora", "bacoreta", "besuc", "bis", "boga", "bonitol", "calamar", "cap-roig", "capsamper", "congre", "cantara", "donzella", "dentol", "escorball", "escorpora", "esparrall", "espet", "gall", "gato", "grivia", "lampuga", "llissa", "marbre", "rei", "mare_anfos", "melva", "moll", "morena", "morruda", "mollera_de_fang", "mollera_de_roca", "oblada", "orada", "orenol", "pagell", "palaia", "palomina_blanca", "pedac", "pulpo", "paguera", "raboa_de_magranar", "rafel", "raor", "saig", "salpa", "sarg", "serra", "serra_imperial", "sirviola", "sorell", "surer", "sipia", "tord_flassader", "tord_massot", "tord_pastanaga", "trugeta", "vaca", "variada", "verat", "verderol", "xerna", "xucla")
list1 <- c("sarg_reial","tonyina","tuta","verrugato","sorella","roncador","palometon","llop","fedri", "alatxa", "anfos", "anfos_blanc", "anfos_bord", "anfos_llis", "aranya_de_cap_negre", "aranya_fragata", "aranya_monja", "ase", "bacora", "bacoreta", "besuc", "bis", "boga", "bonitol", "calamar", "cap-roig", "capsamper", "congre", "cantara", "donzella", "dentol", "escorball", "escorpora", "esparrall", "espet", "gall", "gato", "grivia", "llampuga", "llissa", "marbre", "rei", "mare_anfos", "melva", "moll", "morena", "morruda", "mollera_de_fang", "mollera_de_roca", "oblada", "orada", "orenol", "pagell", "palaia", "palomina_blanca", "pedac", "pop", "paguera", "raboa_de_magranar", "rafel", "raor", "saig", "salpa", "sarg", "serra", "gallineta", "sirviola", "sorell", "surer", "sipia", "tord_flassader", "tord_massot", "tord_pastanaga", "trugeta", "vaca", "variada", "verat", "verderol", "xerna", "xucla")
list2 <- c("Diplodus_cervinus","Thunnus_thynnus","Chromis_chromis","Umbrina_cirrosa","Caranx_rhonchus","Pomadasys_incisus","Lichia_amia","Dicentrarchus_labrax","Thalassoma_pavo","Sardinella_aurita", "Epinephelus_marginatus", "Epinephelus_aeneus", "Mycteroperca_rubra", "Epinephelus_costae", "Trachinus_radiatus", "Trachinus_araneus", "Trachinus_draco", "Dactylopterus_volitans", "Thunnus_alalunga", "Euthynnus_alletteratus", "Pagellus_acarne", "Scomber_japonicus", "Boops_boops", "Sarda_sarda", "Loligo_vulgaris", "Scorpaena_scrofa", "Synodus_saurus", "Conger_conger", "Spondyliosoma_cantharus", "Coris_julis", "Dentex_dentex", "Sciaena_umbra", "Scorpaena_porcus", "Diplodus_annularis", "Sphyraena_sphyraena", "Zeus_faber", "Scyliorhinus_canicula", "Labrus_viridis", "Coryphaena_hippurus", "Mugil_cephalus", "Lithognathus_mormyrus", "Apogon_imberbis", "Anthias_anthias", "Auxis_rochei", "Mullus_surmuletus", "Muraena_helena", "Diplodus_puntazzo", "Phycis_blennoides", "Phycis_phycis", "Oblada_melanura", "Sparus_aurata", "Exocoetus_volitans", "Pagellus_erythrinus", "Solea_solea", "Trachinotus_ovatus", "Bothus_podas", "Octopus_vulgaris", "Pagrus_pagrus", "Blennius_ocellaris", "Trigla_lyra", "Xyrichthys_novacula", "Symphodus_mediterraneus", "Sarpa_salpa", "Diplodus_sargus", "Serranus_cabrilla", "Helicolenus_dactylopterus", "Seriola_dumerili", "Trachurus_trachurus", "Balistes_capriscus", "Sepia_officinalis", "Symphodus_doderleini", "Labrus_merula", "Labrus_mixtus", "Symphodus_rostratus", "Serranus_scriba", "Diplodus_vulgaris", "Scomber_scombrus", "Seriola_dumerili_xs", "Epinephelus_caninus", "Spicara_maena")
col_names <- colnames(datadpr)
# Perform the substitutions for each word separately with word boundaries
for (i in seq_along(list1)) {
  col_names <- gsub(paste0("\\b", list1[i], "\\b"), list2[i], col_names, perl = TRUE)
}
colnames(datadpr) <- col_names


species_columns <- c("Diplodus_cervinus","Thunnus_thynnus","Chromis_chromis","Umbrina_cirrosa","Caranx_rhonchus","Pomadasys_incisus","Lichia_amia","Dicentrarchus_labrax","Thalassoma_pavo","Sardinella_aurita", "Epinephelus_marginatus", "Epinephelus_aeneus", "Mycteroperca_rubra", "Epinephelus_costae", "Trachinus_radiatus", "Trachinus_araneus", "Trachinus_draco", "Dactylopterus_volitans", "Thunnus_alalunga", "Euthynnus_alletteratus", "Pagellus_acarne", "Scomber_japonicus", "Boops_boops", "Sarda_sarda", "Loligo_vulgaris", "Scorpaena_scrofa", "Synodus_saurus", "Conger_conger", "Spondyliosoma_cantharus", "Coris_julis", "Dentex_dentex", "Sciaena_umbra", "Scorpaena_porcus", "Diplodus_annularis", "Sphyraena_sphyraena", "Zeus_faber", "Scyliorhinus_canicula", "Labrus_viridis", "Coryphaena_hippurus", "Mugil_cephalus", "Lithognathus_mormyrus", "Apogon_imberbis", "Anthias_anthias", "Auxis_rochei", "Mullus_surmuletus", "Muraena_helena", "Diplodus_puntazzo", "Phycis_blennoides", "Phycis_phycis", "Oblada_melanura", "Sparus_aurata", "Exocoetus_volitans", "Pagellus_erythrinus", "Solea_solea", "Trachinotus_ovatus", "Bothus_podas", "Octopus_vulgaris", "Pagrus_pagrus", "Blennius_ocellaris", "Trigla_lyra", "Xyrichthys_novacula", "Symphodus_mediterraneus", "Sarpa_salpa", "Diplodus_sargus", "Serranus_cabrilla", "Helicolenus_dactylopterus", "Seriola_dumerili", "Trachurus_trachurus", "Balistes_capriscus", "Sepia_officinalis", "Symphodus_doderleini", "Labrus_merula", "Labrus_mixtus", "Symphodus_rostratus", "Serranus_scriba", "Diplodus_vulgaris", "Scomber_scombrus", "Seriola_dumerili_xs", "Epinephelus_caninus", "Spicara_maena", "nada")


# Reshape the data: gather the species columns into 'species' and 'captures'. Cada pesca (ID) passarà a tenir tantes files replicades com espècies pescades

datadpr <- datadpr %>% 
  pivot_longer(cols = all_of(species_columns),
               names_to = "especie",
               values_to = "captures")


# Remove rows where 'captures' is NA
datadpr <- datadpr %>%
  filter(!is.na(captures))

species_variables <- c("Caranx_rhonchus","Lichia_amia", "Xyrichthys_novacula",  "Serranus_scriba",  "Serranus_cabrilla",  "Bothus_podas",  "Loligo_vulgaris",  "Diplodus_annularis",  "Coris_julis",
                       "Trachinus_draco",  "Spondyliosoma_cantharus",  "Seriola_dumerili",  "Seriola_dumerili_xs",  "Euthynnus_alletteratus",  "Pagrus_pagrus",  "Pagellus_erythrinus",  "Diplodus_vulgaris",
                       "Trachurus_trachurus",  "Oblada_melanura",  "Synodus_saurus",  "Trachinus_radiatus",  "Labrus_mixtus",  "Diplodus_sargus",  "Balistes_capriscus",  "Sphyraena_sphyraena",  "Boops_boops",  "Auxis_rochei",  "Scorpaena_scrofa",  "Coryphaena_hippurus",  "Spicara_maena",  "Dentex_dentex",  "Pagellus_acarne",  "Scomber_japonicus",
                       "Labrus_viridis",  "Mullus_surmuletus",  "Epinephelus_marginatus",  "Octopus_vulgaris",  "Sardinella_aurita",  "Labrus_merula",  "Lithognathus_mormyrus",  "Thunnus_alalunga",
                       "Dactylopterus_volitans",  "Scomber_scombrus",  "Muraena_helena",  "Scyliorhinus_canicula",  "Trigla_lyra",  "Diplodus_puntazzo",  "Sparus_aurata",
                       "Epinephelus_caninus",  "Zeus_faber",  "Sepia_officinalis",  "Symphodus_doderleini",  "Apogon_imberbis",  "Epinephelus_costae",  "Conger_conger",  "Scorpaena_porcus")

#dades obtingudes d'informe de la conselleria
species_multiplier <- c(
  200, 341.23, 55.51, 48.48, 53.11, 59.29, 219,  40.83, 19.65, 63.33, 119.87, 244, 244,  204, 147.68, 128.46, 109.93, 71.24,  105.02, 103.31, 240.12, 71.31, 181.07,
  595.33, 201, 65.79, 703, 543.29,  930, 98.37, 297.84, 92.3,  222.45, 346.76, 107.59, 2087.5, 212.28,  48.57, 289.37, 107.63, 2152.5, 631.56,  191.81, 889.95, 277.52, 260, 242.77,
  193, 2087.5, 882.5, 200, 102.86, 12.58, 1700, 889.95,  96.22)


# Add the new 'weight' column
multiplier_df <- data.frame(
  especie = species_variables,
  multiplier = species_multiplier
)

datadpr <- datadpr %>%
  left_join(multiplier_df, by = "especie") %>%
  mutate(captures = as.numeric(captures),
         weight = captures * multiplier)


### Arreglam noms d'espècies

datadpr$especie <- gsub("_", " ", datadpr$especie)
datadpr$especie <- sub("^(.)", "\\U\\1", tolower(datadpr$especie), perl = TRUE)


#############################################################################

datadpr$duracio <- as.numeric(datadpr$duracio)
datadpr$pescadors <- as.numeric(datadpr$pescadors)
dataset <- datadpr

desired_order <- c("id", "usuari", "reserva_marina", "especie", "nre._peces", "captures", 
                   "weight", "pescadors", "dia", "duracio", "start", "end", "inicio", 
                   "calamars", "currica_fons", "fluixa", "pescasub", "raors", 
                   "roquer", "serrans", "spinning","zona_1", "zona_2", "zona_3", "zona_4", "zona_5", "zona_6") 

dataset <- dataset[desired_order]
dataset$weight <- dataset$weight/1000 #passam a kg


########################################## ANALISIS ##############################################
dataset$dia <- as.Date(dataset$dia, format = "%Y-%m-%d")
dataset$year <- format(dataset$dia, "%Y")
dataset$month <- format(dataset$dia, "%m")
#dataset <- subset(dataset, dataset$duracio > 0.2) # some entries are of less than 15 minutes
dataset$modalitysum <- rowSums(dataset[modality_columns], na.rm = TRUE)


dataset <- dataset %>%
  filter(!(modalitysum == 0))




dataset <- dataset %>%
  mutate(captures = ifelse(especie == "nada", 0, captures))
dataset <- dataset %>%
  mutate(weight = ifelse(especie == "nada", 0, weight))


#afegim files quan hi ha més d'una modalitat declarada
dataset <- dataset %>%
  pivot_longer(
    cols = all_of(modality_columns),
    names_to = "modality",
    values_to = "value"
  ) %>%
  filter(value == 1) %>%
  dplyr::select(-value)

#Afegim variables dividides pel nombre de modalitats (_mod)
dataset <- dataset %>%
  mutate(duracio_mod = duracio / modalitysum)
dataset$effort <- as.numeric(dataset$duracio) * dataset$pescadors
dataset$effort_mod <- as.numeric(dataset$duracio_mod) * dataset$pescadors
dataset$captures_mod <- dataset$captures / dataset$modalitysum
dataset$weight_mod <- dataset$weight / dataset$modalitysum
#dataset$cpue.esp.weight <- dataset$weight / dataset$effort #cpue per kg. Això ja és per modalitat (o total, és igual). weight/E = weight/mod / E/mod
#dataset$cpue.esp.ind <- dataset$captures / dataset$effort # cpue per nombre de peixos



# First, count the non-NA zone columns (n_zones) and compute new zone-adjusted columns
dataset <- dataset %>%
  mutate(n_zones = rowSums(!is.na(across(starts_with("zona_")))),
         duracio_mod_zone = duracio_mod / n_zones,
         duracio_zone = duracio / n_zones,
         effort_mod_zone      = duracio_mod_zone * pescadors,
         effort_zone = duracio_zone * pescadors,
         captures_mod_zone = captures_mod / n_zones,
        weight_mod_zone = weight_mod / n_zones,
        captures_zone = captures / n_zones,
        weight_zone = weight / n_zones)

# Then, reshape the dataset to one row per non-NA zone
dataset <- dataset %>%
  pivot_longer(
    cols = starts_with("zona_"),
    names_to = "zona",
    values_to = "zona_value"
  ) %>%
  filter(!is.na(zona_value))



dataset <- dataset %>%
  filter(!is.na(reserva_marina))


#calculate cpue
cpue <- dataset %>%
  group_by(id) %>%
  summarize(
    reserva_marina = first(reserva_marina),
    fishn = sum(captures_mod_zone, na.rm = TRUE),   # Total captures per id
    weightn = sum(weight_mod_zone, na.rm = TRUE),   # Total captures per id
    pescadors = first(pescadors),         # First value of pescadors
    duracio = first(duracio),             # First value of duracio
    cpue.ind = fishn / (pescadors * duracio),  # CPUE calculation
    cpue.weight = weightn / (pescadors * duracio),
    .groups = 'drop'
  )

# Merge cpue_unique back into the original dataset (if needed)
dataset <- dataset %>%
  dplyr::left_join(cpue %>% dplyr::select(id, cpue.ind, cpue.weight), by = "id")


dataset <- dataset %>%
  mutate(
    cpue.ind.esp   = captures/(duracio * pescadors),
    cpue.weight.esp   = weight/(duracio * pescadors)
  )


#write.csv(dataset, "dataset_gran.csv", row.names = FALSE)

#*# "id"                = nombre de pescada. Identificador de cada barca en un determinat dia  
# "usuari"            = id únic de cada pescador. Es manté sempre el mateix pel mateix DNI declarant    
#*# "reserva_marina"  
#*# "especie"        
# "nre._peces"        = total de peces de la pescada   
# "captures"          = total de peces de l'espècie a la pescada        
# "weight"            = total del pes de les peces de l'espècie a la pescada        
#*# "pescadors"         = total pescadors a la barca
#*# "dia"               = aaaa-mm-dd
# "duracio"           = total hores de pesca
# "start"             = hora d'inici de la declaració
# "end"               = hora fi de la declaració
# "inicio"            -
# "year"              = any
#*# "month"             = mes
# "modalitysum"       = suma del nombre de modalitats declarades
#*# "modality"          = modalitat. Si modalitysum = 2, hi haurà dues files amb el mateix id i diferent modalitat
# "duracio_mod"       = duracio / modalitysum  , vendria a ser la duració que correspon a cada modalitat. Una aproximació
# "effort"            = pescadors * duracio
# "effort_mod"        = duracio_mod * pescadors
# "captures_mod"      = captures / modalitysum , total peces de l'espècie per modalitat
# "weight_mod"        = weight / modalitysum
# "n_zones"           = nombre de zones declarades
# "duracio_mod_zone"  = duracio_mod / n_zones
# "duracio_zone"      = duracio / n_zones
#*# "effort_mod_zone"   = duracio_mod_zone * pescadors
# "effort_zone"       = duracio_zone * pescadors
#*# "captures_mod_zone" = captures_mod / n_zones   , total peces de l'espècie per modalitat i per zona
#*# "weight_mod_zone"   = weight_mod / n_zones
# "captures_zone"     = captures / n_zones
# "weight_zone"       = weight / n_zones
# "zona"              = zona_1 o zona_2 o zona_3...
#*# "zona_value"        = nom de la zona segons la app
#*# "cpue.ind"          = cpue total de nombre d'individus (sum(captures_mod_zone) / E), (que es el mateix que per modalitat i per zona), sense separar per espècies)
#*# "cpue.weight"       = cpue total de weight a la pescada
# "cpue.ind.esp"      = cpue total (o per zona o modalitat, és igual) de cada espècie a la pescada
# "cpue.weight.esp"   = weight total (o per zona o modalitat, és igual) de cada espècie a la pescada




####################################################################################################
#################################### Mètriques visor ###############################################
####################################################################################################

#A representar:
# E  ,  CPUE (ind) , cpue (weight) , nombre de barques , nombre de pescadors , nombre de captures , 
# Per:
# dia o mes o any (feim 3 datasets diferents lo bruto?), modalitat , reserva , zona 











####################################################################################################
#################################### Feina a nivell de especie #####################################
####################################################################################################


# 1) Overall (by species)
especies_overall <- dataset %>%
  group_by(especie) %>%
  summarise(
    sum_captures          = sum(captures, na.rm = TRUE),
    sum_weight            = sum(weight,   na.rm = TRUE),
    mean_cpue_ind_esp     = mean(cpue.ind.esp,     na.rm = TRUE),
    mean_cpue_weight_esp  = mean(cpue.weight.esp,  na.rm = TRUE))

# 2) By month
especies_by_month <- dataset %>%
  group_by(especie, month) %>%
  summarise(
    sum_captures          = sum(captures, na.rm = TRUE),
    sum_weight            = sum(weight,   na.rm = TRUE),
    mean_cpue_ind_esp     = mean(cpue.ind.esp,     na.rm = TRUE),
    mean_cpue_weight_esp  = mean(cpue.weight.esp,  na.rm = TRUE)
  )

# 3) By modality
especies_by_modality <- dataset %>%
  group_by(especie, modality) %>%
  summarise(
    sum_captures          = sum(captures, na.rm = TRUE),
    sum_weight            = sum(weight,   na.rm = TRUE),
    mean_cpue_ind_esp     = mean(cpue.ind.esp,     na.rm = TRUE),
    mean_cpue_weight_esp  = mean(cpue.weight.esp,  na.rm = TRUE)
  )

# 4) By month and modality
especies_by_month_modality <- dataset %>%
  group_by(especie, month, modality) %>%
  summarise(
    sum_captures          = sum(captures, na.rm = TRUE),
    sum_weight            = sum(weight,   na.rm = TRUE),
    mean_cpue_ind_esp     = mean(cpue.ind.esp,     na.rm = TRUE),
    mean_cpue_weight_esp  = mean(cpue.weight.esp,  na.rm = TRUE)
  )

