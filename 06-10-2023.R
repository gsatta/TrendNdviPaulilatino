library(dplyr)
library(purrr)
library(tidyr)
library(sf)
library(bfast)
library(progress)

# Carica il file dell'ndvi
NDVI_VALUES <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_VALUES/merged_data_ndvi.shp")

# Ottenere i valori unici nella colonna "COD"
cod_unique <- unique(NDVI_VALUES$COD)

# Inizializza una lista per archiviare i risultati
dbest_results_list <- list()

# Inizializza una lista vuota per memorizzare le date di maggior variazione negativa
worst_change_dates <- list()

# Crea una nuova barra di avanzamento
pb <- progress_bar$new(total = length(cod_unique))

# Loop attraverso i valori unici di "COD"
for (cod_value in cod_unique) {
  # Estrai i dati per il valore specifico di "COD"
  data0 <- NDVI_VALUES[NDVI_VALUES$COD == cod_value, ]
  
  # Crea una serie temporale (ts)
  ts_data <- ts(data0$ndvi, start = c(2018, 1), end = c(2023, 8), frequency = 12)
  
  # Applica la funzione DBEST alla serie temporale
  dbest_results <- DBEST(data=ts_data, data.type="cyclical", 
                         algorithm="change detection", 
                         breakpoints.no="default", first.level.shift=0.1, 
                         second.level.shift=0.2, duration=12, 
                         distance.threshold="default", alpha=0.05)
  
  # Trova l'indice del cambiamento più negativo
  worst_change_index <- which.min(dbest_results$Change)
  
  # Estrai l'anno e il mese corrispondenti a questo indice
  worst_change_year <- floor(worst_change_index / 12) + 2018
  worst_change_month <- worst_change_index %% 12
  if (worst_change_month == 0) {
    worst_change_month <- 12
    worst_change_year <- worst_change_year - 1
  }
  
  # Memorizza la data nella lista
  worst_change_dates[[cod_value]] <- paste(worst_change_year, worst_change_month, sep="-")
  
  # Memorizza i risultati in lista
  dbest_results_list[[cod_value]] <- dbest_results
  
  # Aggiorna la barra di avanzamento solo se non ha raggiunto il limite
  if (!pb$finished) {
                    pb$tick()
  }
}

# # Loop attraverso i valori unici di "COD"
# for (cod_value in cod_unique) {
#   # Estrai i dati per il valore specifico di "COD"
#   data0 <- NDVI_VALUES[NDVI_VALUES$COD == cod_value, ]
#   
#   # Crea una serie temporale (ts)
#   ts_data <- ts(data0$ndvi, start = c(2018, 1), end = c(2023, 8), frequency = 12)
#   
#   # Applica la funzione DBEST alla serie temporale
#   dbest_results <- DBEST(data=ts_data, data.type="cyclical", 
#                          algorithm="change detection", 
#                          breakpoints.no="default", first.level.shift=0.1, 
#                          second.level.shift=0.2, duration=12, 
#                          distance.threshold="default", alpha=0.05)
#   # Memorizza i risultati in lista
#   dbest_results_list[[cod_value]] <- dbest_results
#   
#   # # Estrai le date dei punti di rottura
#   # break_dates <- time(bfast$breakpoints$Tt)
#   # 
#   # # Estrai i trend prima e dopo ogni punto di rottura
#   # trends <- coef(bfast$breakpoints$Tt)
#   # 
#   # # Trova le date in cui il trend cambia in negativo
#   # negative_trend_dates <- break_dates[which(diff(trends) < 0)]
#   # 
#   # # Memorizza le date in lista
#   # bfast_results_list[[paste0(cod_value, "_negative_trend_dates")]] <- negative_trend_dates
#   # 
#   # Aggiorna la barra di avanzamento solo se non ha raggiunto il limite
#   if (!pb$finished) {
#     pb$tick()
#   }
# }

file_path <- "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_VALUES/bfast_results_list.rds"

# Salva la lista in formato RDS
saveRDS(dbest_results_list, file_path)

# Imposta l'opzione scipen su un valore elevato per eliminare la notazione esponenziale dei valori
options(scipen = 999)

# Carico il nuovo file con le classi
pixel_trend <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_VALUES/pixels_trend3.shp")

# Filtra le righe con Trnd_Cl nelle classi 1, 2 o 3
cod_class_1_2_3 <- pixel_trend$COD[pixel_trend$Trnd_Cl %in% c(1, 2, 3)]


beast_results_list <- readRDS("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_VALUES/beast_results_list.rds")

# Trova i nomi comuni tra cod_class_1_2_3 e names(beast_results_list)
common_names <- intersect(cod_class_1_2_3, names(beast_results_list))

# Inizializza una nuova lista vuota
matching_results <- list()

# Loop attraverso i nomi comuni
for (name in common_names) {
  # Estrai l'elemento dalla lista originale
  result <- beast_results_list[[name]]
  
  # Assegna l'elemento alla nuova lista con il nome originale
  matching_results[[name]] <- result
}

# Inizializza una lista vuota per memorizzare le date con la massima probabilità di cambiamento decrescente
dates_max_dec_prob <- list()

# Loop attraverso gli elementi della lista
for (name in common_names) {
  result <- beast_results_list[[name]]
  
  # Estrai la probabilità di cambiamento decrescente dal risultato
  dec_prob <- result$trend$dec_cpPr
  
  # Trova l'indice dell'elemento con la massima probabilità
  max_dec_prob_index <- which.max(dec_prob)
  
  # Estrai la data corrispondente all'indice
  date_with_max_prob <- result$trend$dec_cp[max_dec_prob_index]
  
  # Assegna la data alla lista con il nome dell'elemento
  dates_max_dec_prob[[name]] <- date_with_max_prob
}

# Ora dates_max_dec_prob conterrà le date con la massima probabilità di cambiamento decrescente per ciascun elemento

# Definisci una funzione per convertire le date nel formato mese-anno
converti_in_mese_anno <- function(data_decimal) {
  anno <- floor(data_decimal)
  percentuale_anno <- (data_decimal - anno) * 100  # Moltiplica per 100 per ottenere la percentuale
  mese <- ceiling((12 / 100) * percentuale_anno)  # Calcola il mese basato sulla percentuale
  return(paste(anno, mese, sep = "-"))
}

# Converti le date nel formato mese-anno utilizzando lapply per ottenere una lista
date_convertite <- lapply(dates_max_dec_prob, converti_in_mese_anno)


# ----------

# 1. Creare un vettore di nomi degli elementi
element_names <- names(date_convertite)

# 2. Estrai le date convertite e crea un dataframe
date_data <- data.frame(
  Elemento = element_names,
  Data_Convertita = unlist(date_convertite)
)

# 3. Estrai il valore di dec_cpPr da matching_results
dec_cpPr_values <- lapply(matching_results, function(x) max(x$trend$dec_cpPr, na.rm = TRUE))

# Converti la lista in un vettore
dec_cpPr_values <- unlist(dec_cpPr_values)

# 4. Combina i dati in un unico dataframe
final_dataframe <- data.frame(
  Elemento = element_names,
  Data_Convertita = unlist(date_convertite),
  Dec_cpPr = dec_cpPr_values
)

# 5. Esegui un loop attraverso gli elementi di dates_max_dec_prob
for (element_name in names(dates_max_dec_prob)) {
  # Estrai la data massima per l'elemento corrente
  max_date <- dates_max_dec_prob[[element_name]]
  
  # Trova l'indice corrispondente nell'elenco dei dati finali
  index <- which(final_dataframe$Elemento == element_name)
  
  # Assegna la data massima al dataframe final_dataframe
  final_dataframe$Data_Max_Dec_Prob[index] <- max_date
}

# Ora final_dataframe contiene tutti i dati richiesti

final_dataframe <- final_dataframe %>%
  rename(COD = Elemento) 
