library(dplyr)
library(purrr)
library(tidyr)
library(sf)
library(Rbeast)

# Carica il file dell'ndvi
NDVI_VALUES <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_VALUES/merged_data_ndvi.shp")

data0 <- NDVI_VALUES[NDVI_VALUES$COD == 'MAGRINA_E_FIGU_22', ]

ts <- ts(data0$ndvi, start = c(2018, 1), end = c(2023, 8), frequency = 12)

beastRun <- beast(ts, period = 12, season = "harmonic")

plot(beastRun)




# ----------------------------
library(forecast)


# Carica il file dell'ndvi
NDVI_VALUES <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_VALUES/merged_data_ndvi.shp")

# Ottenere i valori unici nella colonna "COD"
cod_unique <- unique(NDVI_VALUES$COD)

# Inizializza una lista per archiviare i risultati
beast_results_list <- list()

# Loop attraverso i valori unici di "COD"
for (cod_value in cod_unique) {
  # Estrai i dati per il valore specifico di "COD"
  data0 <- NDVI_VALUES[NDVI_VALUES$COD == cod_value, ]
  
  # Crea una serie temporale (ts)
  ts_data <- ts(data0$ndvi, start = c(2018, 1), end = c(2023, 8), frequency = 12)
  
  # Applica la funzione beast alla serie temporale
  beastRun <- beast(ts_data, period = 12, season = "harmonic")
  
  # Memorizza i risultati in lista
  beast_results_list[[cod_value]] <- beastRun
}

file_path <- "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_VALUES/beast_results_list.rds"

# Salva la lista in formato RDS
saveRDS(beast_results_list, file_path)

# ----------------------------

beast_results_list <- readRDS("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_VALUES/beast_results_list.rds")

# Crea una lista vuota per memorizzare le date
max_trend_change_dates <- list()

# Itera su ogni elemento della lista dei risultati
for(i in 1:length(beast_results_list)) {
  # Estrae l'elemento corrente
  current_result <- beast_results_list[[i]]
  
  # Controlla se l'elemento 'trend' esiste nei risultati correnti
  if("trend" %in% names(current_result)) {
    # Estrae l'elemento 'trend'
    trend <- current_result$trend
    
    # Controlla se 'dec_cp' e 'dec_cpPr' esistono in 'trend'
    if(all(c("dec_cp", "dec_cpPr") %in% names(trend))) {
      # Estrae le date dei cambiamenti di punto e le loro probabilità
      dec_cp <- trend$dec_cp
      dec_cpPr <- trend$dec_cpPr
      
      # Trova l'indice della data con la probabilità massima di cambiamento del trend
      max_prob_index <- which.max(dec_cpPr)
      
      # Estrae la data con la probabilità massima di cambiamento del trend
      max_prob_date <- dec_cp[max_prob_index]
      
      # Aggiunge il nome dell'elemento e la data alla lista
      max_trend_change_dates[[names(beast_results_list)[i]]] <- max_prob_date
    }
  }
}

# Stampa le date dei cambiamenti di punto con la probabilità massima di cambiamento del trend
print(max_trend_change_dates)































# # Estrai i nomi degli elementi nella lista
# nomi_elementi <- names(beast_results_list)
# 
# # Estrai le prime 4 lettere da ciascun nome di elemento
# prime_4_lettere <- substr(nomi_elementi, 1, 4)
# 
# # Trova gli unici gruppi di prime 4 lettere
# gruppi_unici <- unique(prime_4_lettere)
# 
# # Inizializza una nuova lista vuota per contenere gli elementi medi
# nuova_lista <- list()
# 
# # Calcola la media dei dati per ciascun gruppo di prime 4 lettere
# for (grp in gruppi_unici) {
#   indici <- which(prime_4_lettere == grp)
#   dati_media <- colMeans(sapply(indici, function(i) beast_results_list[[i]]$data))
#   
#   # Crea un nuovo elemento con la media dei dati e altre informazioni
#   nuovo_elemento <- list(
#     time = beast_results_list[[indici[1]]]$time,  # Usa il time da uno degli elementi
#     data = dati_media,
#     marg_lik = mean(sapply(indici, function(i) beast_results_list[[i]]$marg_lik)),
#     R2 = mean(sapply(indici, function(i) beast_results_list[[i]]$R2)),
#     RMSE = mean(sapply(indici, function(i) beast_results_list[[i]]$RMSE)),
#     sig2 = mean(sapply(indici, function(i) beast_results_list[[i]]$sig2)),
#     trend = beast_results_list[[indici[1]]]$trend,  # Usa il trend da uno degli elementi
#     season = beast_results_list[[indici[1]]]$season  # Usa il season da uno degli elementi
#   )
#   
#   # Aggiungi il nuovo elemento alla nuova lista
#   nuova_lista[[grp]] <- nuovo_elemento
# }
# 
# 
# library(ggplot2)
# 
# # Creare un dataframe dai dati in nuova_lista
# df <- do.call(rbind, nuova_lista)
# 
# # Plottare i dati mediati
# ggplot(df, aes(x = grp, y = dati_media)) +
#   geom_bar(stat = "identity", fill = "blue") +  # Grafico a barre
#   labs(x = "Gruppo", y = "Media dei dati") +  # Etichette degli assi
#   ggtitle("Media dei dati per gruppo")  # Titolo del gra




# Crea una lista vuota per memorizzare le date dei cambiamenti di tendenza
cambiamenti_tendenza <- list()

# Scorrere attraverso ciascun elemento nella lista
for (punto in beast_results_list) {
  # Estrai le date dei cambiamenti di tendenza dal campo "cp"
  date_cambiamenti <- punto$trend$cp
  
  # Aggiungi le date dei cambiamenti di tendenza alla lista
  cambiamenti_tendenza[[names(punto)]] <- date_cambiamenti
}

# Ora la lista cambiamenti_tendenza contiene le date dei cambiamenti di tendenza per ciascun punto




