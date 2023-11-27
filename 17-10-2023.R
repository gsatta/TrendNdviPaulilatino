library(sf)
library(lubridate)
library(readr)
library(DBEST)
library(progress)

# Importa il dataframe
dataframe <- read_csv("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/CSV/results_df_class.csv")

# Carica il file dell'ndvi
NDVI_VALUES_0 <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_VALUES/merged_data_ndvi.shp")

# Crea un nuovo dataframe escludendo le date del 2023
NDVI_VALUES <- NDVI_VALUES_0 %>% filter(year(date) != 2023)

# Estrai le righe dove Trend_Class è diverso da 0
filtered_df <- subset(dataframe, Trend_Class != 0)

# Estrai la colonna "COD" da filtered_df
cod_names_0 <- filtered_df$COD

# Inizializza una lista per archiviare i risultati
dbest_results_list_0 <- list()

# Crea una nuova barra di avanzamento
pb <- progress_bar$new(total = length(cod_names_0))

# Loop attraverso i valori di "COD" in cod_names_0
for (cod_value in cod_names_0) {
  # Estrai i dati per il valore specifico di "COD"
  data0 <- NDVI_VALUES[NDVI_VALUES$COD == cod_value, ]
  
  # Crea una serie temporale (ts)
  ts_data <- ts(data0$ndvi, start = c(2018, 1), end = c(2022, 12), frequency = 12)
  
  # Applica la funzione dbest
  dbest <- DBEST(data=ts_data, data.type="cyclical", 
                 algorithm="change detection", 
                 change.magnitude	= 0.05, first.level.shift=0.1, 
                 second.level.shift=0.2, duration=12, 
                 distance.threshold="default", alpha=0.05)
  
  # Memorizza i risultati nella nuova lista
  dbest_results_list_0[[cod_value]] <- dbest
  
  # Aggiorna la barra di avanzamento solo se non ha raggiunto il limite
  if (!pb$finished) {
    pb$tick()
  }
}

library(dplyr)

library(dplyr)

library(dplyr)

# Crea un dataframe vuoto con le colonne necessarie
df <- data.frame(
  BreakpointNo = integer(),
  SegmentNo = numeric(),
  Start = character(),
  Duration = character(),
  End = character(),
  Change = character(),
  ChangeType = character(),
  Significance = character(),
  Fit = list(),
  Data = list(),
  Trend = list(),
  Seasonal = list(),
  Remainder = list(),
  f_local = list(),
  stringsAsFactors = FALSE
)

# Combinare i dati da ciascun elemento della lista
for (i in 1:length(dbest_results_list_0)) {
  # Verifica la lunghezza dei dati (ad esempio 60 righe)
  if (length(dbest_results_list_0[[i]]$Data) == 60) {
    df <- bind_rows(df, data.frame(
      BreakpointNo = dbest_results_list_0[[i]]$BreakpointNo,
      SegmentNo = dbest_results_list_0[[i]]$SegmentNo,
      Start = dbest_results_list_0[[i]]$Start,
      Duration = dbest_results_list_0[[i]]$Duration,
      End = dbest_results_list_0[[i]]$End,
      Change = dbest_results_list_0[[i]]$Change,
      ChangeType = dbest_results_list_0[[i]]$ChangeType,
      Significance = dbest_results_list_0[[i]]$Significance,
      Fit = list(unlist(dbest_results_list_0[[i]]$Fit)),
      Data = list(unlist(dbest_results_list_0[[i]]$Data)),
      Trend = list(unlist(dbest_results_list_0[[i]]$Trend)),
      Seasonal = list(unlist(dbest_results_list_0[[i]]$Seasonal)),
      Remainder = list(unlist(dbest_results_list_0[[i]]$Remainder)),
      f_local = list(unlist(dbest_results_list_0[[i]]$f_local))
    ))
  }
}

# Resetta i nomi delle colonne del dataframe se necessario
# names(df) <- c("BreakpointNo", "SegmentNo", "Start", "Duration", "End", "Change", "ChangeType", "Significance", "Fit", "Data", "Trend", "Seasonal", "Remainder", "f_local")


