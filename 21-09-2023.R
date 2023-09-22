
library(sf)
library(dplyr)
library(lubridate)
library(DBEST)
library(progress)
library(trend)
library(mblm)
library(RobustLinearReg)
library(readr)
library(RobustLinearReg)

# Carica il nuovo file combinato
NDVI_VALUES <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_VALUES/merged_data_ndvi.shp")

# Imposta l'opzione scipen su un valore elevato per eliminare la notazione esponenziale dei valori
options(scipen = 999)

# Ottieni l'elenco unico dei COD
cod_list <- unique(NDVI_VALUES$COD)

# Inizializza una lista per memorizzare i risultati
results_list <- list()

# Crea una nuova barra di avanzamento
pb <- progress_bar$new(total = length(cod_list))

# Esegui il ciclo for su ogni COD
for (cod in cod_list) {
  # Seleziona i dati per il COD corrente
  data0 <- NDVI_VALUES[NDVI_VALUES$COD == cod, ]
  
  ts <- ts(data0$ndvi, start = c(2018, 1), end = c(2023, 8), frequency = 12)
  fit <- lm(ndvi ~ date, data = data0)
  
  # Esegui il test non parametrico di Mann-Kendall (MK)
  mk_test <- smk.test(ts)
  
  pettitt_test <- pettitt.test(ts)
  
  valore_K <- unname(pettitt_test$estimate)
  
  # Converti le date in numeri (numero di giorni dalla data minima)
  data0$date_numeric <- as.numeric(data0$date - min(data0$date))
  
  # Calcola Theil-Sen's slope (Q) utilizzando la data numerica
  # theil_sen_fit <- mblm(ndvi ~ date_numeric, data = data0)
  
  theil_sen_fit <- theil_sen_regression(ndvi ~ date, data = data0)
  
  # Estrazione della pendenza e l'intercetta
  slope_mblm <- theil_sen_fit$coefficients[2]
  intercept_mblm <- theil_sen_fit$coefficients[1]
  
  eq_theil = paste0(" slope = ", round(slope_mblm, 8))
  
  # Estrai Q
  Q <- coef(theil_sen_fit)[2]
  
  # Estrai i coefficienti
  intercept <- coef(fit)[1]
  slope <- coef(fit)[2]
  
  area <- unique(data0$COD)
  
  # Equazione della linea di regressione
  eq = paste0(" slope = ", round(slope, 8))
  
  dbest_analysis <- DBEST::DBEST(data = ts, data.type = "cyclical", algorithm = "change detection", breakpoints.no= 10, first.level.shift= 0.1, second.level.shift=0.2, duration=12, distance.threshold="default", alpha=0.05, plot="on" )
  
  trend_dbest <- dbest_analysis$Fit
  
  trend_df <- as.data.frame(trend_dbest, row.names = FALSE)
  colnames(trend_df) <- "ndvi"
  
  trend_df$date <- data0$date
  
  min_date <- min(data0$date)
  max_date <- max(data0$date)
  
  # Imposta i margini del grafico
  par(mar = c(5, 4.5, 4, 1),  mfrow = c(1,1))
  
  # Crea il grafico di dispersione dei dati
  plot(data0$date, data0$ndvi, type = "p", xlab = "Date", ylab = "NDVI values", xlim = c(min_date, max_date))
  
  # Sovrapponi la linea retta basata sui coeficienti del modello di Theil-Sen 
  abline(theil_sen_fit, col= "red")
  
  # Linea di tendenza 1 (colore blu)
  lines(smooth.spline(data0$date, data0$ndvi), col = "blue")
  
  # Linea di tendenza 2 (colore nero)
  lines(trend_df$date, trend_df$ndvi, type = "l", col = "black")
  
  # Aggiungi il titolo e il sottotitolo
  title(main = area)
  mtext(eq_theil, side = 3, line = 0.3)
  
  # Salva i risultati e il grafico nella lista
  results_list[[cod]] <- list(intercept = intercept, slope = slope, area = area, plot = recordPlot(), mk = mk_test, k_pettit.test = valore_K )
  
  # Aggiorna la barra di avanzamento solo se non ha raggiunto il limite
  if (!pb$finished) {
    pb$tick()
  }
}

 
