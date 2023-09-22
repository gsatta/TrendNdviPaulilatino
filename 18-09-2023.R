# Carica i pacchetti neccessari
library(sf)
library(dplyr)
library(lubridate)
library(DBEST)
library(progress)
library(trend)
library(mblm)
library(RobustLinearReg)

# Carica il file dell'ndvi
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
  
  # Calcola la deviazione standard (SD)
  sd_value <- sd(ts)
  
  # Esegui il test non parametrico di Mann-Kendall (MK)
  mk_test <- smk.test(ts)
  
  # Converti le date in numeri (numero di giorni dalla data minima)
  data0$date_numeric <- as.numeric(data0$date - min(data0$date))
  
  # Calcola Theil-Sen's slope (Q) utilizzando la data numerica
  theil_sen_fit <- mblm(ndvi ~ date_numeric, data = data0)
  
  # Estrazione della pendenza e l'intercetta
  slope_mblm <- theil_sen_fit$coefficients[2]
  intercept_mblm <- theil_sen_fit$coefficients[1]
  
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
  
  # Imposta i margini del grafico
  par(mar = c(5, 4.5, 4, 1),  mfrow = c(1,1))
  
  # Crea il grafico di dispersione dei dati
  plot(data0$date,data0$ndvi, type = "p", xlab = "Date", ylab = "NDVI values")
  
  # Sovrapponi la linea retta basata sui coefficienti del modello di regressione
  abline(a = intercept, b = slope, col = "red")
  
  # Sovrapponi la linea retta basata sui coeficienti del modello di Theil-Sen 
  abline(intercept_mblm, slope_mblm, col= "orange")
  
  # Linea di tendenza 1 (colore blu)
  lines(smooth.spline(data0$date, data0$ndvi), col = "blue")
  
  # Linea di tendenza 2 (colore nero)
  lines(trend_df$date, trend_df$ndvi, type = "l", col = "black")
  
  # Aggiungi il titolo e il sottotitolo
  title(main = area)
  mtext(eq, side = 3, line = 0.3)

    # Salva i risultati e il grafico nella lista
  results_list[[cod]] <- list(intercept = intercept, slope = slope, area = area, plot = recordPlot(), sd = sd_value, mk = mk_test)
  
  # Aggiorna la barra di avanzamento solo se non ha raggiunto il limite
  if (!pb$finished) {
    pb$tick()
  }
}

saveRDS(results_list, file = "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/SCRIPT/TrendNdviPaulilatino/Risultati/results_list.rds")


results_list <- readRDS("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/SCRIPT/TrendNdviPaulilatino/Risultati/results_list.rds")


# --------------------
# Individuo gli olivastri che, secondo il test di MannKendall hanno un trend 
# statisticamente significativo

# Inizializza una lista vuota per memorizzare i nomi dei COD
cod_names95 <- list()

# Esegui il ciclo for su ogni elemento in results_list
for (cod in names(results_list)) {
  # Estrai il valore p dell'MK
  pvalue <- results_list[[cod]]$mk$p.value
  
  # Controlla se pvalue esiste e se è inferiore a 0.05
  if (length(pvalue) > 0 && pvalue < 0.05) {
    # Se il valore p esiste ed è inferiore a 0.05, aggiungi il nome del COD alla lista
    cod_names95 <- c(cod_names95, cod)
  }
}

# Stampa i nomi dei COD con un valore p dell'MK inferiore a 0.05
print(cod_names95)

# Inizializza una lista vuota per salvare i risultati che hanno un p value<0.05
extracted_results95 <- list()

# Esegui il ciclo for su ogni nome in cod_nnames
for (name in cod_names95) {
  # Controlla se il nome è presente in results_list
  if (name %in% names(results_list)) {
    # Se il nome è presente, aggiungi i risultati corrispondenti a extracted_results95
    extracted_results95[[name]] <- results_list[[name]]
  }
}

# -----------------------
# Inizializza una lista vuota per memorizzare i nomi dei COD
cod_names90 <- list()

# Esegui il ciclo for su ogni elemento in results_list
for (cod in names(results_list)) {
  # Estrai il valore p dell'MK
  pvalue <- results_list[[cod]]$mk$p.value
  
  # Controlla se pvalue esiste e se è inferiore a 0.1
  if (length(pvalue) > 0 && pvalue < 0.1) {
    # Se il valore p esiste ed è inferiore a 0.1, aggiungi il nome del COD alla lista
    cod_names90 <- c(cod_names90, cod)
  }
}

# Inizializza una lista vuota per salvare i risultati che hanno un p value<0.05
extracted_results90 <- list()

# Esegui il ciclo for su ogni nome in cod_nnames
for (name in cod_names90) {
  # Controlla se il nome è presente in results_list
  if (name %in% names(results_list)) {
    # Se il nome è presente, aggiungi i risultati corrispondenti a extracted_results95
    extracted_results90[[name]] <- results_list[[name]]
  }
}

print(cod_names90)



library(bfast)

fit <- bfast(ts, h = 0.15, season = "harmonic", max.iter = 5)

plot(fit, type = "all")
