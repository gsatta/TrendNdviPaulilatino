library(sf)
library(stats)

# Carica il nuovo file combinato
NDVI_VALUES <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_VALUES/merged_data_ndvi.shp")

library(tidyverse)
library(tsibble)
ndvi_ts2 <- NDVI_VALUES %>%
  group_by(COD) %>%
  summarise(date = list(date), ndvi = list(ndvi)) %>%
  mutate(ts = map2(date, ndvi, ~ ts(.y, start = c(year(min(.x)), month(min(.x))), end = c(2023, 7), frequency = 12))) %>%
  select(COD, ts) %>%
  pull(ts) %>%
  setNames(., unique(NDVI_VALUES$COD))

library(greenbrown)


plot(TSGFlinear(ndvi_ts2[["ORTERI2_70"]], interpolate = TRUE))


ttt <- TrendSTL(ndvi_ts2[["ORTERI2_68"]], mosum.pval=1)

plot(ttt)

# Provo un altro pacchetto
library(trend)

# Calcola il trend della serie temporale di ogni serie temporale
trends <- lapply(ndvi_ts2, function(x) mk.test(x))

# Individua i punti di cambiamento
change_points <- lapply(ndvi_ts2, function(x) pettitt.test(x))


library(ggplot2)

# Crea un grafico a linee per la serie temporale ORTERI2_45
x <- ndvi_ts2[["ORTERI2_2"]]
df <- data.frame(time = time(x), value = as.numeric(x))
p <- ggplot(df, aes(x = time, y = value)) +
  geom_line() +
  ggtitle("ORTERI2_2")

p

# Aggiungi una linea verticale per indicare i punti di cambiamento
cp <- change_points[["ORTERI2_2"]]
if (!is.na(cp$cp)) {
  p <- p + geom_vline(xintercept = cp$cp, linetype = "dashed")
}

# Aggiungi una linea di tendenza
trend <- trends[["ORTERI2_2"]]
p <- p + geom_abline(intercept = trend$intercept, slope = trend$slope)

# Visualizza il grafico
p




x <- ndvi_ts2[["ORTERI2_45"]]
cp <- pettitt.test(x)


# PROV ALTRO PACCHETTOI

library(forecast)

ndvi_ts2_filt <-ma(ndvi_ts2[["ORTERI2_45"]], order = 1)

ndvi_ts2_filt

plot(ndvi_ts2_filt)
lines(ndvi_ts2_filt, col="red")

# elimino il trend stagionale
# ipotizzo dati mensili e la presenza di stagionalità
dx <- diff(ndvi_ts2_filt, lag=12)
ts.plot(dx, main="Tendenza utenti destagionalizzata")


# ALTRO PACCHETTO

# decompongo la serie temporale. Ho scelto una decomposizione 
# moltiplicativa, ovviamente avrei
# potuto scegliere una additiva
componenti <- decompose(ndvi_ts2[["ORTERI2_45"]], type ="multiplicative")
names(componenti)
# esploro nel grafico le componenti della serie storica
plot(componenti)


# uso stl per un decompose di tipo LOESS
componenti_loess <- stl(ndvi_ts2[["ORTERI2_45"]], s.window="periodic")
names(componenti_loess)
plot(componenti_loess)




















# Inizializza una lista vuota per i risultati
results <- list()

# Itera su ogni serie temporale nella lista
for (i in seq_along(ndvi_ts2)) {
  # Calcola il trend per la serie temporale corrente
  trend <- Trend(ndvi_ts2[[i]], method = "STM", mosum.pval=1)
  
  # Aggiungi i risultati alla lista dei risultati
  results[[i]] <- list(trend = trend)
}

# Aggiungi i risultati alla lista originale
ndvi_ts_results <- ndvi_ts2

names(ndvi_ts_results) <- paste0(names(ndvi_ts_results), "_trend")
ndvi_ts_results[] <- results

# Visualizza i risultati
ndvi_ts_results

# Installa e carica il pacchetto ggplot2
library(ggplot2)

# Inizializza una lista vuota per i dati del plot
plot_data <- list()

# Itera su ogni serie temporale nella lista
for (i in seq_along(ndvi_ts_results)) {
  # Estrai i dati del trend per la serie temporale corrente
  trend_data <- ndvi_ts_results[[i]]$trend
  
  # Estrai i dati dei segmenti di breakpoint
  segments_data <- trend_data$Segments
  
  # Crea un tibble con i dati dei segmenti di breakpoint
  segments_tibble <- tibble(
    start = segments_data$Start,
    end = segments_data$End,
    slope = segments_data$Slope,
    series = names(ndvi_ts_results)[i]
  )
  
  # Aggiungi il tibble alla lista dei dati del plot
  plot_data[[i]] <- segments_tibble
}

# Combina tutti i tibbles in un unico tibble
plot_data <- bind_rows(plot_data)

# Crea il plot utilizzando ggplot2
ggplot(plot_data, aes(x = start, xend = end, y = slope, yend = slope, color = series)) +
  geom_segment() +
  labs(x = "Time", y = "Slope", title = "Segmenti di breakpoint per ogni serie temporale")
