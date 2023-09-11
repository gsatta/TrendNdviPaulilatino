library(sf)
library(tidyverse)
library(lubridate)
library(forecast)
library(readr)
library(signal)
library(zoo)

# Leggi le date dal file CSV
date <- read.csv("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/date.csv")

# Converti le date in oggetti di tipo Date (se non sono già)
date$Date <- as.Date(date$date)

# Carica il nuovo file combinato
NDVI_sf <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_VALUES/merged_data_ndvi.shp")

# Aggrega i dati mensilmente e crea le serie temporali
NDVI_ts <- NDVI_sf %>%
  group_by(COD) %>%
  mutate(year_month = as.Date(paste0(format(date, "%Y-%m"), "-01"))) %>%
  group_by(COD, year_month) %>%
  summarize(avg_ndvi = mean(ndvi, na.rm = TRUE)) %>%  
  group_map(~ ts(.x$avg_ndvi, start = c(as.numeric(format(min(.x$year_month), "%Y")), as.numeric(format(min(.x$year_month), "%m"))), frequency = 12), .keep = TRUE) %>%
  setNames(unique(NDVI_sf$COD))

smooth_ndvi <- function(ndvi_ts) {
  smoothed_ndvi <- sgolayfilt(ndvi_ts, p = 3, n = 7) # Modifica p e n secondo le tue esigenze
  return(smoothed_ndvi)
}

# Applica sgolayfilt a ciascun gruppo di serie temporali NDVI
smoothed_ndvi_list <- NDVI_ts %>%
  map(~ smooth_ndvi(.x))

# Combina i valori lisciati con le date originali
smoothed_ndvi_with_dates <- map(smoothed_ndvi_list, ~ cbind(date = date, smoothed_value = .x))

# Rimuovi la colonna date.Date
smoothed_ndvi_with_dates <- lapply(smoothed_ndvi_with_dates, function(df) df[, -2])

# Converti la colonna date.date in un oggetto di tipo "Date"
smoothed_ndvi_with_dates[["SOS_CUZZOS_69"]]$date.date <- as.Date(smoothed_ndvi_with_dates[["SOS_CUZZOS_69"]]$date.date, format = "%d/%m/%Y")

# Seleziona la serie temporale per ATZARA_1
atzara_1_series <- smoothed_ndvi_with_dates$ATZARA_1
atzara_1_series$date.date <- as.Date(atzara_1_series$date.date, format = "%d/%m/%Y")

# Accedi ai dati della colonna ATZARA_1 nel data frame NDVI_ts
atzara_1_data <- NDVI_ts$ATZARA_1

# Ora puoi utilizzare $ per accedere ai dati all'interno di atzara_1_data se è una lista o un data frame
# Ad esempio, se vuoi tracciare la colonna "data" in atzara_1_data:
plot(atzara_1_data)

# Converti le date di atzara_1_data nel formato "Date" se non lo sono già
atzara_1_data <- as.Date(atzara_1_data, format = "%d/%m/%Y")

# Plot con la data sull'asse x per ATZARA_1_series
plot(atzara_1_series$date.date, atzara_1_series$smoothed_value, type = "p", xlab = "Date", ylab = "Smoothed Value")
lines(atzara_1_series$date.date, atzara_1_series$smoothed_value, col = "blue")

# Plot con la data sull'asse x per ATZARA_1_data
points(atzara_1_data, atzara_1_data$smoothed_value, type = "p", col = "red")
lines(atzara_1_data$date.date, atzara_1_data$smoothed_value, col = "red")

plot(smoothed_ndvi_with_dates$date.date, smoothed_ndvi_with_dates$smoothed_value, type = "l", col = "red")



decomposed_list <- list()

for (i in seq_along(NDVI_ts)) {
  ts_data <- NDVI_ts[[i]]
  decomposed_data <- stl(ts_data, s.window="periodic")
  decomposed_list[[i]] <- decomposed_data
}

names(decomposed_list) <- names(NDVI_ts)

adjusted_list <- list()

for (i in seq_along(decomposed_list)) {
  decomposed_data <- decomposed_list[[i]]
  seasonal_component <- decomposed_data$time.series[, "seasonal"]
  adjusted_data <- NDVI_ts[[i]] - seasonal_component
  adjusted_list[[i]] <- adjusted_data
}

names(adjusted_list) <- names(NDVI_ts)

# Leggi il file CSV delle date
date_df <- read_csv("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/date.csv", col_names = TRUE)

# Converti la colonna 'date' nel formato desiderato
date_df$date <- as.Date(date_df$date, format = "%d/%m/%Y")
date_df$date <- format(date_df$date, "%Y-%m-%d")

# Creazione del dataframe NDVI_df utilizzando reframe
NDVI_df <- NDVI_sf %>%
  group_by(COD, date) %>%
  reframe(NDVI = ts(ndvi, start = year(min(date_df$date)), frequency = 12))

# Creazione del dataframe utilizzando le date dal file CSV
dataframe <- bind_rows(
  map2(adjusted_list, names(adjusted_list), ~ {
    y <- .y
    data.frame(
      COD = y,
      date = date_df$date,
      adj_value = as.vector(.x))
  })
)

dataframe$date <- as.Date(dataframe$date)

# Unisci il dataframe NDVI_df al tuo dataframe principale
dataframe <- left_join(dataframe, NDVI_df, by = c("COD", "date"))

dataframe$Trend <- dataframe$NDVI - dataframe$adj_value

# Riorganizza le colonne
dataframe <- dataframe %>%
  select(COD, date, NDVI, adj_value, Trend)

# Calcola il summary del modello lineare per ciascun gruppo COD
trend_summaries <- dataframe %>%
  group_by(COD) %>%
  group_map(~summary(lm(formula = Trend ~ date, data = .x)))

# Estrai la colonna COD unica dal dataframe
unique_COD <- unique(dataframe$COD)

# Rinomina gli elementi della lista trend_summaries
trend_summaries <- setNames(trend_summaries, unique_COD)

# Seleziona il COD desiderato
target_COD <- "ATZARA_1"

# Filtra il dataframe per il COD desiderato
filtered_df <- dataframe %>%
  filter(COD == target_COD)

# Estrai il summary del modello lineare per il COD desiderato
summary_for_target_COD <- trend_summaries[[which(names(trend_summaries) == target_COD)]]

# Fai il plot del Trend per il COD desiderato
{
plot(filtered_df$Trend)
lines(filtered_df$Trend)
abline(a = summary_for_target_COD$coefficients[1, 1], b = summary_for_target_COD$coefficients[2, 1], col = 'red')
}
