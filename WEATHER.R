

#  Carica i file csv contenenti i dati di Temperature e Precipitazioni

Prec <- read.csv("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/MODELLO/CSV/Weather/precipitationTimeSeries-04-10-2023.csv", header = T)
Temp <- read.csv("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/MODELLO/CSV/Weather/temperatureTimeSeries-04-10-23.csv", header = T)

# Converti le colonne date dei due dataframe in formato Data
Prec$date <- as.Date(Prec$date)
Temp$date <- as.Date(Temp$date)

write.csv(Prec, "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/MODELLO/CSV/Weather/precipitationTimeSeries_date.csv")
write.csv(Temp, "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/MODELLO/CSV/Weather/temperatureTimeSeries_date.csv")


View(Prec)
View(Temp)

# Crea i plot delle due serie temporali di precipitazioni  e temperature
par(mfrow = c(2,1))
plot(Temp$date, Temp$tempMean, type = "l", main  ="Mean Temperature in °C")
plot(Prec$date, Prec$precipMean, type = "l", main = " Mean precipitation in mm")

# Carica le librerie necessarie
library(ggplot2)
library(dplyr)

# Unisci i dataframe 'Prec' e 'Temp' sulla colonna 'date'
df <- merge(Prec, Temp, by = "date")

# Crea una nuova colonna 'month' che contiene solo l'anno e il mese di 'date'
df$month <- format(df$date, "%Y-%m")

# Raggruppa i dati per 'month', calcola la somma delle precipitazioni e la media della temperatura
df_monthly <- df %>%
  group_by(month) %>%
  summarise(precipSum = sum(precipMean, na.rm = TRUE), tempMean = mean(tempMean, na.rm = TRUE))

# Converti la colonna 'month' in un formato di data
df_monthly$month <- as.Date(paste(df_monthly$month, "01", sep="-"), format="%Y-%m-%d")

write.csv(df_monthly, "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/CSV/Weather/df_monthly_weather.csv" )


df_monthly <- read_csv("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/CSV/Weather/df_monthly_weather.csv")

# Crea il grafico --> è questo il grafico corretto
ggplot(df_monthly, aes(x = month)) +
  geom_smooth(aes(y= precipSum, color = "Precipitation"), method = "loess", span = 0.1, se = FALSE) +
  geom_smooth(aes(y= tempMean, color = "Temperature"), method = "loess", span = 0.1, se = FALSE) +
  scale_y_continuous("Average Temperature (°C)", sec.axis = sec_axis(~.*2, name = "Precipitation  (mm)")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values=c("Precipitation"="blue", "Temperature"="red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
  labs(title = "Monthly Precipitation and Average Temperature") +
  guides(color=guide_legend(title=NULL))






