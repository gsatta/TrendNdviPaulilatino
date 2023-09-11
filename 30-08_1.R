
library(sf)

# Imposta l'opzione scipen su un valore elevato per eliminare la notazione esponenziale dei valori
options(scipen = 999)

# Carica il file dei valori dell'ndvi medio per ogni area
ndvi_avg <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_VALUES/ndvi_avg_areas.shp")

# Carica il pacchetto lubridate
library(lubridate)

# Aggiungi le colonne per il mese e l'anno
ndvi_avg$Mese <- month(ndvi_avg$date)
ndvi_avg$Anno <- year(ndvi_avg$date)

# Crea un modello di regressione lineare con effetti fissi per il mese
modello <- lm(mean_NDVI ~ Mese + Anno, data = ndvi_avg)

# Esegui un'analisi della varianza sul modello
anova(modello)

# Visualizza i risultati del modello di regressione lineare
summary(modello)

# Crea un modello di regressione lineare con effetti fissi per il mese e l'AREA
modello2 <- lm(mean_NDVI ~ Mese + Anno + AREA, data = ndvi_avg)

# Visualizza i risultati del modello di regressione lineare
summary(modello2)

# Crea un grafico Q-Q dei residui
qqnorm(modello2$residuals)
qqline(modello2$residuals)

# Esegui il test di Shapiro-Wilk sui residui
shapiro.test(modello2$residuals)


# Carica il pacchetto nlme
library(nlme)

# Crea un modello di regressione lineare separato per ogni AREA
modelli <- lmList(mean_NDVI ~ Mese + Anno | AREA, data = ndvi_avg)

# Visualizza i risultati dei modelli
summary(modelli)




# Carica il pacchetto lme4
library(lme4)

# Crea un modello lineare misto con effetti fissi per il mese e l'anno e effetti casuali per l'AREA
modello3 <- lmer(mean_NDVI ~ Mese + Anno + (1 | AREA), data = ndvi_avg)

# Visualizza i risultati del modello
summary(modello3)

# Crea un grafico dei residui
plot(modello3)

# Crea un grafico dei valori previsti rispetto ai valori osservati
plot(fitted(modello3), ndvi_avg$mean_NDVI)
abline(0, 1)










# Crea un grafico delle tendenze temporali usando ggplot2 e aggiungi etichette per ciascuna area
plot <- ggplot(ndvi_avg, aes(x = date, y = mean_NDVI, group = AREA, color = AREA)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  scale_color_brewer(palette = "Set3") +
  labs(title = "Tendenze temporali dell'NDVI per ciascuna area",
       x = "Date",
       y = "NDVI") +
  theme_minimal()