library(sf)
library(stats)

# Carica il nuovo file combinato
NDVI_VALUES <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_VALUES/merged_data_ndvi.shp")

data <- NDVI_VALUES[NDVI_VALUES$COD == 'ORTERI1_61', ]

ts <- ts(data$ndvi, start = c(2018, 1), end = c(2023, 8), frequency = 12)

ts13<- stats::filter(ts, rep(1,12)/12)

plot(ts, type = "p")
lines(ts13, lwd=2, col = "red")

### Lowess
plot(ts, type='p', ylab='NDVI', main='nearest neighbor')
lines(smooth.spline(time(ts), ts), col = "blue") # n/100 for seasonal component
lines(supsmu(time(ts), ts, span=0.5), col = "red") # n/2 for trend component

## splines
plot(ts, type='p', ylab='Mortality')
lines(smooth.spline(time(ts), ts), col = "blue")
lines(smooth.spline(time(ts), ts, spar=1), col='red')


# proviamo con la regressione semplice

library(ggplot2)

ts_df <- data.frame(Time = time(ts), ts)

convert_decimal_date <- function(x) {
  year <- floor(x)
  month <- round((x - year) * 12) + 1
  as.Date(paste(year, month, "01", sep = "-"))
}

ts_df$Time <- convert_decimal_date(ts_df$Time)

ts_df$Time <- as.Date(ts_df$Time, format = "%d/%m/%Y")
ts_df$ts <- as.numeric(ts_df$ts)


ggplot(ts_df, aes(x = Time, y = ts)) +
  geom_line() +
  ggtitle("Grafico di ts_df") +
  xlab("Time") +
  ylab("NDVI") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) + 
  theme_minimal()


# ------------------------------------------
library(bfast)
library(strucchange)

ts_stl <- stl(ts, s.window = "periodic")

plot(ts_stl)

ts_bfast <- bfast(ts, h =  0.8, max.iter = 10, season = "harmonic")

plot(ts_bfast,
     type = "components",
    ylim = c(3,max(ts)+1))

plot(ts_bfast$Yt/ts_bfast$output[[1]]$Tt-1)
abline(v=breakdates(ts_bfast$Yt/ts_bfast$output[[1]]$bp.Vt))

# ######################

library(tseries)

# Esegui il test ADF
adf.test(ts_df$ts, alternative = "stationary")

# Interpreta i risultati
# Se il p-value è inferiore a 0.05, la serie temporale è stazionaria (non ha un trend)
# Se il p-value è superiore a 0.05, la serie temporale ha un trend

# Calcola la linea di regressione
regression <- lm(ts ~ time(ts))

# Stampa la pendenza della linea di regressione
cat("La pendenza della linea di regressione è", coef(regression)[2])

# Interpreta i risultati
# Se la pendenza è positiva, il trend è positivo
# Se la pendenza è negativa, il trend è negativo




######### Is there a significant trend?
library(tseries)

## Randomness Test-Runs, 
xx1 = factor(ifelse(ts_df$ts >= median(ts_df$ts),1,0))

runs.test(xx1, a = "less")
##p<0.05 hence the H0 rejected
## time series has a trend





