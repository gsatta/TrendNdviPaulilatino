library(dplyr)
library(purrr)
library(tidyr)
library(sf)
library(DBEST)
library(progress)
library(readr)
library(ggplot2)
library(lubridate)
library(gridExtra)


# # Carica il file dell'ndvi
# NDVI_VALUES <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_VALUES/merged_data_ndvi.shp")
# 
# # Ottenere i valori unici nella colonna "COD"
# cod_unique <- unique(NDVI_VALUES$COD)
# 
# data0 <- NDVI_VALUES[NDVI_VALUES$COD == "TROGOS_1",]
# 
# # Crea una serie temporale (ts)
# ts_data <- ts(data0$ndvi, start = c(2018, 1), end = c(2023, 8), frequency = 12)
# 
# # Applica la funzione DBEST alla serie temporale
# dbest_results <- DBEST(data=ts_data, data.type="cyclical", 
#                      algorithm="change detection", 
#                      breakpoints.no="1", first.level.shift=0.1, 
#                      second.level.shift=0.2, duration=12, 
#                      distance.threshold="default", alpha=0.05, plot="on")



# ---------
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

# Crea una nuova lista vuota per gli elementi filtrati
dbest_results_list <- list()

# Loop attraverso gli elementi della lista originale
for (cod_value in names(dbest_results_list_0)) {
  dbest_result <- dbest_results_list_0[[cod_value]]
  
  # Accedi ai valori specifici all'interno di dbest_result
  breakpoint_no <- dbest_result$BreakpointNo
  change_values <- dbest_result$Change
  
  # Controlla i criteri di filtro basati su BreakpointNo e Change
  if (length(breakpoint_no) > 0 && any(breakpoint_no > 0) && any(change_values < 0)) {
    # Aggiungi l'elemento alla nuova lista
    dbest_results_list[[cod_value]] <- dbest_result
  }
}

# Verifica la nuova lista
# str(dbest_results_list)

# Inizializza una lista per archiviare le date di maggiore variazione negativa
negative_change_dates <- list()

# Loop attraverso i risultati di dbest
for (cod_value in names(dbest_results_list)) {
  # Estrai i risultati dbest per il valore specifico di "COD"
  dbest <- dbest_results_list[[cod_value]]
  
  # Escludi l'indice che si riferisce alla data 2018-01
  dbest$f_local[1] <- NA
  
  # Trova l'indice della maggiore variazione negativa all'inizio
  min_change_index_start <- which.min(dbest$f_local)
  
  # Trova l'indice della maggiore variazione negativa alla fine
  min_change_index_end <- which.min(dbest$f_local[(min_change_index_start + 1):length(dbest$f_local)]) + min_change_index_start
  
  # Calcola l'anno e il mese corrispondenti all'indice di inizio
  year_start <- floor((min_change_index_start - 1) / 12) + 2018
  month_start <- ((min_change_index_start - 1) %% 12) + 1
  
  # Calcola l'anno e il mese corrispondenti all'indice di fine
  year_end <- floor((min_change_index_end - 1) / 12) + 2018
  month_end <- ((min_change_index_end - 1) %% 12) + 1
  
  # Memorizza le date di inizio e fine in lista
  negative_change_dates[[cod_value]] <- list(
    start = paste(year_start, month_start, sep = "-"),
    end = paste(year_end, month_end, sep = "-")
  )
}


 # ------------

# Carica il csv delle date delle immagini satellitari
dates <- read_csv("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/date.csv")

# Filtra le date rimuovendo il 2023
dates <- dates %>% filter(year(date) != 2023)

dates$date <- as.Date(dates$date) 

dates$YearMonth <- format(dates$date, "%Y-%m")
dates$YearMonth <- ym(dates$YearMonth)

# Crea una lista vuota per salvare i grafici
plot_list <- list()

# Crea una nuova barra di avanzamento
pb <- progress_bar$new(total = length(negative_change_dates))

# Itera su ogni elemento della lista
for (i in names(dbest_results_list)) {
  
  # Estrai l'elemento corrente
  current_element <- dbest_results_list[[i]]
  
  # Estrai il trend fittato
  fitted_trend <- current_element$Trend
  
  # Crea un dataframe con i dati del trend fittato e NDVI originali
  df <- data.frame(Time = dates$date, Trend = as.numeric(fitted_trend))

  # Crea il grafico con ggplot
  p <- ggplot(df, aes(x = Time)) +
    geom_line(aes(y = Trend), color = "blue") +
    ggtitle(i) +
    ylim(0.4, 0.8)
  
  # Estrai la data del cambiamento negativo per l'elemento corrente
  if (i %in% names(negative_change_dates)) {
    negative_change_date <- as.Date(paste0(negative_change_dates[[i]], "-01"), format = "%Y-%m-%d")
    
    # Crea un dataframe separato per la data del cambiamento negativo
    df_change <- data.frame(Time = negative_change_date)
    
    # Aggiungi il dataframe al grafico come un altro layer
    p <- p + geom_vline(data = df_change, aes(xintercept = Time), color = "red", linetype = "dashed")
  }

  # Salva il grafico nella lista
  plot_list[[i]] <- p
  
  # Aggiorna la barra di avanzamento solo se non ha raggiunto il limite
  if (!pb$finished) {
    pb$tick()
  }
}

# Crea un vettore vuoto per i nomi degli elementi
element_names <- c()

# Crea un vettore vuoto per le date di cambiamento
start_dates <- c()
end_dates <- c()

# Itera su ogni elemento della lista
for (i in names(negative_change_dates)) {
  
  # Aggiungi il nome dell'elemento al vettore dei nomi
  element_names <- c(element_names, i)
  
  # Estrai le date di inizio e fine dall'elemento corrente della lista
  current_dates <- negative_change_dates[[i]]
  
  # Aggiungi le date di inizio e fine ai vettori delle date
  start_dates <- c(start_dates, current_dates$start)
  end_dates <- c(end_dates, current_dates$end)
}

# Crea un nuovo dataframe con i nomi degli elementi e le date di cambiamento
final_dataframe <- data.frame(COD = element_names, start = start_dates, end = end_dates)

# Rimuovi l'ultimo _ e i numeri dalla colonna COD per estrarre il nome dell'area
final_dataframe$AREA <- gsub("_\\d+$", "", final_dataframe$COD)

library(ggplot2)
library(plotly)

# Concatena COD e Dt_Cnvr in una singola stringa per il popup
final_dataframe$PopupText <- paste("COD:", final_dataframe$COD, "<br>",
                                   "ChangeDate:", final_dataframe$ChangeDate)

library(dplyr)
library(lubridate)

# Calcola il conteggio di ogni combinazione di 'ChangeDate' e 'AREA'
final_dataframe <- final_dataframe %>%
  group_by(start, AREA) %>%
  mutate(Count_start = n()) %>%
  ungroup() %>% 
  group_by(end, AREA) %>%
  mutate(Count_end = n()) %>%
  ungroup()

# Crea il tuo grafico ggplot2 con il testo concatenato come popup
p <- ggplot(data = final_dataframe, aes(x = start, y = AREA, text = PopupText)) +
  geom_point(aes(size = Count_start)) + 
  labs(x = "DATA", y = "AREA") +
  scale_size(range = c(1,10)) + 
  theme_minimal() 

# Trasforma il grafico ggplot2 in un grafico interattivo con plotly
interactive_plot <- ggplotly(p)

# Visualizza il grafico interattivo
interactive_plot

# ----------------
# Crea una nuova colonna 'IsMax' che indica se il conteggio è il massimo per ogni 'AREA'
final_dataframe <- final_dataframe %>%
  group_by(AREA) %>%
  mutate(IsMax = ifelse(Count_start == max(Count_start), "Max", "NotMax")) %>%
  ungroup()

write_csv(final_dataframe, "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/CSV/Weather/final_dataframe.csv" )

p <- ggplot(data = final_dataframe, aes(x = start, y = AREA, text = PopupText)) +
  geom_text(aes(label = Count_start, color = IsMax), vjust = +0.4) +
  scale_color_manual(values = c("Max" = "red", "NotMax" = "black")) +
  labs(x = "DATA", y = "AREA", color = "Legenda") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(limits = as.Date(c("2018-01-01", max(final_dataframe$start))), 
               date_breaks = "1 year", date_labels = "%Y") +
  theme_bw()
  
# Trasforma il grafico ggplot2 in un grafico interattivo con plotly
interactive_plot <- ggplotly(p)

# Visualizza il grafico interattivo
interactive_plot
# ------
library(lubridate)
library(dplyr)
library(readr)

df_monthly_0 <- read_csv("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/CSV/Weather/df_monthly_weather.csv")

# Crea un nuovo dataframe escludendo le date del 2023
df_monthly <- df_monthly_0 %>% filter(year(month) != 2023)

# Calcola il numero di elementi che hanno subito dei cambiamenti per ogni data
count_per_date <- final_dataframe %>%
  group_by(YearMonth) %>%
  summarise(TotalCount = n())

# Unisci count_per_date con df_monthly
df_monthly2 <- left_join(df_monthly, count_per_date, by = c("month" = "YearMonth"))

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


# -----
# Create the first plot
p1 <- ggplot(data = final_dataframe, aes(x = YearMonth, y = AREA, text = PopupText)) +
  geom_text(aes(label = Count_start, color = IsMax), vjust = +0.4) +
  scale_color_manual(values = c("Max" = "red", "NotMax" = "black")) +
  labs(x = "DATA", y = "AREA", color = "Legenda") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(limits = as.Date(c("2018-01-01", "2022-12-31")), 
               date_breaks = "1 year", date_labels = "%Y") +
  theme_bw()

# Create the second plot
p2 <- ggplot(df_monthly2, aes(x = month)) +
  geom_smooth(aes(y= precipSum, color = "Precipitation"), method = "loess", span = 0.1, se = FALSE) +
  geom_smooth(aes(y= tempMean, color = "Temperature"), method = "loess", span = 0.1, se = FALSE) +
  scale_y_continuous("Average Temperature (°C)", sec.axis = sec_axis(~.*2, name = "Precipitation  (mm)")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values=c("Precipitation"="blue", "Temperature"="red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
  labs(title = "Negative trend changes data") +
  guides(color=guide_legend(title=NULL))

p <- p + 
  geom_col(aes(y = TotalCount*10), width = 20, alpha = 0.4) +
  scale_y_continuous(sec.axis = sec_axis(~./10, name = "Total Count"))

# Combine the two plots
p <- subplot(p1, p2, nrows=2)

# Convert the ggplot to an interactive plot with plotly
interactive_plot <- ggplotly(p)

# Display the interactive plot
interactive_plot

# Grafico con le barre della somma mensile dei cambi negativi di trend
p <- ggplot(df_monthly2, aes(x = month)) +
  scale_y_continuous(name = "Precipitation (mm/month) / NCTS",
                     sec.axis = sec_axis(~ ./5, name = "Temperature (°C)", breaks = seq(0, 40, by = 5), labels = scales::comma_format(scale = 1))
  ) +
  scale_x_date(date_breaks = "1 year", date_labels="%Y") + # Aggiungi date_minor_breaks="1 month"
  scale_color_manual(values=c("Precipitation"="blue", "Temperature"="red")) +
  scale_fill_manual(values=c("Negative changes trend sum (NCTS)"="#228b22")) +
  theme(
    axis.text.x=element_text(angle=0,hjust=0.5),
    legend.position="bottom",
    text=element_text(size=12),
    axis.text=element_text(size=12),
    title=element_text(size=14,face="bold")
  ) +
  labs(title="Negative trend changes date") +
  guides(
    color=guide_legend(order=2,title=NULL), # Imposta l'ordine della legenda per il colore e rimuovi il titolo
    fill=guide_legend(order=1,title=NULL) # Imposta l'ordine della legenda per il riempimento e rimuovi il titolo
  ) +
  geom_vline(data=data.frame(month=seq(min(df_monthly2$month), max(df_monthly2$month), by="1 month")), aes(xintercept=month), linetype="dotted", color="grey50", size=0.1, alpha = 0.6) + # Linee dei mesi
  geom_vline(data=data.frame(month=seq(min(df_monthly2$month), max(df_monthly2$month), by="1 year")), aes(xintercept=month), linetype="solid", color="black", size=0.1, alpha = 0.6) +# Linee degli anni
  geom_smooth(aes(y = precipSum, color = "Precipitation"), method = "loess", span = 0.1, se = FALSE, size = 1, alpha = 0.6) +
  geom_smooth(aes(y = tempMean * 5, color = "Temperature"), method = "loess", span = 0.1, se = FALSE, size = 1, alpha = 0.6) +
  geom_col(aes(y = TotalCount, fill = "Negative changes trend sum (NCTS)"), width = 40, alpha = 0.8) +
  theme_bw() 

p

# Convert the ggplot to an interactive plot with plotly
interactive_plot <- ggplotly(p)

# Display the interactive plot
interactive_plot

# -----------
# Carica la libreria ggplot2 se non è già stata caricata
library(ggplot2)

# Crea il grafico climatico con le medie mensili delle precipitazioni e della temperatura
climatic_plot <- ggplot(df_monthly2, aes(x = month)) +
  geom_smooth(aes(y = precipSum, color = "Precipitation"), method = "loess", span = 0.1, se = FALSE) +
  geom_smooth(aes(y = tempMean, color = "Temperature"), method = "loess", span = 0.1, se = FALSE) +
  scale_y_continuous("Average Temperature (°C)", sec.axis = sec_axis(~ .* 2, name = "Precipitation  (mm)")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = c("Precipitation" = "blue", "Temperature" = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
  labs(title = "Negative trend changes date") +
  guides(color = guide_legend(order = 2, title = NULL), fill = guide_legend(order = 1, title = NULL) ) +
  geom_vline(data = data.frame(month = seq(min(df_monthly2$month), max(df_monthly2$month), by = "1 month")), aes(xintercept = month), linetype = "dotted", color = "grey50", size = 0.1, alpha = 0.6) +
  geom_vline(data = data.frame(month = seq(min(df_monthly2$month), max(df_monthly2$month), by = "1 year")), aes(xintercept = month), linetype = "solid", color = "black", size = 0.1, alpha = 0.6) +
  geom_smooth(aes(y = precipSum, color = "Precipitation"), method = "loess", span = 0.1, se = FALSE, size = 1, alpha = 0.6) +
  geom_smooth(aes(y = tempMean * 5, color = "Temperature"), method = "loess", span = 0.1, se = FALSE, size = 1, alpha = 0.6) +
  theme_bw()

# Ensure that 'month' is in Date format
df_monthly2$month <- as.Date(df_monthly2$month)

# Your existing ggplot code
ndvi_plot <- ggplot(data = final_dataframe, aes(x = YearMonth, y = AREA)) +
  geom_point(aes(size = Count), color = "blue") +
  scale_color_manual(values = c("Max" = "red", "NotMax" = "black")) +
  labs(x = "DATA", y = "AREA", color = "Legenda") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1) ) +
  scale_x_date(limits = as.Date(c("2018-01-01", "2022-12-31")), date_breaks = "1 year", date_labels = "%Y") +
  theme_bw()

# Add the smoothed lines for precipitation and temperature
combined_plot <- ndvi_plot +
  geom_smooth(data = df_monthly2, aes(x = month, y = precipSum, color = "Precipitation"), method = "loess", span = 0.1, se = FALSE) +
  geom_smooth(data = df_monthly2, aes(x = month, y = tempMean, color = "Temperature"), method = "loess", span = 0.1, se = FALSE) +
  scale_color_manual(values = c("Precipitation" = "blue", "Temperature" = "red")) +
  scale_y_continuous("Average Temperature (°C)", sec.axis = sec_axis(~ ., name = "Area"))  

# Print the combined plot
print(combined_plot)




#  PCA -------------------------
library(stats)
library(factoextra)
library(ggfortify )

library(dplyr)
library(lubridate)

# Legge il dataframe da un file CSV
final_dataframe <- read.csv("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/CSV/Weather/final_dataframe.csv")

# Aggiunge una colonna YearMonth per "start" e "end"
final_dataframe$YearMonth_start <- format(as.Date(paste(final_dataframe$start, "01", sep = "-"), "%Y-%m-%d"), "%Y-%m")
final_dataframe$YearMonth_end <- format(as.Date(paste(final_dataframe$end, "01", sep = "-"), "%Y-%m-%d"), "%Y-%m")

# Legge le date da un file CSV
dates <- read.csv("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/date.csv")

# Filtra le date rimuovendo il 2023 e formatta la colonna "date"
dates <- dates %>%
  filter(year(date) != 2023) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         YearMonth = format(date, "%Y-%m"))

# Esegue il merge dei due dataframe in base a "YearMonth" per "start" e "end" contemporaneamente
final_dataframe <- final_dataframe %>%
  left_join(dates, by = c("YearMonth_start" = "YearMonth")) %>%
  rename(date_start = date)

final_dataframe <- final_dataframe %>%
  left_join(dates, by = c("YearMonth_end" = "YearMonth")) %>%
  rename(date_end = date)

# Sostituisce la colonna "start" e "end" con i valori delle colonne "date" quando corrispondono a "YearMonth_start" e "YearMonth_end"
final_dataframe$start <- final_dataframe$date_start
final_dataframe$end <- final_dataframe$date_end

# Esegue le operazioni su "start" e "end" in un unico passaggio utilizzando dplyr
final_dataframe <- final_dataframe %>%
  mutate(
    StartYear = lubridate::year(start),
    StartMonth = lubridate::month(start),
    EndYear = lubridate::year(end),
    EndMonth = lubridate::month(end),
    StartDaysInYear = ifelse(StartYear %% 4 == 0 & (StartYear %% 100 != 0 | StartYear %% 400 == 0), 366, 365),
    EndDaysInYear = ifelse(EndYear %% 4 == 0 & (EndYear %% 100 != 0 | EndYear %% 400 == 0), 366, 365),
    StartYearFraction = (StartYear + (StartMonth - 1) / 12) + (day(start) - 1) / StartDaysInYear,
    EndYearFraction = (EndYear + (EndMonth - 1) / 12) + (day(end) - 1) / EndDaysInYear
  )

#  Crea la colonna AreaCode numerica
final_dataframe <- final_dataframe %>%
  mutate(AreaCode = dense_rank(AREA))

# # Select the numeric columns, or transform non-numeric columns as needed
# numeric_data <- final_dataframe %>%
#   select(AreaCode, YearFraction)

# If "AREA" is categorical and you want to include it, you can one-hot encode it:
numeric_data <- final_dataframe %>%
  select(AreaCode, StartYearFraction, EndYearFraction) %>%
  mutate(across(contains("AREA"), as.numeric))


# APPLICAZIONE ANOVA

# SELEZIONA SOLO ALCUNE COLONNE
selected_data <- final_dataframe %>%
  select(COD, AREA, StartYearFraction, EndYearFraction)

boxplot(selected_data$StartYearFraction  ~ selected_data$AREA, col= terrain.colors((12)))

boxplot(selected_data$EndYearFraction  ~ selected_data$AREA, col= terrain.colors((12)))

selected_data$AREA = factor(selected_data$AREA)

model <- lm(EndYearFraction~AREA, data=selected_data)

anova(model)



confronto_medie=aov(model)
confronto_medie

TukeyHSD(confronto_medie)

library(emmeans)

lsmeans(selected_data,"AREA")





# https://rpubs.com/KarolinaSzczesna/862710

M = cor(numeric_data)
corrplot(M, method = 'number')
corrplot(M, method = 'color', order = 'alphabet')
corrplot(M)
corrplot(M, order = 'AOE')
corrplot(M, method = 'shade', order = 'AOE', diag = FALSE)
corrplot(M, method = 'square', order = 'FPC', type = 'lower', diag = FALSE)
corrplot(M, method = 'ellipse', order = 'AOE', type = 'upper')
corrplot.mixed(M, order = 'AOE')
corrplot.mixed(M, lower = 'shade', upper = 'pie', order = 'hclust')
corrplot(M, order = 'hclust', addrect = 2)
corrplot(M, method = 'square', diag = FALSE, order = 'hclust',
         addrect = 3, rect.col = 'blue', rect.lwd = 3, tl.pos = 'd')


PCA <- prcomp(numeric_data, scale = TRUE)


PCA
summary(PCA)

get_eigenvalue(PCA)

fviz_eig(PCA, col.var="blue", addlabels = TRUE)

fviz_screeplot(PCA)

var <- get_pca_var(PCA)
head(var$cos2)

biplot(PCA)


library("corrplot")
corrplot(var$cos2, is.corr=TRUE)


fviz_cos2(PCA, choice = "var", axes = 1:2)

fviz_pca_var(PCA,
             col.var = "cos2", # Color by the quality of representation
             gradient.cols = c("darkorchid4", "gold", "darkorange"),
             repel = TRUE
)

# Contributions of variables to PC1
a<-fviz_contrib(PCA, choice = "var", axes = 1)
# Contributions of variables to PC2
b<-fviz_contrib(PCA, choice = "var", axes = 2)
grid.arrange(a,b, ncol=2, top='Contribution of the variables to the first two PCs')



ind <- get_pca_ind(PCA)
ind

fviz_pca_ind(PCA,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("darkorchid4", "gold", "darkorange"),
             repel = TRUE
)


# Total contribution on PC1 and PC2
fviz_contrib(PCA, choice = "ind", axes = 1:2)

autoplot(PCA, loadings=TRUE, loadings.colour='darkorchid4', loadings.label=TRUE, loadings.label.size=3)

kmeans<-eclust(numeric_data, k=4)
autoplot(PCA, data=kmeans, colour="cluster")



kmeans<-eclust(numeric_data, k= 4)

autoplot(PCA, data=kmeans, colour="cluster")



cor(numeric_data, PCA$x)

plot(PCA)

screeplot(PCA, type = "l")

biplot(PCA)


# ------
library(tidyverse)
library(FactoMineR)

# Standardizza i dati (opzionale, ma spesso utile in PCA)
scaled_data <- scale(numeric_data)

# Applica la PCA
pca_result <- PCA(scaled_data, graph = FALSE)

# Ottieni le principali componenti principali e le varianze spiegate
eigenvalues <- pca_result$eig

# Ottieni i carichi dei componenti principali
loadings <- pca_result$var$cor

# Ottieni la proiezione dei dati originali sulle componenti principali
scores <- pca_result$ind$coord

# Visualizza il grafico della varianza spiegata
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50))

# Visualizza il grafico dei carichi delle variabili
fviz_pca_var(pca_result, col.var = "contrib", col.ind = "cos2")

# Visualizza il grafico delle osservazioni
fviz_pca_ind(pca_result, col.ind = "cos2", pointsize = 2)

fviz_pca_biplot(pca_result)

# -----------

# https://www.analyticsvidhya.com/blog/2016/03/pca-practical-guide-principal-component-analysis-python/

pca.train <- numeric_data[1:nrow(numeric_data),]

pca.test <- numeric_data[-(1:nrow(numeric_data)),]

prin_comp <- prcomp(pca.train, scale. = T)

names(prin_comp)

dim(prin_comp$x)

biplot(prin_comp, scale = 0)

# compute standard deviation of each principal component
std_dev <- prin_comp$sdev
#compute variance
pr_var <- std_dev^2
#check variance of first 10 components
pr_var[1:10]

prop_varex <- pr_var/sum(pr_var)

plot(prop_varex, xlab = "Principal Component",
     
     ylab = "Proportion of Variance Explained",
     
     type = "b")

#add a training set with principal components
train.data <- data.frame(numeric_data = numeric_data$YearFraction, prin_comp$x)
#we are interested in first 30 PCAs
train.data <- train.data[,1:2]
#run a decision tree
library(rpart)
rpart.model <- rpart(selected_data ~ .,data = train.data, method = "anova")
rpart.model
#transform test into PCA
test.data <- predict(prin_comp, newdata = pca.test)
test.data <- as.data.frame(test.data)
#select the first 30 components
test.data <- test.data[,1:2]
#make prediction on test data
rpart.prediction <- predict(rpart.model, test.data)
#For fun, finally check your score of leaderboard
final.sub <- data.frame(Item_Identifier = sample$Item_Identifier, Outlet_Identifier = sample$Outlet_Identifier, Item_Outlet_Sales = rpart.prediction)
write.csv(final.sub, "pca.csv",row.names = F)c




# Installa e carica ggplot2 se non è già stato fatto
# install.packages("ggplot2")
library(ggplot2)

# Crea lo scatter plot con colori diversi per i gruppi
ggplot(numeric_data, aes(x = AreaCode, y = YearFraction)) +
  geom_point() +
  labs(x = "AreaCode", y = "YearFraction") +
  ggtitle("Scatter Plot dei dati selected_data con colori per gruppi")

# Sostituisci con i tuoi dati e il numero desiderato di cluster (k)
set.seed(123)  # Per rendere i risultati riproducibili
k <- 4  # Numero di cluster desiderato
cluster_results <- kmeans(numeric_data[, c("AreaCode", "StartYearFraction", "EndYearFraction")], algorithm = "Forgy", centers = k)

# Plot dei risultati del clustering
ggplot(numeric_data, aes(x = AreaCode, y = StartYearFraction, color = factor(cluster_results$cluster))) +
  geom_point() +
  labs(x = "AreaCode", y = "StartYearFraction") +
  ggtitle("Risultati del Clustering")


