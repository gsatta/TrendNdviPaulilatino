# Per determinare quale metodo sia più appropriato per il tuo caso specifico, 
# potresti considerare di eseguire un’analisi dei residui e un test di normalità sui residui

# Installa e carica il pacchetto mblm se non è già stato fatto
if (!require(mblm)) {
  install.packages("mblm")
  library(mblm)
}

# Installa e carica il pacchetto Metrics se non è già stato fatto
if (!require(Metrics)) {
  install.packages("Metrics")
  library(Metrics)
}

# Installa e carica il pacchetto nortest se non è già stato fatto
if (!require(nortest)) {
  install.packages("nortest")
  library(nortest)
}

# Esegui il ciclo for su ogni COD
for (cod in cod_list) {
  # Seleziona i dati per il COD corrente
  data0 <- NDVI_VALUES[NDVI_VALUES$COD == cod, ]
  
  ts <- ts(data0$ndvi, start = c(2018, 1), end = c(2023, 8), frequency = 12)
  
  # Converti le date in numeri (numero di giorni dalla data minima)
  data0$date_numeric <- as.numeric(data0$date - min(data0$date))
  
  # Calcola Theil-Sen's slope (Q) utilizzando la data numerica
  theil_sen_fit <- mblm(ndvi ~ date_numeric, data = data0)
  
  # Calcola la regressione lineare utilizzando la data numerica
  lm_fit <- lm(ndvi ~ date_numeric, data = data0)
  
  # Calcola i residui per entrambi i modelli
  theil_sen_resid <- residuals(theil_sen_fit)
  lm_resid <- residuals(lm_fit)
  
  # Esegui un test di normalità sui residui
  theil_sen_normality <- ad.test(theil_sen_resid)
  lm_normality <- ad.test(lm_resid)
  
  # Stampa i risultati dei test di normalità
  print(paste("Theil-Sen normality test p-value:", theil_sen_normality$p.value))
  print(paste("Linear regression normality test p-value:", lm_normality$p.value))
}

# Dai risultati dei test di normalità, sembra che i residui di entrambi i metodi 
# (Theil-Sen e regressione lineare) non seguano una distribuzione normale, 
# dato che tutti i valori p sono inferiori a 0.05. Questo suggerisce che entrambi 
# i modelli potrebbero non essere adatti per i tuoi dati.
# 
# Tuttavia, il metodo di Theil-Sen è un metodo non parametrico e non richiede 
# l’ipotesi di normalità, quindi potrebbe essere più appropriato in questo caso. 
# D’altra parte, la regressione lineare è un metodo parametrico e richiede l’ipotesi 
# di normalità dei residui.

Inoltre, potresti considerare di eseguire ulteriori analisi sui tuoi dati. Ad esempio, potresti esaminare i grafici dei residui per entrambi i metodi per vedere se mostrano modelli o tendenze specifiche. Potresti anche considerare di utilizzare altri metodi robusti o non parametrici se ritieni che né Theil-Sen né la regressione lineare siano adatti per i tuoi dati.