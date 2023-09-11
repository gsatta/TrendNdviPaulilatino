

library(ggplot2)
library(sf)
library(dplyr)

# Imposta l'opzione scipen su un valore elevato per eliminare la notazione esponenziale dei valori
options(scipen = 999)

# Carica il file dei valori dell'ndvi medio per ogni area
ndvi_avg <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_VALUES/ndvi_avg_areas.shp")

# Crea un grafico delle tendenze temporali usando ggplot2 e aggiungi etichette per ciascuna area
plot <- ggplot(ndvi_avg, aes(x = date, y = mean_NDVI, group = AREA, color = AREA)) +
  binomial_smooth() +
  scale_color_brewer(palette = "Set3") +
  labs(title = "Tendenze temporali dell'NDVI per ciascuna area",
       x = "Date",
       y = "NDVI") +
  theme_minimal()

plot


ggplot(data = ndvi_avg, aes(x = date, y = mean_NDVI , group = AREA)) +
  geom_boxplot()

library(ggplot2)
ggplot(data = ndvi_avg, aes(x = date, y = mean_NDVI, group = AREA)) +
  geom_line(aes(color=AREA)) +
  geom_point(aes(color=AREA))


ggplot(data = ndvi_avg, aes(x = date, y = mean_NDVI, group = AREA)) +
  geom_line(aes(color=AREA)) +
  geom_point(aes(color=AREA)) +
  facet_wrap(~AREA) + 
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2, aes(color=AREA))
  
library(ggpubr)

ggplot(data = ndvi_avg, aes(x = date, y = mean_NDVI, group = AREA)) +
  geom_line(aes(color=AREA)) +
  geom_point(aes(color=AREA)) +
  facet_wrap(~AREA) + 
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2, aes(color=AREA)) +
  stat_regline_equation(label.y = 1, aes(label = ..eq.label..))

  

basic.lm <- lm(mean_NDVI ~ date, data = ndvi_avg)
summary(basic.lm)

# Call:
#   lm(formula = mean_NDVI ~ date, data = ndvi_avg)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.26280 -0.07129  0.03312  0.07297  0.16255 
# 
# Coefficients:
#   Estimate   Std. Error t value           Pr(>|t|)    
# (Intercept)  0.850930086  0.109509466   7.770 0.0000000000000302 ***
#   date        -0.000007273  0.000005900  -1.233              0.218    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.09033 on 658 degrees of freedom
# Multiple R-squared:  0.002304,	Adjusted R-squared:  0.0007879 
# F-statistic:  1.52 on 1 and 658 DF,  p-value: 0.2181



(prelim_plot <- ggplot(ndvi_avg, aes(x = date, y = mean_NDVI)) +
    geom_point() +
    geom_smooth(method = "lm"))

plot(basic.lm, which = 1)

plot(basic.lm, which = 2) 

boxplot(mean_NDVI ~ AREA, data = ndvi_avg)

(colour_plot <- ggplot(ndvi_avg, aes(x = date, y = mean_NDVI, colour = AREA)) +
    geom_point(size = 2) +
    theme_classic() +
    theme(legend.position = "none"))


# Potremmo eseguire molte analisi separate e adattare una regressione per ciascuna catena montuosa.

(split_plot <- ggplot(aes(date, mean_NDVI), data = ndvi_avg) + 
    geom_point() + 
    facet_wrap(~ AREA) + # create a facet for each mountain range
    xlab("date") + 
    ylab("Mean NDVI"))
