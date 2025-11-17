# lade das here package
library(here)


# lade den diversitäts-datensatz aus der letzten stunde
# er liegt im unterordner data/ und heißt "island_diversity.rds"
dat_div <- readRDS()

# fitte ein lineares Modell mit y = Diversität und x = Inselgröße 
mod1 <- lm()

# fitte ein Nullmodell das besagt, dass es kein Effekt von Inselgröße auf Diversität gibt. 
# dieses kannst du durch ein intercept-only Modell abschätzen
# intercept-only linear model
mod0 <- lm()


# jetzt fitte eine drittes Modell, bei dem die Diversität mit dem Logarithmus von 
# der Inselgröße zusammenhängt
mod3 <- lm()

# jetzt fitte eine viertes Modell, bei dem die Diversität mit dem Exponent von 
# der Inselgröße zusammenhängt
mod3 <- lm()

# schau dir die AIC Funktion an
?AIC

# vergleiche alle vier Modelle, welches ist das Beste?
AIC()
