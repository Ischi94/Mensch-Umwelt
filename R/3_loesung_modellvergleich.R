# lade das here package
library(here)


# lade den diversitäts-datensatz aus der letzten stunde
# er liegt im unterordner data/ und heißt "island_diversity.rds"
dat_div <- readRDS(here("data", "island_diversity.rds"))

# fitte ein lineares Modell mit y = Diversität und x = Inselgröße 
mod1 <- lm(diversity ~ size, data = dat_div)

# fitte ein Nullmodell das besagt, dass es kein Effekt von Inselgröße auf Diversität gibt. 
# dieses kannst du durch ein intercept-only Modell abschätzen
# intercept-only linear model
mod0 <- lm(diversity ~ 1, data = dat_div)


# jetzt fitte eine drittes Modell, bei dem die Diversität mit dem Logarithmus von 
# der Inselgröße zusammenhängt
mod3 <- glm(diversity ~ 1, family = poisson(), data = dat_div)

# jetzt fitte eine viertes Modell, bei dem die Diversität mit dem Exponent von 
# der Inselgröße zusammenhängt


# schau dir die AIC Funktion an
?AIC

# vergleiche alle vier Modelle, welches ist das Beste?
AIC(mod0, mod1, mod2, mod3)
