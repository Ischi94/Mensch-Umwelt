# lade das here-package
library(here)

# lade den datensatz "island_diversity.rds", der sich im unterordner data/ befindet
dat_div <- readRDS(here())

# baue ein einfaches lineares modell mit "diversity" als zielvariable 
# und "island size" als erklärende variable 
# speichere das modell als mod1
mod1 <- lm()


# extrahiere den schätzwert (estimate) für den achsenabschnittseffekt von "island size" auf "diversity"
# mithilfe der funktion coef() und speichere ihn in dat_coef
dat_coef <- 
  
# filtere anschließend den schätzwert für die steigung (slope) heraus und interpretiere ihn
  
  
# lies die dokumentation der funktion confint mit ?confint
?confint

# verwende nun die funktion confint, um das 95%-konfidenzintervall 
# (entspricht einem alpha-wert von 0.05) für den steigungsparameter zu berechnen,
# also für den geschätzten effekt der inselgröße auf die diversität
# speichere die schätzwerte in conf_slope
conf_slope <- 
  
# schneiden sich die konfidenzintervalle mit null?
 conf_slope
