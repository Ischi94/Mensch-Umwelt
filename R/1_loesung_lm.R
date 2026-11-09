# das here-paket erlaubt uns, dateipfade relativ zum projektstamm zu verwenden.
# dadurch müssen wir das arbeitsverzeichnis nicht für jeden computer neu setzen.

# lade das here-paket
library(here)

# überprüfe den erkannten projektstamm
here()

# lade den datensatz aus dem unterordner data/
dat_div <- readRDS(here("data", "island_diversity.rds"))

# berechne mit mean() den mittelwert der zielvariable diversity (artenanzahl)
# über alle inseln
mean(dat_div$diversity)

# erstelle mit lm() ein konstantmodell mit diversity als zielvariable.
# das modell soll nur einen intercept enthalten und mod1 heißen.
mod1 <- lm(diversity ~ 1, data = dat_div)

# untersuche das modell mit summary().
# stimmt der geschätzte intercept mit dem zuvor berechneten mittelwert überein?
summary(mod1)

# erstelle ein lineares modell mit diversity als zielvariable (y)
# und size (inselgröße) als erklärvariable (x). speichere es als mod2.
# die frage lautet: wie verändert sich die erwartete artenanzahl mit der inselgröße?
mod2 <- lm(diversity ~ size, data = dat_div)

# untersuche das modell mit summary().
# wie interpretierst du intercept und steigung?
summary(mod2)

# zeichne mit plot() die inselgröße (x-achse) gegen die artenanzahl (y-achse).
# nutze xlab und ylab für verständliche achsenbeschriftungen.
plot(dat_div$size, dat_div$diversity,
     xlab = "Inselgröße", ylab = "Artenanzahl")

# ergänze mit abline() zuerst die horizontale mittelwertlinie aus mod1.
# zeichne danach die ansteigende regressionslinie aus mod2 ein.
abline(mod1, col = "grey50", lty = 2, lwd = 2)
abline(mod2, col = "orange", lwd = 2)
