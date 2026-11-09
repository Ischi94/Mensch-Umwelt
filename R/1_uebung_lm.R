# das here-paket erlaubt uns, dateipfade relativ zum projektstamm zu verwenden.
# dadurch müssen wir das arbeitsverzeichnis nicht für jeden computer neu setzen.

# lade das here-paket
library(here)

# überprüfe den erkannten projektstamm
here()

# lade den datensatz aus dem unterordner data/
dat_div <- readRDS(here("data", "island_diversity.rds"))

# berechne mit mean() den mittelwert der zielvariable diversity (artenanzahl)
# über alle inseln. ergänze deinen code:


# erstelle mit lm() ein konstantmodell mit diversity als zielvariable.
# das modell soll nur einen intercept enthalten und mod1 heißen.
mod1 <-
  
# untersuche das modell mit summary().
# stimmt der geschätzte intercept mit dem zuvor berechneten mittelwert überein?
  
  
# erstelle ein lineares modell mit diversity als zielvariable (y)
# und size (inselgröße) als erklärvariable (x). speichere es als mod2.
# die frage lautet: wie verändert sich die erwartete artenanzahl mit der inselgröße?
mod2 <-
  
# untersuche das modell mit summary().
# wie interpretierst du intercept und steigung?
  
  
# zeichne mit plot() die inselgröße (x-achse) gegen die artenanzahl (y-achse).
# nutze xlab und ylab für verständliche achsenbeschriftungen.
  
  
# ergänze mit abline() zuerst die horizontale mittelwertlinie aus mod1.
# zeichne danach die ansteigende regressionslinie aus mod2 ein.
  
  
  