# Das here-Package ist eine der besten Erfindungen im R-Universum. 
# Es ermöglicht, den Speicherort einer .Rproj-Datei als Referenzpunkt (root directory) zu verwenden. 
# Das bedeutet, dass du nie wieder manuell dein Arbeitsverzeichnis setzen musst – 
# du kannst einfach here() verwenden.


# lade das here-Package
library(here)

# überprüfe, auf welches Verzeichnis dein root directory zeigt
here()

# Der Datensatz, den wir verwenden, befindet sich im Unterordner data/ 
# und heißt "island_diversity.rds".
# Mit der Funktion here() können wir ihn folgendermaßen laden und einer Variable zuweisen:
dat_div <- readRDS(here("data", "island_diversity.rds"))

# Verwende die Funktion mean(), um den Mittelwert der Dauer über alle Beobachtungen zu berechnen.
# Füge deinen Code unten ein:


# Modelliere den Mittelwert über alle Beobachtungen mit einem linearen Modell, 
# das nur einen Achsenabschnitt (Intercept) enthält.
# Weise dieses Modell der Variable mod1 zu.
mod1 <- 
  
# Verwende die Funktion summary(), um dir die Ausgabe des Modells anzusehen.
# Vergleiche den geschätzten Intercept mit dem Mittelwert, den du oben berechnet hast.
# Gibt uns das Modell denselben Mittelwert zurück?
  
  
# Fitte nun ein lineares Modell mit „diversity“ als abhängiger Variable 
# und „island size“ (Inselgröße in km²) als erklärender Variable.
# Damit fragen wir explizit: Wie verändert sich die Diversität mit der Größe der Insel?
# Weise dieses Modell der Variable mod2 zu.
  
  
# Verwende erneut die Funktion summary(), um dir die Modellergebnisse anzusehen.
# Wie interpretierst du die Ergebnisse des Modells?
  