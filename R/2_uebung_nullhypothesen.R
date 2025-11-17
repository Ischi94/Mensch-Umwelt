# lade die here-bibliothek
library(here)

# lade den diversitäts-datensatz aus der letzten stunde
# er liegt im unterordner data/ und heißt "island_diversity.rds"
dat_div <- readRDS(here("data", "island_diversity.rds"))

# baue ein einfaches lineares modell mit diversität (diversity) als zielvariable (y)
# und inselgröße (size) als erklärende variable (x) und speichere das modell als mod1
mod1 <- lm(diversity ~ size, data = dat_div)

# extrahiere die geschätzten koeffizienten aus dem modell mit der coef() Funktion
# und speichere sie in dat_coef
dat_coef <- coef(mod1)

# filtere den schätzwert für die steigung (slope) heraus und interpretiere ihn
dat_coef["size"]
  
# lies die dokumentation der funktion confint mit ?confint
?confint

# verwende nun die funktion confint, um das 95%-konfidenzintervall zu berechnen
# (entspricht einem alpha-wert von 0.05) für die steigung, also den effekt der inselgröße auf die diversität
# speichere das ergebnis in conf_slope
conf_slope <- confint(mod1, parm = "size")
  
  
# überschneidet sich das konfidenzintervall mit null?
conf_slope

# nun verbessern wir unsere nullverteilung mithilfe einer permutation
# wir möchten ein neues datenset erstellen, in dem die inselgrößen zufällig durchmischt werden,
# sodass jede beziehung zwischen diversität und größe aufgehoben wird,
# während die eigenschaften der daten (z. b. verteilung der werte) erhalten bleiben

# wir können die spalte size zufällig durchmischen, indem wir sie ohne zurücklegen neu anordnen
sample(dat_div$size, size = nrow(dat_div), replace = FALSE)

# jedes mal, wenn du den code oben ausführst, wird die spalte size in einer neuen reihenfolge ausgegeben
# wir können die permutierten daten in einem neuen datensatz speichern
dat_new <- data.frame(island = dat_div$island, diversity = dat_div$diversity, 
                      size = sample(dat_div$size, size = nrow(dat_div), replace = FALSE))

# jetzt wollen wir das 100 mal wiederholen, dafür legen wir eine leere liste mit 100 plätzen an
list_perm <- vector(mode = "list", length = 100)

# nun setzen wir eine schleife auf, die die daten 100 mal permutiert
# und jedes mal in den entsprechenden platz von list_perm speichert
for (i in 1:100) {
  
  list_perm[[i]] <- 
    
}

# nun, da wir 100 permutierte datensätze haben, erstellen wir eine neue liste mit 100 plätzen,
# um dort die jeweiligen steigungswerte (slopes) der linearen modelle zu speichern
list_slopes <- 

# erstelle eine schleife über alle 100 permutierten datensätze
for (i in 1:100) {
  
  # wende das lineare modell auf den jeweiligen permutierten datensatz an
  mod <- lm(diversity ~ size, data = )
  
  # extrahiere den steigungsparameter aus dem modell (siehe oben)
  list_slopes[[i]] <- 
}

# jetzt haben wir 100 steigungswerte, die unter der nullhypothese erzeugt wurden
# wir können sie mit unlist() zu einem vektor zusammenfassen und in vec_slopes speichern
vec_slopes <- 

# wie verhält sich unser empirischer steigungswert (dat_coef[[2]]) im vergleich zu dieser verteilung?
# können wir die nullhypothese verwerfen?
