# lies die neuen Daten ein
# die daten sind im csv Format (englisches Excel) und liegen online auf github
# wir können die daten wie folgt über eine url in R laden
dat_div <- read.csv("https://raw.githubusercontent.com/Ischi94/Mensch-Umwelt/refs/heads/main/data/island_diversity_extended.csv")

# inspiziere die ersten zeilen
head(dat_div)

# schaue auf die struktur des datensatzes
str(dat_div)

# welche variablen sind numerisch? welche sind kategorisch? welche binär?
# numerisch = island, diversity, forest_cover
# kategorisch = ecosystem
# binär = human_presence


# lineare regression --------------------------------------------------------------------------

# fitte ein lineares modell mit diversität (diversity) als zielvariable und
# anwesenheit von menschen (human_presence) als erklärvariable
mod1 <- lm(diversity ~ human_presence, data = dat_div)


# um wie viele arten ändert sich die diversität im schnitt wenn menschen auf einer insel
# vorhanden sind (im vergleich zu wenn sie nicht vorhanden sind)?
mod1_slope <- coef(mod1)[2]

# was ist die durchschnittliche diversität einer insel auf der menschen fehlen?
mod1_intercept <- coef(mod1)[1]
mod1_intercept

# was ist die durschnittliche diversität einer insel auf der menschen anwesend sind?
mod1_intercept + mod1_slope


# fitte ein lineares modell mit diversität (diversity) als zielvariable und
# ökosystemen (ecosystem) als erklärvariable
mod2 <- lm(diversity ~ ecosystem, data = dat_div)

# finde heraus welches ökosystem das modell als intercept/ baseline verwendet
summary(mod2) # boreal

# was ist die durschnittliche diversität einer borealen insel?
coef(mod2)[1]

# was ist die durschnittliche diversität einer gemäßigten insel?
coef(mod2)[1] + coef(mod2)[2]

# was ist die durschnittliche diversität einer tropischen insel?
coef(mod2)[1] + coef(mod2)[3]



# logistische regression ----------------------------------------------------------------------


# fitte ein lineares modell mit menschlicher präsenz (human_presence) als zielvariable und
# inselgröße (size) als eklärvariable
mod_lm <- lm(human_presence ~ size, data = dat_div)

# extrahiere den intercept koeffizienten (= die wahrscheinlichkeit einen Menschen auf einer Insel
# mit der größe 0 anzufinden)
lm_intercept <- coef(mod_lm)[1]

# fitte eine logistische regression (family = "binomial) mit der selben struktur
mod_glm <- glm(human_presence ~ size, family = "binomial", data = dat_div)

# extrahiere den intercept koeffizienten
glm_intercept <- coef(mod_glm)[1]

# um diesen wert interpretieren zu können, müssen wir in mithilfe der inverse-link zurück-
# transformieren
# die inverse logit funktion ist definiert als: exp(x)/(1+exp(x))
# wende diese funktion auf glm_slope an
exp(glm_intercept)/(1+exp(glm_intercept))


# log-odds ------------------------------------------------------------------------------------

# extrahiere den slope koeffizienten der logistischen regression
glm_slope <- coef(mod_glm)[2]

# der slope ist in log-odds angegeben, rechne ihn in odds um 
# wie interpretierst du die odds?
glm_odds <- exp(glm_slope)

# die folgende funktion berechnet die änderung in der wahrscheinlichkeit einen menschen
# anzutreffen wenn sich die inselgröße um eine einheit erhöht, relativ zur startwahrscheinlichkeit
# schau dir die funktion an und versuche sie zu verstehen
get_prob_change <- function(prob_val) {
  
  odd_val <- prob_val/(1 - prob_val) # umrechnung von probs in odds
  odd_change <- odd_val * glm_odds # berechnung der änderung in odds
  prob_change <- odd_change/(odd_change + 1) # berechnung in änderung der wahrscheinlichkeit
  return(prob_change)
  
}

# wie hoch ist die wahrscheinlich einen menschen anzutreffen wenn wir die inselgröße um 
# einen km vergrößern
# gehe hierbei von einer ausgangswahrscheinlichkeit von 2% aus
get_prob_change(0.02)

# gehe jetzt von einer ausgangswahrscheinlichkeit von 15% aus
get_prob_change(0.15)

# gehe jetzt von einer ausgangswahrscheinlichkeit von 70% aus
get_prob_change(0.7)

# gehe jetzt von einer ausgangswahrscheinlichkeit von 97% aus
get_prob_change(0.97)
