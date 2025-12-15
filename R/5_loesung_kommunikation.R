# lade das heruntergeladene ggplot2 package
library(ggplot2)

# lies die neuen Daten ein
# die daten sind im csv Format (englisches Excel) und liegen online auf github
# wir können die daten wie folgt über eine url in R laden
dat_div <- read.csv("https://raw.githubusercontent.com/Ischi94/Mensch-Umwelt/refs/heads/main/data/island_diversity_extended.csv")


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

# koeffizienten sind sehr schwer zu interpretieren
# einfacher ist es einen datensatz zu erstellen (eine parallel-welt) 
dat_pred1 <- data.frame("human_presence" = c(0, 1)) # erklärvariable des modells

# im nächsten schritt wenden wir das modell auf unsere parallelwelt an,
# damit das modell uns sagt was es über denkt
dat_pred1$pred_val <- predict(mod1, newdata = dat_pred1)

# diese ergebnisse sind viel leichter zu interpretieren
dat_pred1


# # lineare regression 2 ----------------------------------------------------------------------


# fitte ein lineares modell mit diversität (diversity) als zielvariable und
# ökosystemen (ecosystem) als erklärvariable
mod2 <- lm(diversity ~ ecosystem, data = dat_div)

# selber ansatz, parallelwelt
dat_pred2 <- data.frame("ecosystem" = unique(dat_div$ecosystem))

# vorhersage
dat_pred2$pred_val <- predict(mod2, newdata = dat_pred2)

# interpretation
dat_pred2



# grenzen unsere modelle ----------------------------------------------------------------------


# fitte ein lineares modell mit menschlicher präsenz (human_presence) als zielvariable und
# inselgröße (size) als eklärvariable
mod_lm <- lm(human_presence ~ size, data = dat_div)

# parallelwelt
dat_pred3 <- data.frame("size" = seq(0, 5, by = 0.2))

# vorhersage
dat_pred3$pred_val <- predict(mod_lm, newdata = dat_pred3)

# interpretation mit visualisierung
ggplot(data = dat_div, 
       aes(size, human_presence)) +
  geom_point() +
  geom_line(aes(size, pred_val), 
            data = dat_pred3)


# logistische regression ----------------------------------------------------------------------


# fitte eine logistische regression (family = "binomial) mit der selben struktur
mod_glm <- glm(human_presence ~ size, family = "binomial", data = dat_div)

# vorhersage
dat_pred3$pred_val_log <- predict(mod_glm, newdata = dat_pred3, 
                                  type = "response")

# interpretation
ggplot(data = dat_div, 
       aes(size, human_presence)) +
  geom_point() +
  geom_line(aes(size, pred_val_log), 
            data = dat_pred3)


# wie hoch ist die vorhergesagte wahrscheinlichkeit einen menschen anzutreffen für eine 
# inselgröße von 0.5 km?
predict(mod_glm, newdata = data.frame("size" = 0.5), 
        type = "response")

# wie hoch ist die vorhergesagte wahrscheinlichkeit einen menschen anzutreffen wenn wir 
# die inselgröße um eine einheit von 0.5 km auf 1.5 km erhöhen?
predict(mod_glm, newdata = data.frame("size" = 1.5), 
        type = "response")

# wie sieht es aus wenn wir diese von 1.5 km um eine einheit auf 2.5 erhöhen?
predict(mod_glm, newdata = data.frame("size" = 2.5), 
        type = "response")


# poisson regression --------------------------------------------------------------------------

# bau ein lineare modell mit diversität als zielvariable und inselgröße als erklärvariable
mod3 <- lm(diversity ~ size, dat_div)

# wie hoch schätzt das lineare modell die diversität für eine insel mit größe 0 ein?
predict(mod3, newdata = data.frame("size" = 0))

# bau ein generalisiertes lineares modell mit diversität als zielvariable und 
# inselgröße als erklärvariable und der poisson verteilung
mod4 <- glm(diversity ~ size, dat_div, 
            family = "poisson")

# wie hoch schätzt das generalisierte modell die diversität für eine insel mit größe 0 ein?
predict(mod4, newdata = data.frame("size" = 0))


# visualisiere die unterschiedlichen vorhersagen für den gesamten datenbereich
dat_pred <- data.frame("size" = seq(min(dat_div$size), 
                                    max(dat_div$size), 
                                    0.1))
# vorhersage lineares modell
dat_pred$lin_pred <- predict(mod3, newdata = dat_pred)
# vorhersage generalisiertes modell
dat_pred$glm_pred <- predict(mod4, newdata = dat_pred, 
                             type = "response")

# visualisierung
plot_1 <- ggplot(data = dat_div, 
                 aes(size, diversity)) +
  geom_point() +
  geom_line(aes(size, lin_pred), 
            data = dat_pred, 
            colour = "coral") +
  geom_line(aes(size, glm_pred), 
            data = dat_pred, 
            colour = "darkblue")

# lade das here package
library(here)

# abspeichern im ordner figures
ggsave(filename = here("figures", "erster_ggplot.png"), 
       plot = plot_1)
