library(here)       # hilft beim organisieren der projektstruktur
library(terra)      # zum bearbeiten räumlicher daten
library(tidyverse)  # datenverarbeitung und visualisierung
library(rgbif)      # herkunft von artenverbreitungsdaten
library(geodata)    # herkunft von umweltdaten
library(ggspatial)  # visualisierung räumlicher daten


# ausmaß von europa holen
Europe_ext <- c(-15,45,35,72)

# europa-karte holen
Europe_map <- crop(x = world(resolution = 2,
                             level = 0),
                   y = Europe_ext)

plot(Europe_map, main = "Europe")


# Schritt 1: Daten zu Präsenz/Absenz und Umweltvariablen beschaffen

## Umweltdaten

# bioklimatische variablen herunterladen
bioclim_data <- worldclim_global(var = "bio",
                                 res = 5)

# erste sichtkontrolle
plot(bioclim_data)

# auf europa zuschneiden
bioclim_data <- crop(bioclim_data, Europe_ext)

# check
plot(bioclim_data)

## Vorkommensdaten (Species occurrences)

# gbif-daten herunterladen
gbif_ArnMon_download <- occ_data(scientificName = "Arnica montana",
                                 hasCoordinate = TRUE,
                                 limit = 1000,
                                 year="1999,2005",
                                 basisOfRecord = "HUMAN_OBSERVATION")

# daten extrahieren
gbif_ArnMon <- gbif_ArnMon_download$data

# daten ansehen
head(gbif_ArnMon)

# nur relevante spalten auswählen
gbif_ArnMon <- gbif_ArnMon[,c("key",
                              "decimalLatitude",
                              "decimalLongitude",
                              "occurrenceStatus",
                              "coordinateUncertaintyInMeters")]

# daten prüfen
summary(gbif_ArnMon)

# karte plotten
plot(Europe_map,
     axes = TRUE, 
     col = "grey95",
     main = "Vorkommen in Europa")

# vorkommenspunkte hinzufügen
points(x = gbif_ArnMon$decimalLongitude, 
       y = gbif_ArnMon$decimalLatitude, 
       col = "orange", 
       pch = 16, 
       cex = 0.5)

### Qualitätscheck

# messungs-unsicherheiten anzeigen (meter)
ggplot(gbif_ArnMon, aes(x = coordinateUncertaintyInMeters)) +
  geom_histogram(bins = 1e2) +
  theme_bw() +
  scale_y_continuous(trans = "log10")

# vorkommen mit >100m unsicherheit entfernen
gbif_ArnMon <- gbif_ArnMon[which(gbif_ArnMon$coordinateUncertaintyInMeters < 100), ]

### Pseudo-Absenzen erzeugen

# oseudo-absenzen ziehen
background <- spatSample(x = bioclim_data,
                         size = nrow(gbif_ArnMon), # gleiche anzahl wie präsenzen
                         values = FALSE,           # werte braucht es nicht
                         na.rm = TRUE,             # nicht von na-zellen samplen
                         xy = TRUE)                # mit koordinaten

# check 
head(background)

# visualisierung
plot(Europe_map,
     axes = TRUE, 
     col = "grey95",
     main = "Präsenzen und Absenzen in Europa")

# füge absenzen hinzu
points(background,
       col = "darkgrey",
       pch = 1,
       cex = 0.75)

# und die präsenzen
points(x = gbif_ArnMon$decimalLongitude, 
       y = gbif_ArnMon$decimalLatitude, 
       col = "orange", 
       pch = 16, 
       cex = 0.5)

# koordinaten von präsenzen
ArnMon_presences <- gbif_ArnMon[, c("decimalLongitude", "decimalLatitude")]
colnames(ArnMon_presences) <- c("longitude", "latitude")

# füge spalte hinzu
ArnMon_presences$pa <- 1

# kovertiere zu dataframe
ArnMon_absences <- as.data.frame(background)
colnames(ArnMon_absences) <- c("longitude", "latitude")

# füge spalte mit absenzen hinzu
ArnMon_absences$pa <- 0

# füge zeilen der dataframes zusammen
ArnMon_PA <- rbind(ArnMon_presences, ArnMon_absences)

# Umweltinformationen für Probenahmepunkte extrahieren  
bioclim_df <- terra::extract(x = bioclim_data,
                             y = ArnMon_PA[, c("longitude", "latitude")],
                             ID = FALSE)

# Umwelt- und Artendaten in einem dataframe zusammenführen
ArnMon_complete_df <- cbind(ArnMon_PA, bioclim_df)

head(ArnMon_complete_df)


# Schritt 2: Model building

## Baue ein generalized linear model (GLM)

# build GLM
glm_ArnMon <- glm(pa ~ .,
                  data = ArnMon_complete_df[,-c(1,2)],
                  family = binomial())

# Die obige Darstellung ist eine Kurzfassung dieser Modellformel, die je nach Ihren Annahmen angepasst werden kann
#glm_ArnMon <- glm(pa ~ wc2.1_5m_bio_1+ wc2.1_5m_bio_2+wc2.1_5m_bio_3+wc2.1_5m_bio_4+ wc2.1_5m_bio_5+wc2.1_5m_bio_6+wc2.1_5m_bio_7+ wc2.1_5m_bio_8+wc2.1_5m_bio_9+wc2.1_5m_bio_10+ wc2.1_5m_bio_11+wc2.1_5m_bio_12+wc2.1_5m_bio_13+wc2.1_5m_bio_14+wc2.1_5m_bio_15+ wc2.1_5m_bio_16+wc2.1_5m_bio_17+wc2.1_5m_bio_18+ wc2.1_5m_bio_19, data = ArnMon_complete_df[,-c(1,2)],                   family = binomial())


# Schritt 3: Vorhersagen der potenziellen Verteilung

# räumliche Vorhersage von GLM in den Raum 
predict_ArnMon <- predict(bioclim_data, glm_ArnMon, type = "response")

# visualisierung
plot(predict_ArnMon)

## Extrapolation unter Klimawandel

# klimavorhersagedaten herunterladen
forecast_data <- cmip6_world(model = "MPI-ESM1-2-HR",
                             ssp = "245",
                             time = "2061-2080",
                             var = "bioc",
                             res = 5)

# verwende die selben namen
names(forecast_data) <- names(bioclim_data)

# auf europa ausschneiden
forecast_data <- crop(x = forecast_data, y = Europe_ext)

# vorhersage wo arten in zukunft auftreten
forecast_presence <- predict(forecast_data, glm_ArnMon, type = "response")

# visualisierung
plot(Europe_map, 
     axes = TRUE, 
     col = "grey95")

# modell-wahrscheinlichkeit
plot(forecast_presence, add = TRUE)

# landesgrenzen
plot(Europe_map, add = TRUE, border = "grey5")

# mit eigentlichen beobachtungen
points(x = ArnMon_presences$longitude, 
       y = ArnMon_presences$latitude, 
       col = "black", 
       pch = "+", 
       cex = 0.75)


## Finale Karte

# für ggplot transformiere raster zu dataframe
predict_ArnMon_df <- as.data.frame(predict_ArnMon, xy = TRUE)

ggplot() +
  geom_raster(data = predict_ArnMon_df, 
              aes(x = x, y = y, 
                  fill = lyr1)) +
  annotation_spatial(Europe_map, fill = NA, colour = "black")+
  scale_fill_viridis_c(name = "Wahrscheinlichkeit") +
  labs(x = "Breitengrad", 
       y = "Längengrad") +
  ggtitle("Realised niche") +
  theme_bw()+
  annotation_north_arrow(location = "tr", which_north = "true")+ # Nordpfeil
  annotation_scale(location = "tl") # Massstab

## Aufgaben


# -   Aufgabe 1: Ändere die Art und erstellen Sie Ihr eigenes SDM.  
# -   Aufgabe 2: Ändere auch das entsprechende Forschungsgebiet.  
# -   Aufgabe 3 (Fortgeschritten): Verwende zwei verschiedene Klimawandelszenarien, um die potenzielle Verbreitung deiner Zielarten vorherzusagen und die Ergebnisse zu vergleichen.  


# Bitte lade eine Abbildung deines SDMs auf StudIP unter "abbildungen_ggplot2" hoch.
