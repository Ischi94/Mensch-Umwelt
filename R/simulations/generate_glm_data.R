library(MASS)
library(tidyverse)
library(here)



# load island dataset
dat_div <- readRDS(here("data", "island_diversity.rds"))

# linear model to get residuals
mod_div <- lm(diversity ~ size, data = dat_div)

# residual diversity
dat_div$resid_div <- residuals(mod_div)


# set seed for reproducibility
set.seed(123)

# add additional non-normal columns
dat_div_bin <- dat_div %>% 
  mutate(latent_humans  =  1.5 * scale(size)[,1] + # humas prefer large island
           -1.2 * scale(resid_div)[,1] + # but reduce diversity
           rnorm(n(), 0, 0.7), # rauschen
         human_presence = if_else(latent_humans > quantile(latent_humans, 0.6), 1, 0))


# add forest cover
set.seed(123)
dat_div_beta <- dat_div_bin %>%
  mutate(size_sc = scale(size)[,1], # standardise size
    # interaction: humans on small island
    small_island = -size_sc,   # large: negativ, small: positiv
    # latent forest prob
    latent_forest =
      1.1  * size_sc +        # larger islands -> more forest
      -2.5 * human_presence +        # opposite
      -2.0 * human_presence * small_island +  # pronounced at small islands
      rnorm(n(), 0, 0.6),     # noise
    # logit-transform to 0-1
    forest_cover = plogis(latent_forest)
  )


# add island ecotype
set.seed(123)
dat_div_cat <- dat_div_beta %>%
  mutate(
    # tropical: large, diverse islands
    # boreal: small, depauperate islands
    # temperate: between
    eco_score = 
      0.7 * scale(size)[,1] +
      0.7 * scale(diversity)[,1] +
      0.5 * scale(forest_cover)[,1] +
      rnorm(n(), 0, 0.5),
    
    ecosystem = case_when(
      eco_score >  0.7 ~ "tropical",
      eco_score < -0.7 ~ "boreal",
      TRUE              ~ "temperate"
    ),
    
    ecosystem = factor(ecosystem, levels = c("tropical", "temperate", "boreal"))
  )

# remove redundant columns
dat_div_fin <- dat_div_cat %>% 
  select(island, diversity, size, human_presence, forest_cover, ecosystem)

# interaction plot
ggplot(dat_div_fin, aes(size, diversity, color = ecosystem)) +
  geom_point(size = 3) +
  geom_hline(aes(yintercept = diversity, 
                 colour = ecosystem), 
             data = . %>% 
               group_by(ecosystem, human_presence) %>% 
               summarise(diversity = mean(diversity))) +
  facet_wrap(~human_presence)

# save data
dat_div_fin %>% 
  write_csv(here("data", "island_diversity_extended.csv"))


# individual plots
# humans
plot_bin <- dat_div_fin %>% 
  ggplot(aes(human_presence, diversity)) +
  geom_point(size = 2, shape = 21, fill = "#155560", 
             position = position_jitter(width = 0.01, seed = 1)) +
  geom_smooth(method = "lm", colour = "orange",
              fullrange  = TRUE) +
  scale_x_continuous(breaks = c(0, 1), 
                     labels = c("Nein", "Ja")) +
  theme_minimal() +
  labs(y = "Diversität", x = "Menschenpräsenz", 
       title = "Binäres Beispiel")

# save into figures folder
ggsave(plot = plot_bin, filename = "diversity_glm_1.png", path = here("figures"), bg = "white",
       width = 200, height = 100, units = "mm")

# forest_cover 
plot_prop <- dat_div_fin %>%
  ggplot(aes(forest_cover, diversity)) +
  geom_point(size = 2, shape = 21, fill = "#155560", 
             position = position_jitter(width = 0.01, seed = 1)) +
  geom_smooth(method = "lm", colour = "orange",
              fullrange  = TRUE) +
  theme_minimal() +
  labs(y = "Diversität", x = "Proportionale Waldbedeckung", 
       title = "Proportionales Beispiel")

# save into figures folder
ggsave(plot = plot_prop, filename = "diversity_glm_2.png", path = here("figures"), bg = "white",
       width = 200, height = 100, units = "mm")


# ecosystem
plot_cat <- dat_div_fin %>%
  mutate(ecosystem = str_to_sentence(ecosystem)) %>%
  left_join(tibble(ecosystem = c("Boreal", "Temperate", "Tropical"), 
                   eco_int = 1:3)) %>% 
  ggplot(aes(ecosystem, diversity)) +
  geom_point(size = 2, shape = 21, fill = "#155560", 
             position = position_jitter(width = 0.01, seed = 1)) +
  geom_smooth(aes(x = eco_int), 
              method = "lm", colour = "orange",
              fullrange  = TRUE) +
  theme_minimal() +
  labs(y = "Diversität", x = "Ökosystem", 
       title = "Kategorisches Beispiel")

# save into figures folder
ggsave(plot = plot_cat, filename = "diversity_glm_3.png", path = here("figures"), bg = "white",
       width = 200, height = 100, units = "mm")


# logistic regression
plot_log <- dat_div_fin %>%
  ggplot(aes(size, human_presence)) +
  geom_point(size = 2, shape = 21, fill = "#155560", 
             position = position_jitter(height = 0.03, seed = 1)) +
  geom_smooth(method = "glm", colour = "orange",
              method.args = list(family = "binomial"),
              fullrange  = TRUE) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  labs(y = "P(Menschenpräsenz)", x = "Inselgröße [km]")

# save into figures folder
ggsave(plot = plot_log, filename = "diversity_glm_4.png", path = here("figures"), bg = "white",
       width = 200, height = 100, units = "mm")

# logit-vals
mod_binom <- glm(human_presence ~ size, 
                 data = dat_div_fin, 
                 family = binomial)

# get log odds
dat_binom <- data.frame(size = seq(min(dat_div_fin$size),
                                   max(dat_div_fin$size),
                                   length.out = 50)) %>% 
  mutate(log_odd = predict(mod_binom, newdata = ., type = "link"), 
         log_odd_se = predict(mod_binom, newdata = ., type = "link", se.fit = TRUE)[[2]], 
         log_odd_low = log_odd - 1.96 * log_odd_se, 
         log_odd_high = log_odd + 1.96 * log_odd_se, 
         log_point_low = log_odd - 6 * log_odd_se, 
         log_point_high = log_odd + 6 * log_odd_se) %>% 
  rowwise() %>% 
  mutate(log_point = runif(1, log_point_low, log_point_high))

# plot
plot_log_raw <- ggplot() +
  geom_point(aes(size, log_point),
             size = 2, shape = 21, fill = "#155560", 
             data = dat_binom) +
  geom_ribbon(aes(ymin = log_odd_low, ymax = log_odd_high, y = log_odd, 
                  x = size), 
              fill = "grey20", 
              colour = "white",
              alpha = 0.2,
              data = dat_binom) +
  geom_line(data = dat_binom, aes(size, log_odd), 
            colour = "orange", linewidth = 1.2) +
  theme_minimal() +
  labs(y = "Log-odds der Menschenpräsenz", x = "Inselgröße [km]")

# save into figures folder
ggsave(plot = plot_log_raw, filename = "diversity_glm_5.png", path = here("figures"), bg = "white",
       width = 200, height = 100, units = "mm")
