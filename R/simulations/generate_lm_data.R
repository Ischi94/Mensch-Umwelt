library(tidyverse)
library(here)
library(gganimate)


# set seed for reproducibility
set.seed(123)
dat_div <- tibble(island = 1:50, 
       diversity = rpois(50, runif(50, 0, 100))) %>% # poisson for counts 
  mutate(size = (diversity + rnorm(50, 20, sd = 20))/50) # linearly positive relationship

# save data
dat_div %>% 
  write_rds(here("data", "island_diversity.rds"))

# create visualisaion
plot_div <- dat_div %>%
  add_row(tibble(island = 51, diversity = NA, size = 0)) %>% 
  ggplot(aes(size, diversity)) +
  geom_point(size = 2, shape = 21, fill = "#155560") +
  geom_smooth(method = "lm", colour = "orange", 
              # method.args = list(family = "poisson"),
              fullrange  = TRUE) +
  theme_minimal() +
  labs(x = "Inselgröße [km]", y = "Diversität")

# save into figures folder
ggsave(plot = plot_div, filename = "diversity_lm_1.png", path = here("figures"), bg = "white",
       width = 200, height = 100, units = "mm")

# create visualisaion
plot_div <- dat_div %>%
  add_row(tibble(island = 51, diversity = NA, size = 0)) %>% 
  ggplot(aes(size, diversity)) +
  geom_point(size = 2, shape = 21, fill = "#155560") +
  geom_smooth(method = "lm", colour = "orange", 
              formula = 'y ~1', 
              fullrange  = TRUE) +
  theme_minimal() +
  labs(x = "Inselgröße [km]", y = "Diversität")

# save into figures folder
ggsave(plot = plot_div, filename = "diversity_lm_2.png", path = here("figures"), bg = "white",
       width = 200, height = 100, units = "mm")

# perform 20 permutations 
dat_perm <- map_df(1:20, ~ mutate(dat_div, size = sample(size = 50, dat_div$size)) %>% 
                     add_column(run = .x))


# create visualisaion
plot_div_perm <- dat_perm %>%
  ggplot(aes(size, diversity, group = run)) +
  geom_point(size = 2, shape = 21, fill = "#155560") +
  geom_smooth(method = "lm", se = FALSE, colour = "orange") +
  theme_minimal() +
  labs(x = "Inselgröße [km]", y = "Diversität") 


# create gif
gif_walks <- plot_div_perm +
  transition_states(run)

# save animation into figures folder
anim_save(animation = gif_walks, filename = "diversity_lm.gif", path = here("figures"),
          nframes = 200, fps = 20, width = 1600, height = 800, res = 200, 
          end_pause = 1,
          renderer = gifski_renderer())
