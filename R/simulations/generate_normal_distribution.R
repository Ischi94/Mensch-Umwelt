library(here)
library(tidyverse)
library(gganimate)

# define function for one random walk
random_walk <- function(n_steps = 16) {
  vec_pos <- rep(0, n_steps + 1)
  vec_step <- runif(n_steps, -1, 1)
  
  for (i in 1:n_steps) {
    vec_pos[i+1] <- vec_pos[i] + vec_step[i]
  }
  
  tibble(step = 0:n_steps, pos = vec_pos)
}

# replicate 100 times
nr_reps <- 100
dat_walks <- map_df(1:nr_reps, ~random_walk()) %>% 
  add_column(iter = rep(1:nr_reps, each = 17))

# create visualisaion
plot_walks <- dat_walks %>%
  mutate(col_id = if_else(iter == 100, "#C2A337", "#155560"), 
         alpha_id = if_else(iter == 100, 1, 0.2)) %>% 
  ggplot(aes(step, pos, 
             group = iter, 
             col = col_id, 
             alpha = alpha_id)) +
  geom_line() +
  scale_color_identity() +
  scale_alpha_identity() +
  theme_minimal() +
  labs(y = "Position in field", 
       x = "Step number")

# save into figures folder
ggsave(plot = plot_walks, filename = "normal_dist_sim.png", path = here("figures"),
       bg = "white",
       width = 200, height = 100, units = "mm")

# create histogram
plot_hist_walks <- dat_walks %>% 
  ggplot(aes(pos)) +
  geom_density(colour = "#155560", 
               linewidth = 2) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) +
  theme_minimal() +
  labs(y = "Density", 
       x = "Position in field")

# save into figures folder
ggsave(plot = plot_hist_walks, filename = "normal_dist_hist.png", path = here("figures"),
       bg = "white",
       width = 100, height = 100, units = "mm")

# create gif
gif_walks <- plot_walks +
  transition_reveal(step)

# save animation into figures folder
anim_save(animation = gif_walks, filename = "normal_dist_sim.gif", path = here("figures"),
          nframes = 400, fps = 20, width = 1600, height = 800, res = 200, 
          end_pause = 50,
          renderer = gifski_renderer())
