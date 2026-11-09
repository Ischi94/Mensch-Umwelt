library(here)
library(tidyverse)

tribble(~ taxon, ~ experience_years, ~ mean_change_perception,
        "birds",8,-0.4,
        "birds",12,-0.5,
        "birds",18,-1.0,
        "birds",25,-0.7,
        "birds",32,-1.3,
        "birds",38,-1.2,
        "birds",45,-1.6, 
        "invertebrates",6,-0.9,
        "invertebrates",12,-1.1,
        "invertebrates",20,-1.3,
        "invertebrates",28,-1.0,
        "invertebrates",35,-1.2,
        "invertebrates",42,-1.4,
        "invertebrates",50,-1.3,
        "reef_fish",7,-1.0,
        "reef_fish",14,-1.2,
        "reef_fish",20,-1.4,
        "reef_fish",26,-1.3,
        "reef_fish",32,-1.1,
        "reef_fish",40,-1.5,
        "reef_fish",48,-1.6,
        "turtles",6,-1.0,
        "turtles",12,-1.1,
        "turtles",18,-1.3,
        "turtles",25,-1.0,
        "turtles",33,-1.2,
        "turtles",40,-1.4)


tibble(fishing_sites = c(1, 2, 5), 
       fishing_species = c(1, 7, 11),
       age_cat = c("younger", "middle", "older"))




