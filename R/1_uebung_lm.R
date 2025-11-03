# The here package is one of the best inventions in the R universe. It allows you to the reference the
# location of an .Rproj file and uses that as a root directory. This means that you will never have to 
# set your directory again, and just use here() instead


# load the here package
library(here)

# look where your rootdirectory is pointing to 
here()

# the dataset that we are using is located in the subfolder data/ and called "island_diversity.rds"
# using the here function, we can load it like this and assign it to a variable 
dat_div <- readRDS(here("data", "island_diversity.rds"))

# use the mean function to calculate the mean duration across all occurrences
# input your code below

# model the mean across all observations with a intercept-only linear model
# assign this to the variable mod1
mod1 <- 

# use the summary function to look at the output of the model
# compare the estimate for the intercept with the mean that you calculated above.
# does the model give us the same mean?


# now fit a linear model with diversity as the outcome and island size (in km) as the exposure, 
# explicitely asking how diversity changes with the size of the island. 
# assign this model to the variable mod2


# use the summary function to look at the output of the model. How do you interpret the model results?

