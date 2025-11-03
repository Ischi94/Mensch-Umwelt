# load the here library
library(here)

# load the diversity dataset located in the subfolder data/ and called "island_diversity.rds"
dat_div <- readRDS(here())

# build a simple linear model with diversity as the outcome and island size as the predictor 
# and save it as mod1
mod1 <- lm()


# extract the estimate for the intercept effect of island size on diversity via the coef function
# and save it into dat_coef
dat_coef <- 

# subset the estimate for the slope and interprete it


# read through the confint function description via ?confint
?confint

# now use the confint function to exctract the 95% confidence intervals (corresponds to alpha = 0.05)
# for the slope value, i.e. the estimated effect of island size on diversity
# save the estimates in conf_slope
conf_slope <- 

# do the confidence intervals intersect with zero?
conf_slope

# now let us improve our null distribution by means of permutation
# what we want is a new dataset where we randomly shuffled island size for each island, thereby 
# breaking apart any potential relationship between diversity and size while keeping the characteristics 
# of the data

# we can reshuffle the size column in the dataset by randomly sampling without replacement from the column itself 
sample(dat_div$size, size = nrow(dat_div), replace = FALSE)

# each time you run the code above, it will output the size column in a new order
# we can save our permutated estimates in a new dataframe like this
dat_new <- data.frame(island = dat_div$island, diversity = dat_div$diversity, 
                      size = sample(dat_div$size, size = nrow(dat_div), replace = FALSE))

# now we want to do this 100 times, for this we need to setup a list with 100 slots to save our data into
list_perm <- vector(mode = "list", length = 100)

# now you need to set up a loop which permutates the data 100 times and then saves it into the focal slot
# of list_perm
for (i in 1:100) {
  
  list_perm[[i]] <- 
    
}

# now that we have 100 permutated datasets, we can setup a new loop that fits a lm for each, 
# and extracts the slope value, but first we need to setup a new list with 100 slots for the slopes
list_slopes <- 

# now the loop through list_perm
for (i in 1:100) {
  
  # fit the linear model
  mod <- lm(diversity ~ size, data = )
  
  # extract the slope parameter from the model (see above)
  list_slopes[[i]] <- 
  
  
}

# now we have 100 slope values created under the null hypothesis. We can get them into a vector
# by applying unlist() to the list_slopes. Save this into vec_slopes
vec_slopes <-

# how does our empirical slope value (dat_coef[[2]]) compare to this distribution? Can we reject
# the null hypothesis?
