
# load packages -----------------------------------------------------------

library(tidyverse)


# read data ---------------------------------------------------------------

fish <- read_csv("chap12q19ElectricFish.csv")


# put data in tidy format ------------------------------------------------

fish_long <- 
  pivot_longer(fish, speciesUpstream:speciesDownstream,
               names_to = "location",
               values_to = "species") %>% 
  mutate(location = str_remove(location, c("species"))) %>% 
  print()


#Question A

ttest_results <- t.test(formula = species ~ location, data = fish_long)

ttest_results


# Question B --------------------------------------------------------------




# Question C --------------------------------------------------------------

fish_long %>% 
  ggplot(aes(x=species)) +
  geom_histogram(
    aes(fill=location),
    bins=10,
    alpha = 0.5,
    position = "identity"
  ) +
  scale_fill_manual(values = c("darkorange", "cyan4"))+
  theme_minimal()



# Question D --------------------------------------------------------------

crab_sex <- read_csv("chap15q27FiddlerCrabFans.txt")


  crab_sex %>% 
    ggplot(aes(x = bodyTemperature)) +
    geom_histogram(
      aes(fill = crabType), 
      bins = 8, 
      alpha = 0.5, 
      position = "identity",
      na.rm = TRUE
    ) +
    scale_fill_manual(values = c("darkorange", "red", "cyan4", "blue")) +
    theme_minimal()
  


# Question E --------------------------------------------------------------


  aov_crabs <-
    aov(bodyTemperature ~ crabType, data = crabs)
  aov_crabs
  
  summary(aov_crabs)

  
