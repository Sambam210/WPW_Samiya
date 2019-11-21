############################################### I WANT TO PULL OUT JUST THE GENUS CULTIVARS ############################################

library(tidyverse)

test <- Master_Key %>%
  filter(str_detect(species, "spp.") == TRUE) %>%
  filter(str_detect(master, "spp.") == FALSE) %>%
  filter(is.na(scientificNameStd))
# https://blog.exploratory.io/filter-with-text-data-952df792c2ba

write.csv(test,"Master_database_output/genus_cultivar.csv",row.names=FALSE)

