############################################### I WANT TO PULL OUT JUST THE GENUS CULTIVARS ############################################

library(tidyverse)

test <- Master_Key %>%
  filter(str_detect(species, "spp.") == TRUE) %>%
  filter(str_detect(master, "spp.") == FALSE) %>%
  filter(is.na(scientificNameStd))
# https://blog.exploratory.io/filter-with-text-data-952df792c2ba

write.csv(test,"Master_database_output/genus_cultivar.csv",row.names=FALSE)

########################################################################################################################################

## pick out the species from the master key that were grown in the glasshouse and have a model

library(tidyverse)

whatwehave <- Master_Key %>%
  filter(!is.na(gl) | !is.na(mod))
# 2121 species

# list of glasshouse species and their scientific names

glasshouse <- whatwehave %>%
  filter(!is.na(gl)) # some species have multiple entries as we tested multiple varieties

write.csv(glasshouse, "Master_database_output/glasshouse_list.csv", row.names = FALSE)

glasshouse <- whatwehave %>%
  filter(!is.na(gl)) %>%
  distinct(scientificNameStd, .keep_all = TRUE) # add correa, cynodon and plantanus. In traits database, Correa is Correa pulchella, cynodon is Cynodon dactylon and plantanus is Plantanus acerifolia. These are not recognised scientific names.

###################################################################################################################################################

# pull out all the traits for the glasshouse species

library(tidyverse)

glasshouse <- Master_Key %>%
  filter(!is.na(gl)) %>% # some species have multiple entries as we tested multiple varieties
  select(master, scientificNameStd, gl, tr)

# open traits table manually

# extract all the data for the glasshouse species from the traits table
all_gl_traits <- left_join(glasshouse, traits, by = c("tr" = "newspecies"))

# write.csv(all_gl_traits, "Master_database_output/all_gh_traits.csv", row.names = FALSE)

# pick out the species that have no traits in the database

missing <- glasshouse %>%
  filter(is.na(tr))
# most of these missing species are actually varieties, do we have traits for the species in the trait database?

missing_traits_species_level <- left_join(missing, traits, by = c("scientificNameStd" = "newspecies"))

# join to all glasshouse traits

all_gl_traits2 <- bind_rows(all_gl_traits, missing_traits_species_level)

all_gl_traits2 <- arrange(all_gl_traits2, master)

write.csv(all_gl_traits2, "Master_database_output/all_gh_traits.csv", row.names = FALSE)

# there are some species in the trait database that are cultivars/varieties and we have traits for them but we also want traits for the 
# actual species

all_gh_traits <- read.csv("Master_database_output/all_gh_traits.csv")

# let's take a step back
# for all the gh species I want all the cultivars/varieties we have

# open entities table

entities_gh <- entities %>%
  filter(!is.na(gl))

# extract the scientific names

sci_names <- entities_gh %>%
  select(scientificNameStd) %>%
  distinct(scientificNameStd)
# we are missing some species

# extract those scientific names from the entities table

all_entities <- left_join(sci_names, entities, by = "scientificNameStd")

# join the missing species

missing <- entities_gh %>%
  filter(is.na(scientificNameStd))

all_entities <- bind_rows(all_entities, missing)

# tidy up

all_entities <- select(all_entities, -ACT, -NSW, -NT, -QLD, -SA, -VIC, -WA, -numGrowers, -numStates)

write.csv(all_entities, "Master_database_output/all_entities_gh.csv", row.names = FALSE)

# 

##############################

# how many traits do we have for each species?

all <- read.csv("Master_database_output/all_gh_traits.csv")

unique <- all %>%
  distinct(master, trait_name) %>%
  add_count(master, sort = TRUE, name = "n_traits") %>%
  select(-trait_name) %>%
  distinct(master, .keep_all = TRUE) %>%
  arrange(master)

write.csv(unique, "Master_database_output/gh_traits_completeness.csv", row.names = FALSE)



