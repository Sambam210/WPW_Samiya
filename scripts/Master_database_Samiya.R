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

# library(tidyverse)

# glasshouse <- Master_Key %>%
#  filter(!is.na(gl)) %>% # some species have multiple entries as we tested multiple varieties
#  select(master, scientificNameStd, gl, tr)

# open traits table manually

# extract all the data for the glasshouse species from the traits table
# all_gl_traits <- left_join(glasshouse, traits, by = c("tr" = "newspecies"))

# write.csv(all_gl_traits, "Master_database_output/all_gh_traits.csv", row.names = FALSE)

# pick out the species that have no traits in the database

# missing <- glasshouse %>%
#  filter(is.na(tr))
# most of these missing species are actually varieties, do we have traits for the species in the trait database?

# missing_traits_species_level <- left_join(missing, traits, by = c("scientificNameStd" = "newspecies"))

# join to all glasshouse traits

# all_gl_traits2 <- bind_rows(all_gl_traits, missing_traits_species_level)

# all_gl_traits2 <- arrange(all_gl_traits2, master)

# write.csv(all_gl_traits2, "Master_database_output/all_gh_traits.csv", row.names = FALSE)

# there are some species in the trait database that are cultivars/varieties and we have traits for them but we also want traits for the 
# actual species

# all_gh_traits <- read.csv("Master_database_output/all_gh_traits.csv")

########################################################################################################################

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

# join traits table by matching names in master column

# load traits table

# rename species column so it is unique from the onw in the other database

traits_new <- rename(traits, species_tr = species)

# join

everything <- left_join(all_entities, traits_new, by = c("master" = "newspecies"))

# add back the newspecies column

everything$newspecies <- traits_new$newspecies[match(everything$master, traits_new$newspecies)]
# https://stackoverflow.com/questions/37034242/r-add-a-new-column-to-a-dataframe-using-matching-values-of-another-dataframe/37034516

# organise

everything <- select(everything, master, scientificNameStd, gl, tr, species, plantType, origin, Min_5_traits,
                     Min_8_traits, category, newspecies, species_tr, List_source, date_sourced, verification, species_number,
                     multiple_forms, source, trait_index, trait_name_original, trait_name,
                     value_original, value)

write.csv(everything, "Master_database_output/EVERYTHING_gh.csv", row.names = FALSE)

# remove some mistakes
everything <- read.csv("Master_database_output/EVERYTHING_gh.csv")

# filter out entries with no scientific name and gh name
extra <- everything %>%
  filter(is.na(scientificNameStd) , is.na(gl))

# remove them from everything dataframe
everything_new <- anti_join(everything, extra)

# check it worked
check <- distinct(everything_new, gl) # 116 entires, worked!!!

write.csv(everything_new, "Master_database_output/EVERYTHING_gh.csv", row.names = FALSE)

# NOTE: need to manually add in Photinia robusta, Arthropodium cirrhatum, Ficus microcarpa Hillii and others traits

###########################################################################################

# how many traits do we have for each species?

# all <- read.csv("Master_database_output/all_gh_traits.csv")

# unique <- all %>%
#  distinct(master, trait_name) %>%
#  add_count(master, sort = TRUE, name = "n_traits") %>%
#  select(-trait_name) %>%
#  distinct(master, .keep_all = TRUE) %>%
#  arrange(master)

# write.csv(unique, "Master_database_output/gh_traits_completeness.csv", row.names = FALSE)

#########################################################################################################

# let's extract the all the other species present in the traits database

library(tidyverse)

# open entities table

entities_tr <- entities %>%
  filter(!is.na(tr))

# extract the master names

master_names <- entities_tr %>%
  select(master) %>%
  distinct(master)

# extract those master names from the entities table

all_entities <- left_join(master_names, entities, by = "master")

# tidy up

all_entities <- select(all_entities, -ACT, -NSW, -NT, -QLD, -SA, -VIC, -WA, -numGrowers, -numStates)

# join traits table by matching names in master column

# load traits table

# rename species column so it is unique from the onw in the other database

traits_new <- rename(traits, species_tr = species)

# join

everything <- left_join(all_entities, traits_new, by = c("master" = "newspecies"))

# add back the newspecies column

everything$newspecies <- traits_new$newspecies[match(everything$master, traits_new$newspecies)]

# organise

everything <- select(everything, master, scientificNameStd, gl, tr, species, plantType, origin, Min_5_traits,
                     Min_8_traits, category, newspecies, species_tr, List_source, date_sourced, verification, species_number,
                     multiple_forms, source, trait_index, trait_name_original, trait_name,
                     value_original, value)

# load the gh species I already did

gh_species <- read.csv("Master_database_input/EVERYTHING_gh.csv")

# select only the master names

gh_species_names <- gh_species %>%
  select(master) %>%
  distinct(master)

# remove them from everything dataframe
everything_new <- anti_join(everything, gh_species_names)

write.csv(everything_new, "Master_database_output/EVERYTHING_traits.csv", row.names = FALSE)

################################################################################################################################

# summary stats for glasshouse manual traits collection

library(tidyverse)

everything <- read.csv("Master_database_input/EVERYTHING_gh.csv")

everything_summary <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  distinct(species, category) %>%
  group_by(category) %>%
  summarise(frequency = n())

#########################################################################################################

# let's extract the all the other species present in the traits database

library(tidyverse)

# open entities table

entities_tr <- entities %>%
  filter(!is.na(tr))

# extract the master names

master_names <- entities_tr %>%
  select(master) %>%
  distinct(master)

# extract those master names from the entities table

all_entities <- left_join(master_names, entities, by = "master")

# tidy up

all_entities <- select(all_entities, -ACT, -NSW, -NT, -QLD, -SA, -VIC, -WA, -numGrowers, -numStates)

# join traits table by matching names in master column

# load traits table

# rename species column so it is unique from the onw in the other database

traits_new <- rename(traits, species_tr = species)

# join

everything <- left_join(all_entities, traits_new, by = c("master" = "newspecies"))

# add back the newspecies column

everything$newspecies <- traits_new$newspecies[match(everything$master, traits_new$newspecies)]

# organise

everything <- select(everything, master, scientificNameStd, gl, tr, species, plantType, origin, Min_5_traits,
                     Min_8_traits, category, newspecies, species_tr, List_source, date_sourced, verification, species_number,
                     multiple_forms, source, trait_index, trait_name_original, trait_name,
                     value_original, value)

# load the gh species I already did

gh_species <- read.csv("Master_database_input/EVERYTHING_gh.csv")

# select only the master names

gh_species_names <- gh_species %>%
  select(master) %>%
  distinct(master)

# remove them from everything dataframe
everything_new <- anti_join(everything, gh_species_names)

write.csv(everything_new, "Master_database_output/EVERYTHING_traits.csv", row.names = FALSE)

###########################################################################################################################

# summary stats for all manual traits collection

library(tidyverse)

everything <- read.csv("Master_database_output/EVERYTHING_traits.csv")

everything_summary <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  distinct(species, category) %>%
  group_by(category) %>%
  summarise(frequency = n())







