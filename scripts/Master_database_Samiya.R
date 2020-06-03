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

# extract the scientific names

sci_names <- entities_tr %>%
  select(scientificNameStd) %>%
  distinct(scientificNameStd)

# extract those sci names from the entities table

all_entities <- left_join(sci_names, entities, by = "scientificNameStd")

# join the missing species

missing <- entities_tr %>%
  filter(is.na(scientificNameStd))

all_entities <- bind_rows(all_entities, missing)

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

# filter out entries with no scientific name and tr name
extra <- everything %>%
  filter(is.na(scientificNameStd) , is.na(tr))

# remove them from everything dataframe
everything_new <- anti_join(everything, extra)

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
  distinct(master, category) %>%
  group_by(category) %>%
  summarise(frequency = n())

###########################################################################################################################

# summary stats for all manual traits collection

library(tidyverse)

everything <- read.csv("Master_database_input/EVERYTHING_traits_26May.csv")

everything_summary <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  distinct(master, category) %>%
  group_by(category) %>%
  summarise(frequency = n())

##############################################################################################################################

# summary of all the species i have minimum traits for for Linda

library(tidyverse)

everything <- read.csv("Master_database_output/EVERYTHING_traits_30_March.csv")

everything_summary <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(category == "SP" | category == "SYN") %>%
  distinct(scientificNameStd)

everything_gh <- read.csv("Master_database_output/EVERYTHING_gh.csv")

everything_gh_summary <- everything_gh %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(category == "SP" | category == "SYN") %>%
  distinct(scientificNameStd)

# join together

min_traits <- bind_rows(everything_summary, everything_gh_summary)

min_traits <- min_traits %>%
  distinct(scientificNameStd) %>%
  arrange(scientificNameStd)

write.csv(min_traits, "Master_database_output/summary_traits_Linda.csv", row.names = FALSE)

#########################################################################################################

# Extracting data for Ale

library(tidyverse)

ale <- read.csv("Master_database_input/tree_species_summary_AO.csv")

traits <- read.csv("Master_database_input/EVERYTHING_traits_26May.csv")

traits <- select(traits, species, origin, trait_name, value)

traits <- filter(traits, trait_name == "leaf_loss"| trait_name == "form"| 
                 trait_name == "max_height_nature"| trait_name == "max_height"|
                 trait_name == "height"| trait_name == "max_width"|
                 trait_name == "width"| trait_name == "habit_canopy")

ale_all_traits <- left_join(ale, traits, by = "species")

# add the gh species
gh <- read.csv("Master_database_input/EVERYTHING_gh.csv")

gh <- select(gh, species, origin, trait_name, value)

gh <- filter(gh, trait_name == "leaf_loss"| trait_name == "form"| 
                   trait_name == "max_height_nature"| trait_name == "max_height"|
                   trait_name == "height"| trait_name == "max_width"|
                   trait_name == "width"| trait_name == "habit_canopy")

ale_gh_traits <- left_join(ale, gh, by = "species")

# join both datasets
all_traits <- bind_rows(ale_all_traits, ale_gh_traits)

all_traits <- arrange(all_traits, species)

all_traits <- distinct(all_traits, species, origin, trait_name, value)

all_traits <- na.omit(all_traits)

write.csv(all_traits, "Master_database_output/tree_species_summary_traits_ST.csv", row.names = FALSE)

#######################################################################################

# Extracting data for the species that have 5 min traits but traits need
# to be manually added

library(tidyverse)

david_traits <- read.csv("Master_database_input/UptodateDB.csv")

extract <- filter(david_traits, newspecies == "Datura sanguinea" | newspecies == "Convolvulus sabatius mauritanicus" 
                  | newspecies == "Cyathea cooperi" | newspecies =="Dodonaea viscosa subsp angustissima" | 
                    newspecies == "Dodonaea viscosa subsp cuneata" | newspecies == "Philotheca myoporoides Profusion" | 
                    newspecies == "Philotheca myoporoides Winter Rouge" | newspecies == "Eucalyptus camaldulensis var obtusa" | 
                    newspecies == "Eucalyptus cladocalyx nana" | newspecies == "Eucalyptus diversifolia subsp diversifolia" | 
                    newspecies == "Grapophyllum ilicifolium" | newspecies == "Hebe buxifolia" | 
                    newspecies == "Hibiscus rosasinensis" | newspecies == "Omalanthus populifolius" | 
                    newspecies == "Koelreuteria bipinnata" | newspecies == "Liriope spicata Variegata" | 
                    newspecies == "Lomandra confertifolia Wingarra" | newspecies == "Lomandra fluvitalis Shara" | 
                    newspecies == "Mahonia lomariifolia" | newspecies == "Melaleuca nesophila Little Nessy" | 
                    newspecies == "Microlaena stipoides")

extract <- arrange(extract, newspecies, trait_index)

write.csv(extract, "Master_database_output/missing_traits_5mintraits.csv", row.names = FALSE)
# some species numbers missing, need to check

#########################################################################################################

# stats for milestone report

library(tidyverse)

everything <- read.csv("Master_database_input/EVERYTHING_traits_26May.csv")

gh <- read.csv("Master_database_input/EVERYTHING_gh.csv")

# join both together

whatihave <- bind_rows(everything, gh)

# summarise by plant type
summary <- whatihave %>%
  filter(Min_5_traits == "TRUE") %>%
  distinct(master, plantType) %>%
  group_by(plantType) %>%
  summarise(frequency = n())

# summarise how many data points I have entered
summary <- whatihave %>%
  group_by(verification) %>%
  summarise(frequency = n())

# summarise in terms of number of street trees
summary <- whatihave %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(trait_name == "placement") %>%
  filter(value == "street" | value == "avenue" | value == "powerline") %>%
  distinct(master) %>%
  summarise(frequency = n())
  
####################################################################################
