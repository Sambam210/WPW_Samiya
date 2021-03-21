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

############
# Sending the list again to Linda

everything <- read.csv("Master_database_input/EVERYTHING_traits_24June.csv")

# species with traits in the database
all_entities <- everything %>%
  filter(category == "SP" | category == "SYN") %>%
  distinct(scientificNameStd, tr) %>%
  drop_na(tr) %>% 
  select(scientificNameStd)

# species with 5 min traits in the database
min5traits <- everything %>%
  filter(category == "SP" | category == "SYN") %>%
  filter(Min_5_traits == "TRUE") %>%
  distinct(scientificNameStd, tr) %>%
  select(scientificNameStd)
  
write.csv(all_entities, "Master_database_output/all_species_Linda.csv", row.names = FALSE)

write.csv(min5traits, "Master_database_output/min5traits_Linda.csv", row.names = FALSE)

##################
# Sending the list to Ale for photos

everything <- read.csv("Master_database_input/EVERYTHING_traits_16Feb2021.csv")

gh <- read.csv("Master_database_input/EVERYTHING_gh_16Feb2021.csv")

# join together

all <- bind_rows(everything, gh)

all <- select(all, scientificNameStd, species, plantType, origin, category, Min_5_traits)

all <- filter(all, Min_5_traits == "TRUE")

all <- select(all, -scientificNameStd)

all <- distinct(all, species, .keep_all = TRUE)

# summarise

summary_category <- all %>%
  group_by(category) %>%
  summarise(frequency = n())

summary_plantType <- all %>%
  group_by(plantType) %>%
  summarise(frequency = n())

summary <- all %>%
  group_by(plantType, category) %>%
  summarise(frequency = n())

write.csv(all, "Master_database_output/min5traits_ale_Feb2021.csv", row.names = FALSE)

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

################

# Aerotropolis species

aero <- read.csv("Master_database_input/SP_AERO.csv")

traits <- read.csv("Master_database_input/EVERYTHING_traits_24June.csv")

gh <- read.csv("Master_database_input/EVERYTHING_gh.csv")

all_traits <- bind_rows(traits, gh)

all_traits <- select(all_traits, master, origin, trait_name, value)

aero <- aero %>%
  select(SPECIES_AERO) %>%
  rename(master = SPECIES_AERO)

aero_traits <- left_join(aero, all_traits, by = "master")

aero_traits <- rename(aero_traits, species = master)

# see how many species we have traits for
aero_traits_summary <- distinct(aero_traits, species)

# save trait output
write.csv(aero_traits, "Master_database_output/aero_traits.csv", row.names = FALSE)

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
                    newspecies == "Microlaena stipoides" | newspecies == "Lomandra fluviatilis Shara" | 
                    newspecies == "Photinia x fraseri Red Robin" | newspecies == "Salvia microphylla Hotlips" | 
                    newspecies == "Scaevola Super Clusters" | newspecies == "Sedum spectabile" | 
                    newspecies == "Tabebuia palmeri" | newspecies == "Themeda triandra Mingo" | 
                    newspecies == "Westringia fruticosa MUNDI" | newspecies == "Bergenia crassifolia" | 
                    newspecies == "Buxus sempervirens Suffruticosa" | newspecies == "Dianella tasmanica Tasred" | 
                    newspecies == "Eucalyptus leucoxylon subsp megalocarpa")

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

# Summary of the database so far
# June 2020

library(tidyverse)

everything <- read.csv("Master_database_input/EVERYTHING_traits_24June.csv")

# number of entities in the database
all_entities <- everything %>%
  distinct(tr)
# 4179 distinct entities in the database

# group by plant type
all_entities_summary <- everything %>%
  distinct(tr, category) %>%
  group_by(category) %>%
  summarise(frequency = n())

#blah <- everything %>%
#  distinct(tr, category) %>%
#  drop_na(tr) %>%
#  group_by(tr) %>%
#  summarise(frequency = n()) %>%
#  filter(frequency != 1)

# entities with the 5 minimum traits

five_min_traits <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  distinct(species, category) %>%
  group_by(category) %>%
  summarise(frequency = n())
  
# summarising in terms of plant type

plant_type <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(category != "GC") %>%
  distinct(species, plantType) %>% 
  group_by(plantType) %>%
  summarise(frequency = n())

# summarising in terms of origin

origin <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(category != "GC") %>%
  distinct(species, origin) %>%
  group_by(origin) %>%
  summarise(frequency = n())  
  
# summarise by native species with 5 min traits, states found

# first need to see if there are any native species that do not have the states found trait
#states <- everything %>%
#  filter(Min_5_traits == "TRUE") %>%
#  filter(origin == "Native") %>%
#  filter(category == "SP" | category == "SYN") %>%
#  distinct(species)

#states_found <- everything %>%
#  filter(Min_5_traits == "TRUE") %>%
#  filter(trait_name == "states_found") %>%
#  distinct(species)
  
#missing <- anti_join(states, states_found) # 0 species

states_found <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(origin == "Native") %>%
  filter(trait_name == "states_found") %>%
  group_by(value) %>%
  summarise(frequency = n())

# trait completeness

# entities with the 5 minimum traits (without the GCs)

species <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(category != "GC") %>%
  distinct(species)
# 2060 entities

traits <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(category != "GC") %>%
  distinct(species, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2060)*100)

write.csv(traits, "Master_database_output/traitcompleteness_June2020.csv", row.names = FALSE)

# if some of the 5 min traits have less than 2060 then use the following code

# leaf loss
#leaf <- everything %>%
#  filter(Min_5_traits == "TRUE") %>%
#  filter(category != "GC") %>%
#  distinct(species, trait_name) %>%
#  filter(trait_name == "leaf_loss") %>%
#  select(species)

#missing_leaf <- anti_join(species, leaf) # 2 species

# light level
#light <- everything %>%
#  filter(Min_5_traits == "TRUE") %>%
#  filter(category != "GC") %>%
#  distinct(species, trait_name) %>%
#  filter(trait_name == "light_level") %>%
#  select(species)

#missing_light <- anti_join(species, light) # 2 species  

# max height
#height <- everything %>%
#  filter(Min_5_traits == "TRUE") %>%
#  filter(category != "GC") %>%
#  distinct(species, trait_name) %>%
#  filter(trait_name == "max_height") %>%
#  select(species)

#missing_height <- anti_join(species, height) # 2 species
  
# max width
#width <- everything %>%
#  filter(Min_5_traits == "TRUE") %>%
#  filter(category != "GC") %>%
#  distinct(species, trait_name) %>%
#  filter(trait_name == "max_width") %>%
#  select(species)

#missing_width <- anti_join(species, width) # 4 species

# placement stats
placement <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(category != "GC") %>%
  filter(trait_name == "placement") %>%
  group_by(value) %>%
  summarise(frequency = n())

# Summary of the database so far
# October 2020

library(tidyverse)

everything <- read.csv("Master_database_input/EVERYTHING_traits_13Oct.csv")

# number of entities in the database
all_entities <- everything %>%
  distinct(tr)
# 4285 distinct entities in the database

# group by plant type
all_entities_summary <- everything %>%
  distinct(tr, category) %>%
  group_by(category) %>%
  summarise(frequency = n())

# entities with the 5 minimum traits

five_min_traits <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  distinct(species, category) %>%
  group_by(category) %>%
  summarise(frequency = n())

# summarising in terms of plant type

plant_type <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(category != "GC") %>% # exclude GCs
  distinct(species, plantType) %>% 
  group_by(plantType) %>%
  summarise(frequency = n())

# trait completeness

# entities with the 5 minimum traits (without the GCs)

species <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(category != "GC") %>%
  distinct(species)
# 2205 entities

traits <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(category != "GC") %>%
  distinct(species, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2205)*100)

write.csv(traits, "Master_database_output/traitcompleteness_October2020.csv", row.names = FALSE)

### work for Malin

# extract the GCs with the % min traits

GCs <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(category == "GC") %>%
  distinct(species) %>%
  select(species) %>%
  arrange(species)

write.csv(GCs, "Master_database_output/Malin/5_min_traits_GCs.csv", row.names = FALSE)

# extract species with the 5 min traits that don't have placement and usage
species <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(category != "GC") %>%
  distinct(species)

# placement
placement <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(category != "GC") %>%
  distinct(species, trait_name) %>%
  filter(trait_name == "placement") %>%
  select(species)

missing_placement <- anti_join(species, placement) # 143 species

# usage
usage <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(category != "GC") %>%
  distinct(species, trait_name) %>%
  filter(trait_name == "usage") %>%
  select(species)

missing_usage <- anti_join(species, usage) # 115 species

# join the two lists together

placementandusage <- rbind(missing_placement, missing_usage)

placementandusage <- distinct(placementandusage, species)

# extract info of those species from the database

placmentandusagespecies <- left_join(placementandusage, everything, by = "species")

placementandusagespecies <- select(placmentandusagespecies, master, scientificNameStd,
                                   gl, tr, species, plantType, origin, Min_5_traits,
                                   Min_8_traits, category, newspecies, species_tr,
                                   List_source, date_sourced, verification, species_number,
                                   multiple_forms, source, trait_index, trait_name_original,
                                   trait_name, value_original, value)

write.csv(placementandusagespecies, "Master_database_output/Malin/placementandusage_missing.csv", row.names = FALSE)

####### Adding Malin's collected placement and usage data to the database

library(tidyverse)

everything <- read.csv("Master_database_input/EVERYTHING_traits_7Sept.csv")

malin <- read.csv("Master_database_output/Malin/placementandusage_missing_Malin_complete.csv")

# filter for the data that Malin collected

malin <- malin %>%
  filter(verification == "manual_collection_Malin")
  
# join the two lists together

all <- rbind(everything, malin)

all <- all %>%
  arrange(master, trait_index)

write_csv(all, "Master_database_output/Malin/placementandusage_Malin_add.csv")

# adding Malin's collected info for the species not found in Plantfile

library(tidyverse)

everything <- read.csv("Master_database_input/EVERYTHING_traits_23Nov.csv")

malin <- read.csv("Master_database_output/Malin/Malin_species_not_in_plantfile_201120.csv")

# filter for the data that Malin collected

malin <- malin %>%
  filter(verification == "manual_collection_Malin")

# join the two lists together

all <- rbind(everything, malin)

all <- all %>%
  arrange(master, trait_index)

# remove duplicates from Malin

test <- distinct(all, master, scientificNameStd, gl, tr, species, plantType, origin, Min_5_traits, Min_8_traits, category,
                 newspecies, species_tr, List_source, date_sourced, verification, species_number, multiple_forms, 
                 source, trait_index, trait_name_original, trait_name, value_original, value)


# what's the difference

allMalin <- filter(all, verification == "manual_collection_Malin") # this is doubling up
alltest <- filter(test, verification == "manual_collection_Malin")

write_csv(test, "Master_database_output/Malin/EVERYTHING_traits_Malin_add.csv")

# Malin has written the numbers for some traits wrong, I have changed them manually in excel, need to order them

library(tidyverse)

everything <- read.csv("Master_database_input/EVERYTHING_traits_14Dec.csv")

everything <- everything %>%
  arrange(master, trait_index, verification)

write_csv(everything, "Master_database_output/Malin/database_14Dec.csv")

########################################################################################################################################

# overlap with PlantFile source

# Ale said ~850 species overlap

library(tidyverse)

# load Ale's data

ale <- read.csv("Master_database_input/WPWvsPlantFile_spp.csv")

ale <- select(ale, New_binomial)

ale <- rename(ale, scientificNameStd = New_binomial)

# extract trait data for those species

trait_db <- read.csv("Master_database_input/EVERYTHING_traits_24June.csv")

trait_db <- select(trait_db, scientificNameStd, trait_name)

# one entry for each combination of species and trait name

trait_db <- distinct(trait_db, scientificNameStd, trait_name)

# extract the species in Ale's list

traits <- left_join(ale, trait_db, by = "scientificNameStd")

check <- distinct(traits, scientificNameStd) # 840 species, missing 17 species but that's ok for now

traits$value <- rep("1",nrow(traits)) # add a new column populated by "1", essentially replacing the characters with a number

traits <- distinct(traits, scientificNameStd, trait_name, .keep_all = TRUE)

# convert into long format
# https://stackoverflow.com/questions/34684925/how-to-use-the-spread-function-properly-in-tidyr

traits_long <- spread(traits, key = trait_name, value = value, fill = 0)

# subset only the traits in common with plantfile

traits_long <- traits_long %>%
  select(scientificNameStd, common_name, form, bark_texture, leaf_loss, supp_watering, habit_canopy,
         growth_rate, max_height, max_width, foliage_colour, flower_colour, flower_period, 
         fruit_colour, fruit_period, ecological_services, usage, placement, risk, light_level)

str(traits_long) # need to change characters into numeric

traits_long[,2:20] <- sapply(traits_long[,2:20], as.numeric)

# add a total records column
traits_long$total <- rowSums(traits_long[,2:20])

# missing traits
traits_long <- mutate(traits_long, missing = 19 - total)

traits_long <- arrange(traits_long, desc(missing))

traits_long <- mutate(traits_long, percent_complete = (total/19)*100)

traits_long <- filter(traits_long, missing < 19) # for some reason I have joined species that aren't actually in the database

write.csv(traits_long, "Master_database_output/Plantfile/plantfile_database_comparison.csv", row.names = FALSE)

#####################################

# species that aren't overlaped with plantfile

# check how many of the 19 traits we have in the database

library(tidyverse)

# load Ale's data

ale <- read.csv("Master_database_input/WPWvsPlantFile_spp.csv")

ale <- select(ale, New_binomial)

ale <- rename(ale, scientificNameStd = New_binomial)

# extract trait data for those species

trait_db <- read.csv("Master_database_input/EVERYTHING_traits_24June.csv")

trait_db <- select(trait_db, scientificNameStd, trait_name, Min_5_traits)

trait_db <- filter(trait_db, Min_5_traits == "TRUE") # filter for min 5 trait species

# one entry for each combination of species and trait name

trait_db <- distinct(trait_db, scientificNameStd, trait_name)

# extract the species NOT in Ale's list

traits <- anti_join(trait_db, ale, by = "scientificNameStd")

check <- distinct(traits, scientificNameStd) # 868 species

traits$value <- rep("1",nrow(traits)) # add a new column populated by "1", essentially replacing the characters with a number

traits <- distinct(traits, scientificNameStd, trait_name, .keep_all = TRUE)

# convert into long format
# https://stackoverflow.com/questions/34684925/how-to-use-the-spread-function-properly-in-tidyr

traits_long <- spread(traits, key = trait_name, value = value, fill = 0)

# subset only the traits in common with plantfile

traits_long <- traits_long %>%
  select(scientificNameStd, common_name, form, bark_texture, leaf_loss, supp_watering, habit_canopy,
         growth_rate, max_height, max_width, foliage_colour, flower_colour, flower_period, 
         fruit_colour, fruit_period, ecological_services, usage, placement, risk, light_level)

str(traits_long) # need to change characters into numeric

traits_long[,2:20] <- sapply(traits_long[,2:20], as.numeric)

# add a total records column
traits_long$total <- rowSums(traits_long[,2:20])

# missing traits
traits_long <- mutate(traits_long, missing = 19 - total)

traits_long <- arrange(traits_long, desc(missing))

traits_long <- mutate(traits_long, percent_complete = (total/19)*100)

traits_long <- na.omit( traits_long) # get rid of random NA in species

write.csv(traits_long, "Master_database_output/Plantfile/species_not_in_plantfile.csv", row.names = FALSE)

##### extracting these species for Malin so she can look for traits

library(tidyverse)

# load Ale's data

ale <- read.csv("Master_database_input/WPWvsPlantFile_spp.csv")

ale <- select(ale, New_binomial)

ale <- rename(ale, scientificNameStd = New_binomial)

# extract trait data for those species

trait_db <- read.csv("Master_database_input/EVERYTHING_traits_7Sept_Malin_add.csv")

trait_db <- filter(trait_db, Min_5_traits == "TRUE") # filter for min 5 trait species

# extract the species NOT in Ale's list

traits <- anti_join(trait_db, ale, by = "scientificNameStd")

check <- distinct(traits, scientificNameStd) # 915 species

write.csv(traits, "Master_database_output/Malin/Malin_species_not_in_plantfile.csv", row.names = FALSE)

######################################################################################################################
#### Extract species that are missing common name

library(tidyverse)

everything <- read.csv("Master_database_input/EVERYTHING_traits_20Jan2021.csv")

gh <- read.csv("Master_database_input/EVERYTHING_gh_20Jan2021.csv")

# join two lists together

all <- rbind(everything, gh)

# extract species with the 5 min traits that don't have common name
species <- all %>%
  filter(Min_5_traits == "TRUE") %>%
  distinct(species)

name <- all %>%
  filter(Min_5_traits == "TRUE") %>%
  distinct(species, trait_name) %>%
  filter(trait_name == "common_name") %>%
  select(species)

missing_name <- anti_join(species, name) # 208 species

write.csv(missing_name,"Master_database_output/missing_common_name.csv", row.names = FALSE)

#### Extract species that are missing flower colour and flower period

library(tidyverse)

everything <- read.csv("Master_database_input/EVERYTHING_traits_16Feb2021.csv")

gh <- read.csv("Master_database_input/EVERYTHING_gh_16Feb2021.csv")

# join two lists together

all <- rbind(everything, gh)

species <- all %>%
  filter(Min_5_traits == "TRUE") %>%
  distinct(species)

# extract species with the 5 min traits that don't have flower colour
colour <- all %>%
  filter(Min_5_traits == "TRUE") %>%
  distinct(species, trait_name) %>%
  filter(trait_name == "flower_colour") %>%
  select(species)

missing_colour <- anti_join(species, colour) # 108 species

# extract species with the 5 min traits that don't have flower period
period <- all %>%
  filter(Min_5_traits == "TRUE") %>%
  distinct(species, trait_name) %>%
  filter(trait_name == "flower_period") %>%
  select(species)

missing_period <- anti_join(species, period) # 190 species

# join the two lists together

missing_colour_period <- rbind(missing_period, missing_colour)

missing_colour_period <- distinct(missing_colour_period, species) #224 species

write.csv(missing_colour_period,"Master_database_output/missing_colour_period.csv", row.names = FALSE)

###########################################################################################
#### Aerotropolis project

library(tidyverse)

everything <- read.csv("Master_database_input/EVERYTHING_traits_6Nov.csv")

gh <- read.csv("Master_database_input/EVERYTHING_gh_6Nov.csv")

# join both together

whatihave <- bind_rows(everything, gh)

aerotropolis <- read.csv("Master_database_input/Aerotropolis_species.csv") # 226 species

# extract info for the species from trait database

info <- left_join(aerotropolis, whatihave, by = "master") 

# some stats

species <- info %>%
  select(master) %>%
  distinct(master) # they gave us 223 'species'

x <- info %>%
  select(trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) # judging by the number of NAs, we are missing traits for 87 species

# extract the species we have traits for

blah <- info %>%
  select(master, trait_name) %>%
  drop_na(trait_name) %>%
  distinct(master) %>%
  select(master) # have traits for 138 'species'

write.csv(blah, "Master_database_output/Aerotropolis/species_traits_list.csv", row.names = FALSE)

summary <- info %>%
  distinct(master, Min_5_traits) %>%
  group_by(Min_5_traits) %>%
  summarise(frequency = n()) # but only 114 'species' have the 5 minimum traits

write.csv(info, "Master_database_output/Aerotropolis/species_traits.csv", row.names = FALSE)

####################################################################################################################
##### Give Janine copy of database

library(tidyverse)

everything <- read.csv("Master_database_input/EVERYTHING_traits_23Feb2021.csv")

# filter out the 5 min traits species

everything <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  distinct(species, trait_name, value, .keep_all = TRUE) %>%
  select(scientificNameStd, species, plantType, origin, category, trait_name, value)

write.csv(everything,"Master_database_output/Janine/sample_traitdatabase_ST_Feb2021.csv", row.names = FALSE)  

######################################################################################################################
######################################################################################################################
####                                                DATABASE CHECKS                                               ####
######################################################################################################################
######################################################################################################################

# names

library(tidyverse)

everything <- read.csv("Master_database_input/EVERYTHING_traits_1Mar2021.csv")

# filter out the 5 min traits species

sciname <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  select(scientificNameStd) %>%
  drop_na(scientificNameStd) %>%
  distinct(scientificNameStd) # 1805 scientific names

species <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  select(species) %>%
  drop_na(species) %>%
  distinct(species) # 2378 species names

summary <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  distinct(species, .keep_all = TRUE) %>%
  group_by(category) %>%
  summarise(frequency = n())

# from above, 1653 SP + 162 SYN = 1815 scientific names

# Where is the mismatch?
# some SYN and SP might both have 5 min traits

# Species with multiple synonyms that have 5 min traits: Abelia uniflora, Myoporum tenuifolium, Pittosporum phillyraeoides

# Species and synonyms that have 5 min traits: Bauhinia variegata, Coronidium scorpioides,
# Cupressus arizonica, Eucalyptus leucoxylon, Ficinia nodosa, Melaleuca fulgens,
# Syringa vulgaris, Syzygium tierneyanum, Virgilia oroboides

# anyway, work with the 1805 species

check <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  distinct(scientificNameStd, .keep_all = TRUE) %>%
  drop_na(scientificNameStd) %>%
  group_by(category) %>%
  summarise(frequency = n())

names <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  distinct(scientificNameStd, .keep_all = TRUE) %>%
  drop_na(scientificNameStd) %>%
  select(scientificNameStd, category)

# culvars that do not have 5mintraits for species

# Species not sure
# Acacia cognata
# Anigozanthos rufus
# Brachyscome microcarpa
# Chamaelaucium uncinatum
# Eremophila racemosa
# Leptospermum nitidum
# Leptospermum petersonii
# Leptospermum polygalifolium
# Lomandra glauca
# Ehrharta stipoides (has a synonym)
# Themeda triandra (has a synonym)

# but in this list these problem species are fine!
names2 <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  distinct(scientificNameStd, category, .keep_all = TRUE) %>%
  drop_na(scientificNameStd) %>%
  select(scientificNameStd, category)

# check if eveything has the 10 min traits

tenmintraits <- everything %>%
  select(scientificNameStd, species, Min_5_traits, trait_name, value) %>%
  filter(trait_name == "common_name" | trait_name == "light_level" | trait_name == "form" | trait_name == "max_height" 
         | trait_name == "max_width" | trait_name == "leaf_loss" | trait_name == "flower_colour" | trait_name == "flower_period" 
         | trait_name == "placement" | trait_name == "usage")

tenmintraits <- distinct(tenmintraits, scientificNameStd, species, Min_5_traits, trait_name) # one row for each entity and trait

tenmintraits["Score"] <- 1 # add new column populated by '1'

glimpse(tenmintraits)

summary <- tenmintraits %>%
  group_by(scientificNameStd, species, Min_5_traits) %>%
  mutate(Sum = sum(Score))

# check

mistakes <- filter(summary, Min_5_traits == "TRUE" & Sum < 10)

mistakes2 <- filter(summary, Min_5_traits != "TRUE" & Sum == "10")

# remove solely indoor/container plants

all <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>% 
  select(species, trait_name, value) %>%
  filter (trait_name == "placement") %>%
  distinct(species, trait_name, value)

all["Score"] <- 1 # add new column populated by '1'

glimpse(all)

summary_all <- all %>%
  group_by(species) %>%
  mutate(Sum = sum(Score)) %>%
  select(species, Sum) %>%
  distinct(species, Sum)

indoor <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  select(species, trait_name, value) %>%
  filter (trait_name == "placement") %>%
  filter(value == "container" | value == "indoor") %>%
  distinct(species, trait_name, value)

indoor["Score"] <- 1 # add new column populated by '1'

glimpse(indoor)

summary_indoor <- indoor %>%
  group_by(species) %>%
  mutate(Sum = sum(Score)) %>%
  select(species, Sum) %>%
  distinct(species, Sum)

# find similarities in the two tables in terms of species and sum

same <- semi_join(summary_indoor, summary_all)


###### Check the glasshouse species

library(tidyverse)

everything_gh <- read.csv("Master_database_input/EVERYTHING_gh_1Mar2021.csv")

# filter out the 5 min traits species

sciname_gh <- everything_gh %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  select(scientificNameStd) %>%
  drop_na(scientificNameStd) %>%
  distinct(scientificNameStd) # 110 scientific names

species_gh <- everything_gh %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  select(species) %>%
  drop_na(species) %>%
  distinct(species) # 262 species names

summary_gh <- everything_gh %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  distinct(species, .keep_all = TRUE) %>%
  group_by(category) %>%
  summarise(frequency = n())

# from above, 105 SP + 7 SYN = 112 scientific names

# Where is the mismatch?
# some SYN and SP might both have 5 min traits

# anyway, work with the 110 species

check_gh <- everything_gh %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  distinct(scientificNameStd, .keep_all = TRUE) %>%
  drop_na(scientificNameStd) %>%
  group_by(category) %>%
  summarise(frequency = n())

names_gh <- everything_gh %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  distinct(scientificNameStd, .keep_all = TRUE) %>%
  drop_na(scientificNameStd) %>%
  select(scientificNameStd, category)

# culvars that do not have 5mintraits for species

# Syzygium smithii

# but in this list these problem species are fine!
names2_gh <- everything_gh %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  distinct(scientificNameStd, category, .keep_all = TRUE) %>%
  drop_na(scientificNameStd) %>%
  select(scientificNameStd, category)

# check if eveything has the 10 min traits

tenmintraits_gh <- everything_gh %>%
  select(scientificNameStd, species, Min_5_traits, trait_name, value) %>%
  filter(trait_name == "common_name" | trait_name == "light_level" | trait_name == "form" | trait_name == "max_height" 
         | trait_name == "max_width" | trait_name == "leaf_loss" | trait_name == "flower_colour" | trait_name == "flower_period" 
         | trait_name == "placement" | trait_name == "usage")

tenmintraits_gh <- distinct(tenmintraits_gh, scientificNameStd, species, Min_5_traits, trait_name) # one row for each entity and trait

tenmintraits_gh["Score"] <- 1 # add new column populated by '1'

glimpse(tenmintraits)

summary_gh <- tenmintraits_gh %>%
  group_by(scientificNameStd, species, Min_5_traits) %>%
  mutate(Sum = sum(Score))

# check

mistakes <- filter(summary_gh, Min_5_traits == "TRUE" & Sum < 10)

mistakes2 <- filter(summary_gh, Min_5_traits != "TRUE" & Sum == "10")

# remove solely indoor/container plants

all_gh <- everything_gh %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>% 
  select(species, trait_name, value) %>%
  filter (trait_name == "placement") %>%
  distinct(species, trait_name, value)

all_gh["Score"] <- 1 # add new column populated by '1'

glimpse(all_gh)

summary_all_gh <- all_gh %>%
  group_by(species) %>%
  mutate(Sum = sum(Score)) %>%
  select(species, Sum) %>%
  distinct(species, Sum)

indoor_gh <- everything_gh %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  select(species, trait_name, value) %>%
  filter (trait_name == "placement") %>%
  filter(value == "container" | value == "indoor") %>%
  distinct(species, trait_name, value)

indoor_gh["Score"] <- 1 # add new column populated by '1'

glimpse(indoor_gh)

summary_indoor_gh <- indoor_gh %>%
  group_by(species) %>%
  mutate(Sum = sum(Score)) %>%
  select(species, Sum) %>%
  distinct(species, Sum)

# find similarities in the two tables in terms of species and sum

same_gh <- semi_join(summary_indoor_gh, summary_all_gh)

############## join the 'all' traits and 'glasshouse' traits together

library(tidyverse)

everything <- read.csv("Master_database_input/EVERYTHING_traits_1Mar2021.csv")

everything_gh <- read.csv("Master_database_input/EVERYTHING_gh_1Mar2021.csv")

all_entities <- bind_rows(everything, everything_gh)

#### Extract a list of the scientificNameStd for Farzin to check against models

Farzin_list <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  distinct(scientificNameStd) %>%
  drop_na(scientificNameStd) %>%
  arrange(scientificNameStd) # 1914 species

write.csv(Farzin_list, "Master_database_output/Farzin/species_list_ST_1Mar2021.csv", row.names = FALSE)

##### Extract a list of the scientificNameStd, species, plantType for Claire to find photos

Claire_list <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  distinct(scientificNameStd, species, category) %>%
  arrange(scientificNameStd) # 2640 species

write.csv(Claire_list, "Master_database_output/Claire/species_list_ST_1Mar2021_photos.csv", row.names = FALSE)

# from before
everything <- read.csv("Master_database_input/EVERYTHING_traits_1Mar2021.csv")

species <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  select(species) %>%
  drop_na(species) %>%
  distinct(species) # 2378 species names

everything_gh <- read.csv("Master_database_input/EVERYTHING_gh_1Mar2021.csv")

species_gh <- everything_gh %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  select(species) %>%
  drop_na(species) %>%
  distinct(species) # 262 species names

# 2378 + 262 = 2640, yay!!!!

summary_Claire <- Claire_list %>%
  group_by(category) %>%
  summarise(frequency = n())

summary <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  distinct(species, .keep_all = TRUE) %>%
  group_by(category) %>%
  summarise(frequency = n())

summary_gh <- everything_gh %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  distinct(species, .keep_all = TRUE) %>%
  group_by(category) %>%
  summarise(frequency = n())

# have 5 extra species when I add together

Claire_list_SP <- Claire_list %>%
  filter(category == "SP") %>%
  select(scientificNameStd, species) %>%
  distinct(scientificNameStd, species)

everything_SP <- everything %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  filter(category == "SP") %>%
  select(scientificNameStd, species) %>%
  distinct(scientificNameStd, species)

everything_gh_SP <- everything_gh %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  filter(category == "SP") %>%
  select(scientificNameStd, species) %>%
  distinct(scientificNameStd, species)

all_SP <- bind_rows(everything_SP, everything_gh_SP)

all_SP_dist <- distinct(all_SP)

all_SP_count <- all_SP %>%
  group_by(scientificNameStd, species) %>%
  summarise(frequency = n()) # 1758 species

all_SP_count_sum <- select(all_SP_count, species)

all_SP_count_2 <- all_SP %>%
  group_by(species) %>%
  summarise(frequency = n()) # 1753 species

all_SP_count_2 <- select(all_SP_count, species)

# what's the diff?
x <- all_SP_count_sum$scientificNameStd == all_SP_count_sum$species

x <- as.data.frame(x, stringsAsFactors = FALSE) # some scientific names did not match species names!

# what is the difference between Claire and Farzin's list in terms of scinames?

library(tidyverse)

Farzin <- read.csv("Master_database_output/Farzin/species_list_ST_1Mar2021.csv") # 1914 species

Claire <- read.csv("Master_database_output/Claire/species_list_ST_1Mar2021_photos.csv")

Claire <- Claire %>%
  filter(category == "SP" | category == "SYN") %>%
  select(scientificNameStd) # 1927 species

Claire_sum <- Claire %>%
  group_by(scientificNameStd) %>%
  summarise(frequency = n())

# identified the below synonyms that were already identified as problematic

# Species with multiple synonyms that have 5 min traits: Abelia uniflora, Myoporum tenuifolium, Pittosporum phillyraeoides

# Species and synonyms that have 5 min traits: Bauhinia variegata, Coronidium scorpioides,
# Cupressus arizonica, Eucalyptus leucoxylon, Ficinia nodosa, Melaleuca fulgens,
# Syringa vulgaris, Syzygium tierneyanum, Virgilia oroboides

diff <- anti_join(Claire, Farzin, by = "scientificNameStd")

##### BETULA PLATYPHYLLA NOT INCLUDED IN FARZIN LIST FOR SOME REASON ##################
##### HAVE CHANGED BETULA PLATYPHYLLA AND ITS 2 CULVARS TO 'NO' IN THE 'INCLUDE_IN_TOOL' COLUMN ############

# check that the hybrid and GC parents are on this Farzin's list

parents <- read.csv("Master_database_input/hybrids_genus_cultivars_parents.csv")

glimpse(parents)

parents_long <- parents %>%
  gather(parent_type, parent, Parent_1:Parent_4)

new_species <- parents_long %>%
  select(parent) %>%
  distinct(parent)

# remove blanks

new_species <- new_species[new_species$parent!="",]
new_species <- as.data.frame(new_species, stringsAsFactors = FALSE)
glimpse(new_species)

# see if these species are included in longer list

colnames(new_species) <- "scientificNameStd"

blah <- anti_join(new_species, Farzin_list, by = "scientificNameStd") # 2 obs, but when I look on Farzin's list they are included


############## check coverage of the traits

library(tidyverse)

everything <- read.csv("Master_database_input/EVERYTHING_traits_1Mar2021.csv")

everything_gh <- read.csv("Master_database_input/EVERYTHING_gh_1Mar2021.csv")

all_entities <- bind_rows(everything, everything_gh)

# trait completeness

# entities with the 5 minimum traits to be included in the database

species <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  distinct(species)
# 2640 entities, same as in Claire's list!

traits <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  distinct(species, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2640)*100)
# all 10traits at 100% completion!!!

write.csv(traits, "Master_database_output/traitcompleteness_March2021.csv", row.names = FALSE)

######## drought tolerance trait coverage

library(tidyverse)

everything <- read.csv("Master_database_input/EVERYTHING_traits_1Mar2021.csv")

everything_gh <- read.csv("Master_database_input/EVERYTHING_gh_1Mar2021.csv")

all_entities <- bind_rows(everything, everything_gh)

drought <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  filter(trait_name == "drought_tolerance") %>%
  select(scientificNameStd, species, trait_name, value)

drought_summary <- drought %>%
  group_by(scientificNameStd, species) %>%
  summarise(number_records = n())
# 1744 records, same as trait completeness!

drought["number"] <- 1 # add new column populated by '1'

drought_summary <- drought %>%
  group_by(scientificNameStd, species, value) %>%
  summarise(number_records = sum(number))

drought_long <- drought_summary %>%
  spread(key = value, value = number_records, fill = 0) %>%
  mutate(total_records = sum(No, Yes)) %>%
  arrange(desc(total_records)) %>%
  group_by(total_records) %>%
  mutate(number_species = n())

write.csv(drought_long, "Master_database_output/drought_summary/drought_summary_ST_March2021.csv", row.names = FALSE)

# run scientific names through Taxonstand to get Family names

library(Taxonstand)

list <- read.csv("Master_database_output/Farzin/species_list_ST_1Mar2021.csv")

sciname_TLPed <- Taxonstand::TPL(list$scientificNameStd, infra = TRUE, corr = TRUE, repeats = 100)
sciname_TLPed


write.csv(sciname_TLPed,"Master_database_output/taxonomy_checks/taxonstandcheck_ST_1.3.2021.csv", row.names = FALSE)

library(taxize)#############################################################################################################################
##### This runs the taxonomic check with taxize and gnr_resolve to produce the taxonomically corrected species list
##### This function might take a while as TPL has to access a server. In case you get an error (e.g., Service Unavailable) you can batch this process.
trees2check_GNRed<- gnr_resolve(as.character(trees2check$ORIGINAL_name), http="post")   # this takes 3' on my computer

# from this tibble you have to extract the matched_name field and reassign correct binomials to each of the original records.
# you might want to drop the Authors' names from the binomial for simplicity during the analyses. As before, you can reassign it at the end on final tables and thesis.



# you could use both functions to run taxo checks sequentially and make sure the overall taxonomy is current (See Ossola et al., 2020, GUTI).


#### check how many plants have a lot of unique values for the 'form' trait

library(tidyverse)

everything <- read.csv("Master_database_input/EVERYTHING_traits_3Mar2021.csv")

everything_gh <- read.csv("Master_database_input/EVERYTHING_gh_3Mar2021.csv")

all_entities <- bind_rows(everything, everything_gh)

form <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  filter(trait_name == "form") %>%
  select(species, value) %>%
  distinct(species, value) %>%
  group_by(species) %>%
  summarise(frequency = n()) 
# 2636 species but remember I removed Betula platyphylla and its two cultivars, and also Dianella Silver Streak

# will need to check again once Michelle tells me what to delete/change

#### check that 'plant type' is one of the values specified in the 'form' trait

plant_type <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  select(species, plantType) %>%
  distinct(species, plantType)
# 2637 species, yay!!!!

plant_form <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  filter(trait_name == "form") %>%
  select(species, value) %>%
  distinct(species, value)

names(plant_form)[names(plant_form) == "value"] <- "plantType"

# need to capitalise the first letter
# https://stackoverflow.com/questions/16249570/uppercase-the-first-letter-in-data-frame

str_sub(plant_form$plantType, 1, 1) <- str_sub(plant_form$plantType, 1, 1) %>% str_to_upper()

# difference

diff2 <- setdiff(plant_type, plant_form) # 36 species, fixed now

### capitalise all the words in the 'common name'
# https://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string

# common_name <- all_entities %>%
#  filter(Min_5_traits == "TRUE") %>%
#  filter(Include_in_tool == "Yes") %>%
#  filter(trait_name == "common_name") %>%
#  distinct(species, value, .keep_all = TRUE)

# common_name$value <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", common_name$value, perl=TRUE)

# but want to change on all_entities not subset!

# solution
all_entities$value_new <- all_entities$value # create a new column 'value_new' which is a copy of 'value'
all_entities$value_new <- ifelse(all_entities$trait_name == "common_name", gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", all_entities$value_new, perl=TRUE),
                           all_entities$value_new)


##### height and width data

library(tidyverse)

everything <- read.csv("Master_database_input/EVERYTHING_traits_8Mar2021.csv")

everything_gh <- read.csv("Master_database_input/EVERYTHING_gh_8Mar2021.csv")

all_entities <- bind_rows(everything, everything_gh)

measurements <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  filter(trait_name == "max_height" | trait_name == "height" | trait_name == "min_height" | 
           trait_name == "max_width" | trait_name == "width" | trait_name == "min_width") %>%
  select(scientificNameStd, species, category, exp_tested, plantType, trait_name, value)

measurements$trait_name_new <- measurements$trait_name # create a new variable to trait name

measurements <- select(measurements, scientificNameStd, species, category, exp_tested, plantType, trait_name, trait_name_new, value)

measurements$trait_name_new <- ifelse(measurements$trait_name == "max_height", "height", 
                                     measurements$trait_name_new)

measurements$trait_name_new <- ifelse(measurements$trait_name == "min_height", "height", 
                                      measurements$trait_name_new)

measurements$trait_name_new <- ifelse(measurements$trait_name == "max_width", "width", 
                                      measurements$trait_name_new)

measurements$trait_name_new <- ifelse(measurements$trait_name == "min_width", "width", 
                                      measurements$trait_name_new)


glimpse(measurements)

measurements$value <- as.numeric(as.character(measurements$value))

summary <- measurements %>%
  group_by(scientificNameStd, species, category, exp_tested, plantType, trait_name_new) %>%
  summarise(max = max(value), min = min(value), average = mean(value), range = max - min)

summary$average <- format(round(summary$average, 1), nsmall = 2) # make values one decimal place

glimpse(summary)
summary$average <- as.numeric(as.character(summary$average))

filter(summary, range < 0) # no mistakes

names <- distinct(summary, species) # 2637, am not missing anything!

write.csv(summary, "Master_database_output/final_data/height_width_all_ST_5Mar2021.csv", row.names = FALSE)

# summary graphs

library(ggplot2)

# height

height <- filter(summary, trait_name_new == "height")

max <- ggplot(height, aes(x = plantType, y = max)) +
  geom_boxplot()
max

# find the mistakes
filter(height, plantType == "Grass" , max >= 5)
filter(height, plantType == "Herb", max >= 2)
shrub_change <- filter(height, plantType == "Shrub", max >= 6) # 71 species, takinging too long, might change later
tree_change <- filter(height, plantType == "Tree", max >= 35)

min <- ggplot(height, aes(x = plantType, y = min)) +
  geom_boxplot()
min

# find the mistakes
filter(height, plantType == "Grass" , min >= 5)
filter(height, plantType == "Shrub", min >= 8)
min_trees <- filter(height, plantType == "Tree", min >= 20)

range <- ggplot(height, aes(x = plantType, y = range)) +
  geom_boxplot()
range

# find the mistakes
filter(height, plantType == "Fern", range > 9)
filter(height, plantType == "Tree", range > 30)

# width

width <- filter(summary, trait_name_new == "width")

max <- ggplot(width, aes(x = plantType, y = max)) +
  geom_boxplot()
max

# find the mistakes
filter(width, plantType == "Climber", max > 20)
filter(width, plantType == "Grass", max > 20)
filter(width, plantType == "Herb", max > 20)
filter(width, plantType == "Palm", max > 20)
filter(width, plantType == "Shrub", max > 10)
filter(width, plantType == "Succulent", max > 5)
filter(width, plantType == "Tree", max > 30)

min <- ggplot(width, aes(x = plantType, y = min)) +
  geom_boxplot()
min

# find the mistakes
filter(width, plantType == "Shrub", min > 10)

range <- ggplot(width, aes(x = plantType, y = range)) +
  geom_boxplot()
range

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
###########################                   START BUILDING MASTER TABLE               ##################################
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

library(tidyverse)

everything <- read.csv("Master_database_input/EVERYTHING_traits_8Mar2021.csv")

everything_gh <- read.csv("Master_database_input/EVERYTHING_gh_8Mar2021.csv")

all_entities <- bind_rows(everything, everything_gh)

all_entities_short <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value) %>%
  distinct(scientificNameStd, species, category, exp_tested, trait_name, value)

check <- distinct(all_entities_short, species) # 2636 species, haven't lost anything

# remove the old height and width dimensions

all_entities_short <- all_entities_short %>%
  filter(trait_name != "max_height", trait_name != "height", trait_name != "min_height", 
           trait_name != "max_width", trait_name != "width", trait_name != "min_width")

check <- distinct(all_entities_short, species) # 2636 species, haven't lost anything

# load the max, min and average height and width data

height_width <- read.csv("Master_database_output/final_data/height_width_all_ST_5Mar2021.csv")

# create a new column with range
height_width <- height_width %>%
  mutate(range_new = paste0(min, " - ", max, "m"),
         average_new = paste0(average, "m"))

height_width <- select(height_width, scientificNameStd, species, category, exp_tested, trait_name_new, range_new, average_new)

# change to long format
# http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/

height_width_long <- height_width %>%
  gather(trait_name, value, range_new:average_new)

height_width_long <- height_width_long %>%
  mutate(trait_name_new_new = paste0(trait_name_new, "_", trait_name))

height_width_long <- select(height_width_long, scientificNameStd, species, category, exp_tested, trait_name_new_new, value)  

height_width_long[] <- lapply(height_width_long, gsub, pattern = "height_range_new", replacement = "height_range")
height_width_long[] <- lapply(height_width_long, gsub, pattern = "width_range_new", replacement = "width_range")
height_width_long[] <- lapply(height_width_long, gsub, pattern = "height_average_new", replacement = "height_average")
height_width_long[] <- lapply(height_width_long, gsub, pattern = "width_average_new", replacement = "width_average")

# change the column name
names(height_width_long)[names(height_width_long) == 'trait_name_new_new'] <- 'trait_name'

# join to master dataset

all_entities_short <- bind_rows(all_entities_short, height_width_long)
all_entities_short <- arrange(all_entities_short, scientificNameStd, species, trait_name, value)

check <- distinct(all_entities_short, species) # 2636 species, haven't lost anything

### do drought classifications

drought <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  filter(trait_name == "drought_tolerance") %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value)

drought_summary <- drought %>%
  group_by(scientificNameStd, species) %>%
  summarise(number_records = n())
# 1743 records

drought["number"] <- 1 # add new column populated by '1'

drought_summary <- drought %>%
  group_by(scientificNameStd, species, category, exp_tested, trait_name, value) %>%
  summarise(number_records = sum(number))

drought_long <- drought_summary %>%
  spread(key = value, value = number_records, fill = 0) %>%
  mutate(total_records = sum(No, Yes)) %>%
  arrange(desc(total_records)) %>%
  group_by(total_records) %>%
  mutate(number_species = n())

# create proprotions
drought_long <- drought_long %>%
  mutate(no_proportion = (No/total_records) * 100, 
         yes_protortion = (Yes/total_records) * 100)

# apply the consensus approach
drought_long <- drought_long %>%
  mutate(value = case_when(no_proportion >= 75 ~ "putatively no",
                           yes_protortion >= 75 ~ "putatively high",
                           TRUE ~ "putatively moderate")) %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value)

# remove 'total records'
drought_long <- drought_long[,2:7]

# remove the old drought data

all_entities_short <- all_entities_short %>%
  filter(trait_name != "drought_tolerance")

# join to master dataset
all_entities_short <- bind_rows(all_entities_short, drought_long)
all_entities_short <- arrange(all_entities_short, scientificNameStd, species, trait_name, value)

# do frost classifications

frost <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  filter(trait_name == "frost_tolerance") %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value)

# change light to yes
frost <- frost %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value = if_else(value == "light", "Yes", value))

# make all the values upper case
frost$value <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", frost$value, perl=TRUE)

frost_summary <- frost %>%
  group_by(scientificNameStd, species) %>%
  summarise(number_records = n())
# 1852 records

frost["number"] <- 1 # add new column populated by '1'

frost_summary <- frost %>%
  group_by(scientificNameStd, species, category, exp_tested, trait_name, value) %>%
  summarise(number_records = sum(number))

frost_long <- frost_summary %>%
  spread(key = value, value = number_records, fill = 0) %>%
  mutate(total_records = sum(No, Yes)) %>%
  arrange(desc(total_records)) %>%
  group_by(total_records) %>%
  mutate(number_species = n())

# create proprotions
frost_long <- frost_long %>%
  mutate(no_proportion = (No/total_records) * 100, 
         yes_protortion = (Yes/total_records) * 100)

# apply the consensus approach
frost_long <- frost_long %>%
  mutate(value = case_when(no_proportion >= 75 ~ "putatively no",
                           yes_protortion >= 75 ~ "putatively high",
                           TRUE ~ "putatively moderate")) %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value)

# remove 'total records'
frost_long <- frost_long[,2:7]

# remove the old frost data

all_entities_short <- all_entities_short %>%
  filter(trait_name != "frost_tolerance")

# join to master dataset
all_entities_short <- bind_rows(all_entities_short, frost_long)
all_entities_short <- arrange(all_entities_short, scientificNameStd, species, trait_name, value)

# do coastal tolerance classifications

coastal <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  filter(trait_name == "coastal_tolerance") %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value)

# make all the values upper case
coastal$value <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", coastal$value, perl=TRUE)

coastal_summary <- coastal %>%
  group_by(scientificNameStd, species) %>%
  summarise(number_records = n())
# 874 records

coastal["number"] <- 1 # add new column populated by '1'

coastal_summary <- coastal %>%
  group_by(scientificNameStd, species, category, exp_tested, trait_name, value) %>%
  summarise(number_records = sum(number))

coastal_long <- coastal_summary %>%
  spread(key = value, value = number_records, fill = 0) %>%
  mutate(total_records = sum(No, Yes)) %>%
  arrange(desc(total_records)) %>%
  group_by(total_records) %>%
  mutate(number_species = n())

# create proprotions
coastal_long <- coastal_long %>%
  mutate(no_proportion = (No/total_records) * 100, 
         yes_protortion = (Yes/total_records) * 100)

# apply the consensus approach
coastal_long <- coastal_long %>%
  mutate(value = case_when(no_proportion >= 75 ~ "putatively no",
                           yes_protortion >= 75 ~ "putatively high",
                           TRUE ~ "putatively moderate")) %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value)

# remove 'total records'
coastal_long <- coastal_long[,2:7]

# remove the old coastal data

all_entities_short <- all_entities_short %>%
  filter(trait_name != "coastal_tolerance")

# join to master dataset
all_entities_short <- bind_rows(all_entities_short, coastal_long)
all_entities_short <- arrange(all_entities_short, scientificNameStd, species, trait_name, value)

##### add back plant type and origin

plant_type_origin <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  select(species, plantType, origin) %>%
  distinct(species, plantType, origin) # 2636 plants

# join to main dataset
all_entities_short <- left_join(all_entities_short, plant_type_origin, by = "species")

all_entities_short <- all_entities_short %>%
  select(scientificNameStd, species, plantType, origin, category, exp_tested, trait_name, value) %>%
  filter(trait_name != "native_exotic") # do not remove 'form' as will need that later

###### separate the species and genus names
# https://stackoverflow.com/questions/4350440/split-data-frame-string-column-into-multiple-columns

# first change species col name
names(all_entities_short)[names(all_entities_short) == 'species'] <- 'entity'

all_entities_short <- all_entities_short %>%
  separate(scientificNameStd, c("genus", "species"), " ", remove = FALSE)

# need to fix GC, H and HC

all_entities_short$genus <- ifelse(all_entities_short$category == "H", word(all_entities_short$entity, 1), 
                                    all_entities_short$genus)
all_entities_short$genus <- ifelse(all_entities_short$category == "GC", word(all_entities_short$entity, 1), 
                                   all_entities_short$genus)
all_entities_short$genus <- ifelse(all_entities_short$category == "HC", word(all_entities_short$entity, 1), 
                                   all_entities_short$genus)

#### attach the family names

# sciname <- all_entities_short %>%
#  select(scientificNameStd) %>%
#  drop_na(scientificNameStd) %>%
#  distinct(scientificNameStd) # 1914, matches with Farzin's list!

# run through taxise

# sciname_TLPed <- Taxonstand::TPL(sciname$scientificNameStd, infra = TRUE, corr = TRUE, repeats = 100)
# sciname_TLPed
## nevermind, this is taking ages

## use old list I created which should be the same

family <- read.csv("Master_database_output/taxonomy_checks/taxonstandcheck_ST_1.3.2021.csv")

family <- select(family, Taxon, Family)

names(family)[names(family) == 'Taxon'] <- 'scientificNameStd'
names(family)[names(family) == 'Family'] <- 'family'

# split genus and species
# family <- family %>%
#  separate(scientificNameStd, c("genus", "species"), " ")

# all_entities_short <- left_join(all_entities_short, family, by = "genus") # blows up number of rows, need to return back to normal

# names(all_entities_short)[names(all_entities_short) == 'species.x'] <- 'species'

# all_entities_short <- select(all_entities_short, scientificNameStd, family, genus, species, entity, plantType, origin, category, trait_name, value)

# all_entities_short <- distinct(all_entities_short, scientificNameStd, family, genus, species, entity, plantType, origin, category, trait_name, value)

all_entities_short <- left_join(all_entities_short, family, by = "scientificNameStd")

all_entities_short <- select(all_entities_short, scientificNameStd, family, genus, species, entity, plantType, origin, category, exp_tested, trait_name, value)
# family is blank for H, GC, HC

#### add synonym column

all_entities_short$synonym <- "NA"

all_entities_short$synonym <- ifelse(all_entities_short$category == "SYN", all_entities_short$entity, 
                                   all_entities_short$synonym)

all_entities_short <- select(all_entities_short, scientificNameStd, family, genus, species, entity, synonym, plantType, origin, category, exp_tested, trait_name, value)

####### extract the ecological services data

eco <- all_entities_short %>%
  filter(trait_name == "ecological_services") %>%
  select(entity, trait_name, value)

eco$score <- "1"

# change from long to wide

eco_wide <- eco %>%
  spread(value, score, fill = 0) 

glimpse(eco_wide) 

eco_wide$bird <- as.numeric(as.character(eco_wide$bird))
eco_wide$insect <- as.numeric(as.character(eco_wide$insect))
eco_wide$lizard <- as.numeric(as.character(eco_wide$lizard))
eco_wide$native_mammal <- as.numeric(as.character(eco_wide$native_mammal))
eco_wide$pollinator <- as.numeric(as.character(eco_wide$pollinator))  

eco_wide <- eco_wide %>%
  mutate(ecological_score = bird + insect + lizard + native_mammal + pollinator) %>%
  select(-trait_name)

# join to main database
all_entities_short <- left_join(all_entities_short, eco_wide, by = "entity")

# replace the NAs with 0

all_entities_short$bird[is.na(all_entities_short$bird)] <- 0
all_entities_short$insect[is.na(all_entities_short$insect)] <- 0
all_entities_short$lizard[is.na(all_entities_short$lizard)] <- 0
all_entities_short$native_mammal[is.na(all_entities_short$native_mammal)] <- 0
all_entities_short$pollinator[is.na(all_entities_short$pollinator)] <- 0
all_entities_short$ecological_score[is.na(all_entities_short$ecological_score)] <- 0

# remove the ecological services trait
all_entities_short <- all_entities_short %>%
  filter(trait_name != "ecological_services")

##### extract the plant form data

form <- all_entities_short %>%
  filter(trait_name == "form") %>%
  select(entity, trait_name, value)

# fix up
form <- form %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value = if_else(value == "aquatic", "herb", value),
         value = if_else(value == "bromeliad", "herb", value),
         value = if_else(value == "bulb", "herb", value),
         value = if_else(value == "vegetable", "herb", value),
         value = if_else(value == "orchid", "herb", value),
         value = if_else(value == "strap-leaved", "herb", value))

form <- distinct(form, entity, trait_name, value)

form$score <- "1"

# change from long to wide

form_wide <- form %>%
  spread(value, score, fill = 0) 

form_wide <- select(form_wide , -trait_name)

# join to main database
all_entities_short <- left_join(all_entities_short, form_wide, by = "entity")

# remove the form trait
all_entities_short <- all_entities_short %>%
  filter(trait_name != "form")

# add the dummy columns for the biodiversity benefits and model type

all_entities_short$model_type <- ""
all_entities_short$shade_value <- ""
all_entities_short$shade_index <- ""
all_entities_short$carbon_value <- ""
all_entities_short$carbon_index <-""
# all_entities_short$ecological_index <-""

names(all_entities_short)[names(all_entities_short) == 'ecological_score'] <- 'ecological_value'


# rearrange all the columns
all_entities_short <- select(all_entities_short, scientificNameStd, family, genus, species, entity, synonym, model_type, plantType, climber, 
                             cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, category, exp_tested, trait_name, value,
                             bird, insect, lizard, native_mammal, pollinator, ecological_value, shade_value, shade_index, carbon_value, carbon_index)


###### extract the trait names and values for Michelle

# traits <- all_entities_short %>%
#  select(trait_name, value) %>%
#  filter(trait_name != "height_average", trait_name != "height_range", trait_name != "width_average", trait_name != "width_range", 
#         trait_name != "common_name", trait_name != "soil_volume", trait_name != "max_height_nature") %>%
#  group_by(trait_name, value) %>%
#  summarise(frequency = n())

# write.csv(traits, "Master_database_output/traitfrequency_March2021.csv", row.names = FALSE)

# form <- all_entities_short %>%
#  select(entity, plantType) %>%
#  distinct(entity, plantType) %>%
#  group_by(plantType) %>%
#  summarise(frequency = n())
# all of them added togther is 2636!!!!

#### add the max height and width for shade and carbon values

height_width <- read.csv("Master_database_output/final_data/height_width_all_ST_5Mar2021.csv")

height_width <- select(height_width, -range, -average)

# change from wide to long
# http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/

height_width_long2 <- height_width %>%
  gather(trait_name, value, max:min)

height_width_long2 <- height_width_long2 %>%
  mutate(trait_name_new_new = paste0(trait_name_new, "_", trait_name))

height_width_long2 <- select(height_width_long2, -trait_name, -trait_name_new)

# change from long to wide
height_width_wide <- height_width_long2 %>%
  spread(trait_name_new_new, value)

height_width_wide <- select(height_width_wide, -scientificNameStd, -category, -plantType, -exp_tested)

names(height_width_wide)[names(height_width_wide) == 'species'] <- 'entity'

# join to main dataset
all_entities_short <- left_join(all_entities_short, height_width_wide, by = "entity")

# rearrange all the columns
all_entities_short <- select(all_entities_short, scientificNameStd, family, genus, species, entity, synonym, model_type, plantType, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, category, exp_tested, trait_name, value,
                             bird, insect, lizard, native_mammal, pollinator, ecological_value, height_min, height_max, width_min, width_max, shade_value, shade_index, carbon_value, carbon_index)


##### fix up family names for H, HC, GC

family_fix <- all_entities_short %>%
  filter(category == "H"| category == "HC"| category == "GC") %>%
  select(family, genus, species, entity, category) %>%
  distinct(family, genus, species, entity, category)

genus_family <- all_entities_short %>%
  filter(category == "SP") %>%
  select(family, genus)

# check no genus is in two families
sum <- genus_family %>%
  distinct(family, genus) %>%
  group_by(family, genus) %>%
  summarise(frequency = n()) # don't think so
# but there are missing families for Cephalotaxus and Chenopodium

# https://stackoverflow.com/questions/51398814/r-if-else-with-multiple-conditions-for-character-vectors-with-nas
### THIS WILL BE IMPORTANT WHEN CHANGING TRAIT LEVELS!!!!

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(family = if_else(genus == "Cephalotaxus", "Taxaceae", family))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(family = if_else(genus == "Chenopodium", "Amaranthaceae", family))

# check if fixed
genus_family <- all_entities_short %>%
  filter(category == "SP") %>%
  select(family, genus)

sum <- genus_family %>%
  distinct(family, genus) %>%
  group_by(family, genus) %>%
  summarise(frequency = n()) # fixed!

# filter out H, HC and GC from database
missing <- all_entities_short %>%
  filter(category == "H"| category == "HC"| category == "GC") %>%
  select(-family)

found <- left_join(missing, sum, by = "genus")

# some missing families, Michelia, Rheum

found <- found %>%
  mutate_if(is.factor, as.character) %>%
  mutate(family = if_else(genus == "Michelia", "Magnoliaceae", family))

found <- found %>%
  mutate_if(is.factor, as.character) %>%
  mutate(family = if_else(genus == "Rheum", "Polygonaceae", family))

## add parents of Gc, H, HC

parents <- read.csv("Master_database_input/hybrids_genus_cultivars_parents.csv")

parents <- select(parents, -plantType)

names(parents)[names(parents) == 'Species'] <- 'entity'

# join
found <- left_join(found, parents, by = "entity")

# rearrange the columns

found <- found %>%
  select(-frequency) %>%
  select(scientificNameStd, family, genus, species, entity, synonym, Parent_1, Parent_2,Parent_3, Parent_4, model_type, plantType, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, category, exp_tested, trait_name, value,
         bird, insect, lizard, native_mammal, pollinator, ecological_value, height_min, height_max, width_min, width_max, shade_value, shade_index, carbon_value, carbon_index)

# replace the blank parent cells with NA

found <- found %>%
  mutate_if(is.factor, as.character) %>%
  mutate(Parent_2 = if_else(Parent_2 == "", "NA", Parent_2))

found <- found %>%
  mutate_if(is.factor, as.character) %>%
  mutate(Parent_3 = if_else(Parent_3 == "", "NA", Parent_3))

found <- found %>%
  mutate_if(is.factor, as.character) %>%
  mutate(Parent_4 = if_else(Parent_4 == "", "NA", Parent_4))

# remove H, HC, Gc from database

all_entities_short <- all_entities_short %>%
  filter(category != "H" & category != "GC" & category != "HC")

# add parent columns to database

all_entities_short$Parent_1 <- "NA"
all_entities_short$Parent_2 <- "NA"
all_entities_short$Parent_3 <- "NA"
all_entities_short$Parent_4 <- "NA"

# rearrange columns

all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, entity, synonym, Parent_1, Parent_2,Parent_3, Parent_4, model_type, plantType, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, category, exp_tested, trait_name, value,
         bird, insect, lizard, native_mammal, pollinator, ecological_value, height_min, height_max, width_min, width_max, shade_value, shade_index, carbon_value, carbon_index)

# join back the H, HC, GCs

all_entities_short <- bind_rows(all_entities_short, found)

all_entities_short <- arrange(all_entities_short, entity, trait_name, value)

# some synonyms with missing family
syn <- all_entities_short %>%
  filter(family == "") %>%
  select(entity) %>%
  distinct(entity) #17

# find and fix

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(family = if_else(entity == "Abutilon megapotamicum", "Malvaceae", family), 
         family = if_else(entity == "Aloe variegata", "Asphodelaceae", family),
         family = if_else(entity == "Anemone hupehensis", "Ranunculaceae", family),
         family = if_else(entity == "Anemone hupehensis japonica", "Ranunculaceae", family),
         family = if_else(entity == "Anemone tomentosa", "Ranunculaceae", family),
         family = if_else(entity == "Babingtonia behrii", "Myrtaceae", family),
         family = if_else(entity == "Baeckea crassifolia", "Myrtaceae", family),
         family = if_else(entity == "Beaufortia sparsa", "Myrtaceae", family),
         family = if_else(entity == "Beaufortia squarrosa", "Myrtaceae", family),
         family = if_else(entity == "Calothamnus quadrifidus", "Myrtaceae", family),
         family = if_else(entity == "Calothamnus sanguineus", "Myrtaceae", family),
         family = if_else(entity == "Calothamnus villosus", "Myrtaceae", family),
         family = if_else(entity == "Eremaea beaufortioides", "Myrtaceae", family),
         family = if_else(entity == "Jasminum fruticans", "Oleaceae", family),
         family = if_else(entity == "Leucopogon parviflorus", "Ericaceae", family),
         family = if_else(entity == "Quisqualis indica", "Myrtaceae", family),
         family = if_else(entity == "Regelia velutina", "Myrtaceae", family))

# check

syn_check <- filter(all_entities_short, family == "") # all fixed

# filter out cultivars
# check if sciename = entity
# check if they are all synonyms

same <- all_entities_short %>%
  filter(category != "CULVAR") %>%
  filter(scientificNameStd != entity) %>%
  select(entity, category) %>%
  distinct(entity, category)

# fix the species
all_entities_short[] <-lapply(all_entities_short, gsub, pattern = "Magnolia Iiliiflora", replacement = "Magnolia liliiflora")
all_entities_short[] <-lapply(all_entities_short, gsub, pattern = "Pittosporum phylliraeoides", replacement = "Pittosporum phillyraeoides")

# remove the category 'synonym'

# these are the problemativ ones
# Species with multiple synonyms that have 5 min traits: Abelia uniflora, Myoporum tenuifolium

# Species and synonyms that have 5 min traits: Bauhinia variegata, Coronidium scorpioides,
# Cupressus arizonica, Eucalyptus leucoxylon, Ficinia nodosa, Melaleuca fulgens,
# Syringa vulgaris, Syzygium tierneyanum, Virgilia oroboides, Pittosporum phillyraeoides

# summary of what we have so far

summary_original <- all_entities_short %>%
  select(entity, category) %>%
  distinct(entity, category) %>%
  group_by(category) %>%
  summarise(frequency = n())

# exclude the problematic ones

syn_good <- all_entities_short %>%
  filter(scientificNameStd != "Abelia uniflora" & scientificNameStd != "Myoporum tenuifolium" & scientificNameStd != "Pittosporum phillyraeoides" 
         & scientificNameStd != "Bauhinia variegata" & scientificNameStd != "Coronidium scorpioides" & scientificNameStd != "Cupressus arizonica" 
         & scientificNameStd != "Eucalyptus leucoxylon" & scientificNameStd != "Ficinia nodosa" & scientificNameStd != "Melaleuca fulgens" 
         & scientificNameStd != "Syringa vulgaris" & scientificNameStd != "Syzygium tierneyanum" & scientificNameStd != "Virgilia oroboides")

# add back the GC, H, HC
others <- all_entities_short %>%
  filter(category == "H" | category == "HC" | category == "GC")

syn_good <- bind_rows(syn_good, others)

# filter out just the synonyms
syn_good_change <- filter(syn_good, category == "SYN")

# get rid of these from syn good
syn_good <- filter(syn_good, category == "CULVAR" | category == "GC" | category == "H" | category == "HC" | category == "SP" | category == "SSP")

# make the entity and scinames the same
syn_good_change$entity <- syn_good_change$scientificNameStd

# change category into species
syn_good_change$category <- "SP"

# species with multiple synonyms and 5 min traits
# Abelia uniflora, Myoporum tenuifolium
multi_syn <- all_entities_short %>%
  filter(scientificNameStd == "Abelia uniflora" | scientificNameStd == "Myoporum tenuifolium")

multi_syn <- multi_syn %>%
  mutate_if(is.factor, as.character) %>%
  mutate(synonym = if_else(entity == "Abelia engleriana", "Abelia engleriana, Abelia schumannii", synonym),
         synonym = if_else(entity == "Myoporum acuminatum", "Myoporum acuminatum, Myoporum montanum", synonym))

multi_syn <- multi_syn %>%
  filter(entity != "Abelia schumannii" & entity != "Myoporum montanum")

# make sciname and entity name the same
multi_syn$entity <- multi_syn$scientificNameStd
# make syn into sp
multi_syn$category <- "SP"

# species with 5 min traits for species and synonyms
sp_syn <- all_entities_short %>%
  filter(scientificNameStd == "Pittosporum phillyraeoides" | scientificNameStd == "Bauhinia variegata" | scientificNameStd == "Coronidium scorpioides" | scientificNameStd == "Cupressus arizonica" 
         | scientificNameStd == "Eucalyptus leucoxylon" | scientificNameStd == "Ficinia nodosa" | scientificNameStd == "Melaleuca fulgens" 
         | scientificNameStd == "Syringa vulgaris" | scientificNameStd == "Syzygium tierneyanum" | scientificNameStd == "Virgilia oroboides")

# salvage the cultivars
sp_syn_cultivars <- filter(sp_syn, category == "CULVAR")

# remove the cultivars and species
sp_syn <- filter(sp_syn, category != "CULVAR" & category != "SP")

sp_syn$entity <- sp_syn$scientificNameStd
sp_syn$category <- "SP"

# join everything back together
all_entities_short_check <- bind_rows(syn_good, syn_good_change, multi_syn, sp_syn, sp_syn_cultivars)

# check everything is still there
summary_new <- all_entities_short_check %>%
  select(entity, category) %>%
  distinct(entity, category) %>%
  group_by(category) %>%
  summarise(frequency = n()) # seems to all be there

all_entities_short <- all_entities_short_check
all_entities_short <- arrange(all_entities_short, entity, trait_name, value)

check_syn <- all_entities_short %>%
  filter(synonym != "NA") %>%
  distinct(entity) # 166 synonyms which is 168 from original minus the two species I combined

# change entity to plant name
names(all_entities_short)[names(all_entities_short) == 'entity'] <- 'plant_name'

##### remove medicinal and apiary fron usage

# first check coverage of traits

species <- all_entities_short %>%
  distinct(plant_name)
# 2624 plant names (2637 - 13 problematic synonyms = 2624!!!)

traits <- all_entities_short %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2624)*100) # all relevant traits still 100%

# remove medicinal from usage

all_entities_short <- all_entities_short %>%
  filter(value != "medicinal")

# check coverage again

traits <- all_entities_short %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2624)*100) # usage still 100%

# check apiary

no_apiary <- all_entities_short %>%
  filter(value != "apiary")

# check coverage again

traits <- no_apiary %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2624)*100) # usage still 100%

# if apiary then add '1' to pollinator

all_entities_short$pollinator <- ifelse(all_entities_short$value == "apiary", 1, 
                                     all_entities_short$pollinator)

# check
check_pollinator <- all_entities_short %>%
  filter(value == "apiary") %>%
  select(plant_name, pollinator, value) # all accounted for

# remove apiary from dataset as a usage
all_entities_short <- all_entities_short %>%
  filter(value != "apiary")

# rearrange columns to make more sense
all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, plantType, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, value,
         bird, insect, lizard, native_mammal, pollinator, ecological_value, height_min, height_max, width_min, width_max, shade_value, shade_index, carbon_value, carbon_index)

# need to calculate a shade value
all_entities_short$shade_value <- "NA"

all_entities_short$width_max <- as.numeric(as.character(all_entities_short$width_max))

all_entities_short$shade_value <- ifelse(all_entities_short$plantType == "Tree", pi*all_entities_short$width_max^2,
                                 all_entities_short$shade_value)

# check playground friendly
# harm <- all_entities_short %>%
#  filter(value == "playgroundfriendly")

# bad <- harm %>%
#  filter(trait_name == "risk") # no playground friendly plants have a risk!!!!

############

# FINAL FORMAT OF THE DATABASE

# first check glasshouse species

gh <- all_entities_short %>%
  distinct(plant_name, exp_tested) %>%
  group_by(exp_tested) %>%
  summarise(frequency = n())
# 111 plants from glasshouse, some may be missing...
# remove old drought classifications and add new ones....

# add a Koppen zone column

all_entities_short$Koppen_zone <- ""

all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, Koppen_zone, plantType, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, value,
         bird, insect, lizard, native_mammal, pollinator, ecological_value, height_min, height_max, width_min, width_max, shade_value, shade_index, carbon_value, carbon_index)

# change the column names

names(all_entities_short)[names(all_entities_short) == 'plantType'] <- 'growth_form'

# select the traits that we want

all_entities_short <- all_entities_short %>%
  filter(trait_name == "common_name" | trait_name == "flower_colour" | trait_name == "flower_period" | trait_name == "leaf_loss" 
         | trait_name == "light_level" | trait_name == "placement" | trait_name == "usage" | trait_name == "height_average" 
         | trait_name == "width_average" | trait_name == "height_range" | trait_name == "width_range" 
         | trait_name == "soil_type" | trait_name == "soil_pH" | trait_name == "supp_watering" | trait_name == "ideal_conditions" | trait_name == "frost_tolerance" | trait_name == "drought_tolerance" 
         | trait_name == "coastal_tolerance" | trait_name == "habit_canopy" | trait_name == "growth_rate" | trait_name == "foliage_colour" | trait_name == "risk" 
         | trait_name == "weed_status")

# check they are all there
trait_name_check <- all_entities_short %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n())

# create new columns to change the trait_name and value
all_entities_short$trait_name_new <- all_entities_short$trait_name
all_entities_short$value_new <- all_entities_short$value

# rearrange columns to make more sense
all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, Koppen_zone, growth_form, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, trait_name_new, value,
         value_new, bird, insect, lizard, native_mammal, pollinator, ecological_value, height_min, height_max, width_min, width_max, shade_value, shade_index, carbon_value, carbon_index)

# COMMON NAME
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "common_name", "common name", trait_name_new))

# make common names capitalised
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(trait_name == "common_name", gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", all_entities_short$value_new, perl=TRUE),
                             all_entities_short$value_new))

# GROWTH FORM
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(growth_form = if_else(growth_form == "Bromeliad", "Shrub", growth_form),
         growth_form = if_else(growth_form == "Bulb", "Herb", growth_form),
         growth_form = if_else(growth_form == "Orchid", "Herb", growth_form),
         growth_form = if_else(growth_form == "Vegetable", "Herb", growth_form),
         growth_form = if_else(growth_form == "Strap-leaved", "Herb", growth_form))

# FLOWER COLOUR
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "flower_colour", "flower colour", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "gold", "yellow", value_new),
         value_new = if_else(value == "golden", "yellow", value_new),
         value_new = if_else(value == "grey", "black", value_new), 
         value_new = if_else(value == "insignificant", "inconspicuous", value_new),
         value_new = if_else(value == "magenta", "pink", value_new),
         value_new = if_else(value == "mauve", "purple", value_new),
         value_new = if_else(value == "not_applicable", "does not flower", value_new),
         value_new = if_else(value == "violet", "purple", value_new))

# FLOWER PERIOD
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "flower_period", "flower period", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "Any_time", "any time", value_new),
         value_new = if_else(value == "not_applicable", "does not flower", value_new))
         
# make the first letters capital
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(trait_name == "flower_period", gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", all_entities_short$value_new, perl=TRUE), value_new))

# PLACEMENT
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "placement", "urban context", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "avenue", "street", value_new),
         value_new = if_else(value == "largegarden", "park", value_new),
         value_new = if_else(value == "powerlines", "under powerlines", value_new))

# USAGE
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "usage", "uses", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "cutflower", "cut flowers", value_new),
         value_new = if_else(value == "cutflowers", "cut flowers", value_new),
         value_new = if_else(value == "cutflowers", "cut flowers", value_new),
         value_new = if_else(value == "edible", "putatively edible", value_new),
         value_new = if_else(value == "erosion", "erosion control", value_new),
         value_new = if_else(value == "featureshrub", "feature", value_new),
         value_new = if_else(value == "featuretree", "feature", value_new),
         value_new = if_else(value == "featuretropical", "feature", value_new),
         value_new = if_else(value == "fire_retardant", "putatively fire retardant", value_new),
         value_new = if_else(value == "groundcover", "ground cover", value_new),
         value_new = if_else(value == "massplanting", "mass planting", value_new),
         value_new = if_else(value == "playgroundfriendly", "playground friendly", value_new),
         value_new = if_else(value == "sensorycolour", "playground friendly", value_new),
         value_new = if_else(value == "sensorytouch", "playground friendly", value_new))

# HEIGHT AND WIDTH
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "height_average", "average height", trait_name_new),
         trait_name_new = if_else(trait_name == "width_average", "average width", trait_name_new),
         trait_name_new = if_else(trait_name == "height_range", "height range", trait_name_new),
         trait_name_new = if_else(trait_name == "width_range", "width range", trait_name_new))


# SOIL
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "soil_type", "soil type", trait_name_new),
         trait_name_new = if_else(trait_name == "soil_pH", "soil pH", trait_name_new))


# PLANTING AND MAINTENANCE
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "supp_watering", "planting and maintenance", trait_name_new),
         trait_name_new = if_else(trait_name == "ideal_conditions", "planting and maintenance", trait_name_new))


all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "high", "high water needs", value_new),
         value_new = if_else(value == "low", "low water needs", value_new),
         value_new = if_else(value == "medium", "medium water needs", value_new),
         value_new = if_else(value == "none", "low water needs", value_new),
         value_new = if_else(value == "fertile", "fertile soil", value_new),
         value_new = if_else(value == "fertile", "fertile soil", value_new),
         value_new = if_else(value == "lateral_space", "lateral space", value_new),
         value_new = if_else(value == "moist", "moist soil", value_new),
         value_new = if_else(value == "poorly_drained", "poorly drained soil", value_new),
         value_new = if_else(value == "sheltered", "protected", value_new),
         value_new = if_else(value == "well_drained", "well drained soil", value_new))


# change back the growth rate medium values
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(trait_name == "growth_rate" & value == "medium", "medium", value_new))

# filter out the rest
remove_maintenance <- all_entities_short %>%
  filter(trait_name == "ideal_conditions" & value == "acidic" | trait_name == "ideal_conditions" & value == "alkaline" | 
           trait_name == "ideal_conditions" & value == "clay_soil" | trait_name == "ideal_conditions" & value == "clay_soils" | 
           trait_name == "ideal_conditions" & value == "dry" | trait_name == "ideal_conditions" & value == "flowers_fullsun" | 
           trait_name == "ideal_conditions" & value == "fullshade" | trait_name == "ideal_conditions" & value == "fullsun" | 
           trait_name == "ideal_conditions" & value == "gravelly_soil" | trait_name == "ideal_conditions" & value == "high" | 
           trait_name == "ideal_conditions" & value == "humid" | trait_name == "ideal_conditions" & value == "large_rootspace" | 
           trait_name == "ideal_conditions" & value == "loam_soil" | trait_name == "ideal_conditions" & value == "loamy_soil" | 
           trait_name == "ideal_conditions" & value == "loamy_soils" | trait_name == "ideal_conditions" & value == "natural_pH" | 
           trait_name == "ideal_conditions" & value == "neutral_pH" | trait_name == "ideal_conditions" & value == "partshade" | 
           trait_name == "ideal_conditions" & value == "sandy_soil" | trait_name == "ideal_conditions" & value == "shaded" | 
           trait_name == "ideal_conditions" & value == "variable")

# remove
all_entities_short <- anti_join(all_entities_short, remove_maintenance)

# LEAF LOSS
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "leaf_loss", "leaf loss", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "semi_deciduous", "semi-deciduous", value_new))

# LIGHT LEVEL
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "light_level", "shade tolerance", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "fullshade", "full shade", value_new),
         value_new = if_else(value == "fullsun", "full sun", value_new),
         value_new = if_else(value == "partshade", "part shade", value_new))


# TOLERANCES
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "frost_tolerance", "frost tolerance", trait_name_new),
         trait_name_new = if_else(trait_name == "drought_tolerance", "drought tolerance", trait_name_new),
         trait_name_new = if_else(trait_name == "drought_tolerance", "drought tolerance", trait_name_new),
         trait_name_new = if_else(trait_name == "coastal_tolerance", "coastal tolerance", trait_name_new))

# GROWTH RATE
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "growth_rate", "growth rate", trait_name_new))

# FOLIAGE COLOUR
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "foliage_colour", "leaf colour", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "bluegreen", "blue-green", value_new),
         value_new = if_else(value == "darkgreen", "green", value_new),
         value_new = if_else(value == "dullgreen", "green", value_new),
         value_new = if_else(value == "glossygreen", "green", value_new),
         value_new = if_else(value == "greygreen", "grey-green", value_new),
         value_new = if_else(value == "lightgreen", "green", value_new),
         value_new = if_else(value == "redpink", "red", value_new),
         value_new = if_else(value == "silver", "silvery", value_new),
         value_new = if_else(value == "silver_foliage", "silvery", value_new),
         value_new = if_else(value == "silvergreen", "silvery", value_new),
         value_new = if_else(value == "silvergrey", "silvery", value_new),
         value_new = if_else(value == "yellowgreen", "yellow", value_new))

# filter out rest
remove_foliage <- all_entities_short %>%
  filter(trait_name == "foliage_colour" & value == "bluegrey" | trait_name == "foliage_colour" & value == "bronze" | 
           trait_name == "foliage_colour" & value == "burgundy" | trait_name == "foliage_colour" & value == "copper" | 
           trait_name == "foliage_colour" & value == "cream" | trait_name == "foliage_colour" & value == "gold" | 
           trait_name == "foliage_colour" & value == "grey" | trait_name == "foliage_colour" & value == "high" |
           trait_name == "foliage_colour" & value == "orange" | trait_name == "foliage_colour" & value == "white")

# remove
all_entities_short <- anti_join(all_entities_short, remove_foliage)

# HABIT CANOPY
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "habit_canopy", "canopy shape", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "columnar", "upright", value_new),
         value_new = if_else(value == "domed", "rounded", value_new),
         value_new = if_else(value == "hedging_possible", "hedging possible", value_new),
         value_new = if_else(value == "narrow", "upright", value_new),
         value_new = if_else(value == "oval", "rounded", value_new),
         value_new = if_else(value == "prostrate", "upright", value_new))

# filter out rest
remove_habit_canopy <- all_entities_short %>%
  filter(trait_name == "habit_canopy" & value == "arborescent" | trait_name == "habit_canopy" & value == "branching" | 
           trait_name == "habit_canopy" & value == "clumping" | trait_name == "habit_canopy" & value == "compact" | 
           trait_name == "habit_canopy" & value == "conical" | trait_name == "habit_canopy" & value == "vase") 

# remove
all_entities_short <- anti_join(all_entities_short, remove_habit_canopy)

# RISK
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "mildallergen", "allergen", value_new),
         value_new = if_else(value == "moderateallergen", "allergen", value_new),
         value_new = if_else(value == "poison", "poisonous or toxic", value_new),
         value_new = if_else(value == "severeallergen", "allergen", value_new))

# filter the rest
remove_risk <- all_entities_short %>%
  filter(trait_name == "risk" & value == "branchdrop" | trait_name == "risk" & value == "disease_prone" | 
           trait_name == "risk" & value == "fruitfall" | trait_name == "risk" & value == "highly_flammable" | 
           trait_name == "risk" & value == "infrastructure_damage" | trait_name == "risk" & value == "litterfall" | 
           trait_name == "risk" & value == "maloderous" | trait_name == "risk" & value == "malodorous" | 
           trait_name == "risk" & value == "parasitic" | trait_name == "risk" & value == "possible_weed" | 
           trait_name == "risk" & value == "sap_fall" | trait_name == "risk" & value == "suckering")
           
# remove
all_entities_short <- anti_join(all_entities_short, remove_risk)

# WEED
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "weed_status", "weed status", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "act", "Australian Capital Territory", value_new),
         value_new = if_else(value == "nsw", "New South Wales", value_new),
         value_new = if_else(value == "nt", "Northern Territory", value_new),
         value_new = if_else(value == "qld", "Queensland", value_new),
         value_new = if_else(value == "sa", "South Australia", value_new),
         value_new = if_else(value == "tas", "Tasmania", value_new),
         value_new = if_else(value == "wa", "Western Australia", value_new))
         
# filter the rest
remove_weed <- all_entities_short %>%
  filter(trait_name == "weed_status" & value == "potentially_act" | trait_name == "weed_status" & value == "potentially_nsw" | 
           trait_name == "weed_status" & value == "potentially_nt" | trait_name == "weed_status" & value == "potentially_qld" | 
           trait_name == "weed_status" & value == "potentially_sa" | trait_name == "weed_status" & value == "potentially_tas" |
           trait_name == "weed_status" & value == "potentially_vic" | trait_name == "weed_status" & value == "potentially_wa")

# remove
all_entities_short <- anti_join(all_entities_short, remove_weed)


# new trait and value columns
all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, Koppen_zone, growth_form, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name_new, value_new,
           bird, insect, lizard, native_mammal, pollinator, ecological_value, height_min, height_max, width_min, width_max, shade_value, shade_index, carbon_value, carbon_index)

# end need to distinct everything
all_entities_short <- all_entities_short %>%
  distinct(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, Koppen_zone, growth_form, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name_new,
           value_new, bird, insect, lizard, native_mammal, pollinator, ecological_value, height_min, height_max, width_min, width_max, shade_value, shade_index, carbon_value, carbon_index)


names(all_entities_short)[names(all_entities_short) == 'trait_name_new'] <- 'trait_name'
names(all_entities_short)[names(all_entities_short) == 'value_new'] <- 'value'

write.csv(all_entities_short,"Master_database_output/FINAL/trait_database_ST_FINAL_8.3.2021.csv",row.names=FALSE)

##################################################################################################################################

############### Version 1.1
### made height and width into long format
### fixed some traits and values that were errors
### got rid of 'moist' from 'ideal conditions' as it may clash with 'supp watering'

library(tidyverse)

everything <- read.csv("Master_database_input/EVERYTHING_traits_10Mar2021.csv")

everything_gh <- read.csv("Master_database_input/EVERYTHING_gh_8Mar2021.csv")

all_entities <- bind_rows(everything, everything_gh)

all_entities_short <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value) %>%
  distinct(scientificNameStd, species, category, exp_tested, trait_name, value)

check <- distinct(all_entities_short, species) # 2636 species, haven't lost anything

# remove the old height and width dimensions

all_entities_short <- all_entities_short %>%
  filter(trait_name != "max_height", trait_name != "height", trait_name != "min_height", 
         trait_name != "max_width", trait_name != "width", trait_name != "min_width")

check <- distinct(all_entities_short, species) # 2636 species, haven't lost anything

# load the max, min and average height and width data

height_width <- read.csv("Master_database_output/final_data/height_width_all_ST_5Mar2021.csv")

# create a new column with range
# height_width <- height_width %>%
#  mutate(range_new = paste0(min, " - ", max, "m"),
#         average_new = paste0(average, "m"))

 height_width <- select(height_width, -range)

# change to long format
# http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/

height_width_long <- height_width %>%
  gather(trait_name, value, max:average)

height_width_long <- height_width_long %>%
  mutate(trait_name_new_new = paste0(trait_name_new, "_", trait_name))

height_width_long <- select(height_width_long, scientificNameStd, species, category, exp_tested, trait_name_new_new, value)  

# change the column name
names(height_width_long)[names(height_width_long) == 'trait_name_new_new'] <- 'trait_name'

glimpse(height_width_long)
height_width_long$value <- as.character(height_width_long$value)

# join to master dataset

all_entities_short <- bind_rows(all_entities_short, height_width_long)
all_entities_short <- arrange(all_entities_short, scientificNameStd, species, trait_name, value)

check <- distinct(all_entities_short, species) # 2636 species, haven't lost anything

### do drought classifications

drought <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  filter(trait_name == "drought_tolerance") %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value)

drought_summary <- drought %>%
  group_by(scientificNameStd, species) %>%
  summarise(number_records = n())
# 1743 records

drought["number"] <- 1 # add new column populated by '1'

drought_summary <- drought %>%
  group_by(scientificNameStd, species, category, exp_tested, trait_name, value) %>%
  summarise(number_records = sum(number))

drought_long <- drought_summary %>%
  spread(key = value, value = number_records, fill = 0) %>%
  mutate(total_records = sum(No, Yes)) %>%
  arrange(desc(total_records)) %>%
  group_by(total_records) %>%
  mutate(number_species = n())

# create proportions
drought_long <- drought_long %>%
  mutate(no_proportion = (No/total_records) * 100, 
         yes_protortion = (Yes/total_records) * 100)

# apply the consensus approach
drought_long <- drought_long %>%
  mutate(value = case_when(no_proportion >= 75 ~ "putatively no",
                           yes_protortion >= 75 ~ "putatively high",
                           TRUE ~ "putatively moderate")) %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value)

# remove 'total records'
drought_long <- drought_long[,2:7]

# remove the old drought data

all_entities_short <- all_entities_short %>%
  filter(trait_name != "drought_tolerance")

# join to master dataset
all_entities_short <- bind_rows(all_entities_short, drought_long)
all_entities_short <- arrange(all_entities_short, scientificNameStd, species, trait_name, value)

# do frost classifications

frost <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  filter(trait_name == "frost_tolerance") %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value)

# change light to yes
frost <- frost %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value = if_else(value == "light", "Yes", value))

# make all the values upper case
frost$value <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", frost$value, perl=TRUE)

frost_summary <- frost %>%
  group_by(scientificNameStd, species) %>%
  summarise(number_records = n())
# 1852 records

frost["number"] <- 1 # add new column populated by '1'

frost_summary <- frost %>%
  group_by(scientificNameStd, species, category, exp_tested, trait_name, value) %>%
  summarise(number_records = sum(number))

frost_long <- frost_summary %>%
  spread(key = value, value = number_records, fill = 0) %>%
  mutate(total_records = sum(No, Yes)) %>%
  arrange(desc(total_records)) %>%
  group_by(total_records) %>%
  mutate(number_species = n())

# create proportions
frost_long <- frost_long %>%
  mutate(no_proportion = (No/total_records) * 100, 
         yes_protortion = (Yes/total_records) * 100)

# apply the consensus approach
frost_long <- frost_long %>%
  mutate(value = case_when(no_proportion >= 75 ~ "putatively no",
                           yes_protortion >= 75 ~ "putatively high",
                           TRUE ~ "putatively moderate")) %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value)

# remove 'total records'
frost_long <- frost_long[,2:7]

# remove the old frost data

all_entities_short <- all_entities_short %>%
  filter(trait_name != "frost_tolerance")

# join to master dataset
all_entities_short <- bind_rows(all_entities_short, frost_long)
all_entities_short <- arrange(all_entities_short, scientificNameStd, species, trait_name, value)

# do coastal tolerance classifications

coastal <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  filter(trait_name == "coastal_tolerance") %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value)

# make all the values upper case
coastal$value <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", coastal$value, perl=TRUE)

coastal_summary <- coastal %>%
  group_by(scientificNameStd, species) %>%
  summarise(number_records = n())
# 874 records

coastal["number"] <- 1 # add new column populated by '1'

coastal_summary <- coastal %>%
  group_by(scientificNameStd, species, category, exp_tested, trait_name, value) %>%
  summarise(number_records = sum(number))

coastal_long <- coastal_summary %>%
  spread(key = value, value = number_records, fill = 0) %>%
  mutate(total_records = sum(No, Yes)) %>%
  arrange(desc(total_records)) %>%
  group_by(total_records) %>%
  mutate(number_species = n())

# create proportions
coastal_long <- coastal_long %>%
  mutate(no_proportion = (No/total_records) * 100, 
         yes_protortion = (Yes/total_records) * 100)

# apply the consensus approach
coastal_long <- coastal_long %>%
  mutate(value = case_when(no_proportion >= 75 ~ "putatively no",
                           yes_protortion >= 75 ~ "putatively high",
                           TRUE ~ "putatively moderate")) %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value)

# remove 'total records'
coastal_long <- coastal_long[,2:7]

# remove the old coastal data

all_entities_short <- all_entities_short %>%
  filter(trait_name != "coastal_tolerance")

# join to master dataset
all_entities_short <- bind_rows(all_entities_short, coastal_long)
all_entities_short <- arrange(all_entities_short, scientificNameStd, species, trait_name, value)

##### add back plant type and origin

plant_type_origin <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  select(species, plantType, origin) %>%
  distinct(species, plantType, origin) # 2636 plants

# join to main dataset
all_entities_short <- left_join(all_entities_short, plant_type_origin, by = "species")

all_entities_short <- all_entities_short %>%
  select(scientificNameStd, species, plantType, origin, category, exp_tested, trait_name, value) %>%
  filter(trait_name != "native_exotic") # do not remove 'form' as will need that later

###### separate the species and genus names
# https://stackoverflow.com/questions/4350440/split-data-frame-string-column-into-multiple-columns

# first change species col name
names(all_entities_short)[names(all_entities_short) == 'species'] <- 'entity'

all_entities_short <- all_entities_short %>%
  separate(scientificNameStd, c("genus", "species"), " ", remove = FALSE)

# need to fix GC, H and HC

all_entities_short$genus <- ifelse(all_entities_short$category == "H", word(all_entities_short$entity, 1), 
                                   all_entities_short$genus)
all_entities_short$genus <- ifelse(all_entities_short$category == "GC", word(all_entities_short$entity, 1), 
                                   all_entities_short$genus)
all_entities_short$genus <- ifelse(all_entities_short$category == "HC", word(all_entities_short$entity, 1), 
                                   all_entities_short$genus)

#### attach the family names

## use old list I created which should be the same

family <- read.csv("Master_database_output/taxonomy_checks/taxonstandcheck_ST_1.3.2021.csv")

family <- select(family, Taxon, Family)

names(family)[names(family) == 'Taxon'] <- 'scientificNameStd'
names(family)[names(family) == 'Family'] <- 'family'

all_entities_short <- left_join(all_entities_short, family, by = "scientificNameStd")

all_entities_short <- select(all_entities_short, scientificNameStd, family, genus, species, entity, plantType, origin, category, exp_tested, trait_name, value)
# family is blank for H, GC, HC

#### add synonym column

all_entities_short$synonym <- "NA"

all_entities_short$synonym <- ifelse(all_entities_short$category == "SYN", all_entities_short$entity, 
                                     all_entities_short$synonym)

all_entities_short <- select(all_entities_short, scientificNameStd, family, genus, species, entity, synonym, plantType, origin, category, exp_tested, trait_name, value)

####### extract the ecological services data

eco <- all_entities_short %>%
  filter(trait_name == "ecological_services") %>%
  select(entity, trait_name, value)

eco$score <- "1"

# change from long to wide

eco_wide <- eco %>%
  spread(value, score, fill = 0) 

glimpse(eco_wide) 

eco_wide$bird <- as.numeric(as.character(eco_wide$bird))
eco_wide$insect <- as.numeric(as.character(eco_wide$insect))
eco_wide$lizard <- as.numeric(as.character(eco_wide$lizard))
eco_wide$native_mammal <- as.numeric(as.character(eco_wide$native_mammal))
eco_wide$pollinator <- as.numeric(as.character(eco_wide$pollinator))  

eco_wide <- eco_wide %>%
  mutate(ecological_score = bird + insect + lizard + native_mammal + pollinator) %>%
  select(-trait_name)

# join to main database
all_entities_short <- left_join(all_entities_short, eco_wide, by = "entity")

# replace the NAs with 0

all_entities_short$bird[is.na(all_entities_short$bird)] <- 0
all_entities_short$insect[is.na(all_entities_short$insect)] <- 0
all_entities_short$lizard[is.na(all_entities_short$lizard)] <- 0
all_entities_short$native_mammal[is.na(all_entities_short$native_mammal)] <- 0
all_entities_short$pollinator[is.na(all_entities_short$pollinator)] <- 0
all_entities_short$ecological_score[is.na(all_entities_short$ecological_score)] <- 0

# remove the ecological services trait
all_entities_short <- all_entities_short %>%
  filter(trait_name != "ecological_services")

##### extract the plant form data

form <- all_entities_short %>%
  filter(trait_name == "form") %>%
  select(entity, trait_name, value)

# fix up
form <- form %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value = if_else(value == "aquatic", "herb", value),
         value = if_else(value == "bromeliad", "herb", value),
         value = if_else(value == "bulb", "herb", value),
         value = if_else(value == "vegetable", "herb", value),
         value = if_else(value == "orchid", "herb", value),
         value = if_else(value == "strap-leaved", "herb", value))

form <- distinct(form, entity, trait_name, value)

form$score <- "1"

# change from long to wide

form_wide <- form %>%
  spread(value, score, fill = 0) 

form_wide <- select(form_wide , -trait_name)

# join to main database
all_entities_short <- left_join(all_entities_short, form_wide, by = "entity")

# remove the form trait
all_entities_short <- all_entities_short %>%
  filter(trait_name != "form")

# add the dummy columns for the biodiversity benefits and model type

all_entities_short$model_type <- ""
all_entities_short$shade_value <- ""
all_entities_short$shade_index <- ""
all_entities_short$carbon_value <- ""
all_entities_short$carbon_index <-""

names(all_entities_short)[names(all_entities_short) == 'ecological_score'] <- 'ecological_value'

# rearrange all the columns
all_entities_short <- select(all_entities_short, scientificNameStd, family, genus, species, entity, synonym, model_type, plantType, climber, 
                             cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, category, exp_tested, trait_name, value,
                             bird, insect, lizard, native_mammal, pollinator, ecological_value, shade_value, shade_index, carbon_value, carbon_index)

#### add the max height and width for shade and carbon values

height_width <- read.csv("Master_database_output/final_data/height_width_all_ST_5Mar2021.csv")

height_width <- select(height_width, -range, -average)

# change from wide to long
# http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/

height_width_long2 <- height_width %>%
  gather(trait_name, value, max:min)

height_width_long2 <- height_width_long2 %>%
  mutate(trait_name_new_new = paste0(trait_name_new, "_", trait_name))

height_width_long2 <- select(height_width_long2, -trait_name, -trait_name_new)

# change from long to wide
height_width_wide <- height_width_long2 %>%
  spread(trait_name_new_new, value)

height_width_wide <- select(height_width_wide, -scientificNameStd, -category, -plantType, -exp_tested)

names(height_width_wide)[names(height_width_wide) == 'species'] <- 'entity'

# join to main dataset
all_entities_short <- left_join(all_entities_short, height_width_wide, by = "entity")

# rearrange all the columns
all_entities_short <- select(all_entities_short, scientificNameStd, family, genus, species, entity, synonym, model_type, plantType, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, category, exp_tested, trait_name, value,
                             bird, insect, lizard, native_mammal, pollinator, ecological_value, height_min, height_max, width_min, width_max, shade_value, shade_index, carbon_value, carbon_index)


##### fix up family names for H, HC, GC

family_fix <- all_entities_short %>%
  filter(category == "H"| category == "HC"| category == "GC") %>%
  select(family, genus, species, entity, category) %>%
  distinct(family, genus, species, entity, category)

genus_family <- all_entities_short %>%
  filter(category == "SP") %>%
  select(family, genus)

# check no genus is in two families
sum <- genus_family %>%
  distinct(family, genus) %>%
  group_by(family, genus) %>%
  summarise(frequency = n()) # don't think so
# but there are missing families for Cephalotaxus and Chenopodium

# https://stackoverflow.com/questions/51398814/r-if-else-with-multiple-conditions-for-character-vectors-with-nas
### THIS WILL BE IMPORTANT WHEN CHANGING TRAIT LEVELS!!!!

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(family = if_else(genus == "Cephalotaxus", "Taxaceae", family))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(family = if_else(genus == "Chenopodium", "Amaranthaceae", family))

# check if fixed
genus_family <- all_entities_short %>%
  filter(category == "SP") %>%
  select(family, genus)

sum <- genus_family %>%
  distinct(family, genus) %>%
  group_by(family, genus) %>%
  summarise(frequency = n()) # fixed!

# filter out H, HC and GC from database
missing <- all_entities_short %>%
  filter(category == "H"| category == "HC"| category == "GC") %>%
  select(-family)

found <- left_join(missing, sum, by = "genus")

# some missing families, Michelia, Rheum

found <- found %>%
  mutate_if(is.factor, as.character) %>%
  mutate(family = if_else(genus == "Michelia", "Magnoliaceae", family))

found <- found %>%
  mutate_if(is.factor, as.character) %>%
  mutate(family = if_else(genus == "Rheum", "Polygonaceae", family))

## add parents of Gc, H, HC

parents <- read.csv("Master_database_input/hybrids_genus_cultivars_parents.csv")

parents <- select(parents, -plantType)

names(parents)[names(parents) == 'Species'] <- 'entity'

# join
found <- left_join(found, parents, by = "entity")

# rearrange the columns

found <- found %>%
  select(-frequency) %>%
  select(scientificNameStd, family, genus, species, entity, synonym, Parent_1, Parent_2,Parent_3, Parent_4, model_type, plantType, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, category, exp_tested, trait_name, value,
         bird, insect, lizard, native_mammal, pollinator, ecological_value, height_min, height_max, width_min, width_max, shade_value, shade_index, carbon_value, carbon_index)

# replace the blank parent cells with NA

found <- found %>%
  mutate_if(is.factor, as.character) %>%
  mutate(Parent_2 = if_else(Parent_2 == "", "NA", Parent_2))

found <- found %>%
  mutate_if(is.factor, as.character) %>%
  mutate(Parent_3 = if_else(Parent_3 == "", "NA", Parent_3))

found <- found %>%
  mutate_if(is.factor, as.character) %>%
  mutate(Parent_4 = if_else(Parent_4 == "", "NA", Parent_4))

# remove H, HC, Gc from database

all_entities_short <- all_entities_short %>%
  filter(category != "H" & category != "GC" & category != "HC")

# add parent columns to database

all_entities_short$Parent_1 <- "NA"
all_entities_short$Parent_2 <- "NA"
all_entities_short$Parent_3 <- "NA"
all_entities_short$Parent_4 <- "NA"

# rearrange columns

all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, entity, synonym, Parent_1, Parent_2,Parent_3, Parent_4, model_type, plantType, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, category, exp_tested, trait_name, value,
         bird, insect, lizard, native_mammal, pollinator, ecological_value, height_min, height_max, width_min, width_max, shade_value, shade_index, carbon_value, carbon_index)

# join back the H, HC, GCs

all_entities_short <- bind_rows(all_entities_short, found)

all_entities_short <- arrange(all_entities_short, entity, trait_name, value)

# some synonyms with missing family
syn <- all_entities_short %>%
  filter(family == "") %>%
  select(entity) %>%
  distinct(entity) #17

# find and fix

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(family = if_else(entity == "Abutilon megapotamicum", "Malvaceae", family), 
         family = if_else(entity == "Aloe variegata", "Asphodelaceae", family),
         family = if_else(entity == "Anemone hupehensis", "Ranunculaceae", family),
         family = if_else(entity == "Anemone hupehensis japonica", "Ranunculaceae", family),
         family = if_else(entity == "Anemone tomentosa", "Ranunculaceae", family),
         family = if_else(entity == "Babingtonia behrii", "Myrtaceae", family),
         family = if_else(entity == "Baeckea crassifolia", "Myrtaceae", family),
         family = if_else(entity == "Beaufortia sparsa", "Myrtaceae", family),
         family = if_else(entity == "Beaufortia squarrosa", "Myrtaceae", family),
         family = if_else(entity == "Calothamnus quadrifidus", "Myrtaceae", family),
         family = if_else(entity == "Calothamnus sanguineus", "Myrtaceae", family),
         family = if_else(entity == "Calothamnus villosus", "Myrtaceae", family),
         family = if_else(entity == "Eremaea beaufortioides", "Myrtaceae", family),
         family = if_else(entity == "Jasminum fruticans", "Oleaceae", family),
         family = if_else(entity == "Leucopogon parviflorus", "Ericaceae", family),
         family = if_else(entity == "Quisqualis indica", "Myrtaceae", family),
         family = if_else(entity == "Regelia velutina", "Myrtaceae", family))

# check

syn_check <- filter(all_entities_short, family == "") # all fixed

# filter out cultivars
# check if sciename = entity
# check if they are all synonyms

same <- all_entities_short %>%
  filter(category != "CULVAR") %>%
  filter(scientificNameStd != entity) %>%
  select(entity, category) %>%
  distinct(entity, category)

# fix the species
all_entities_short[] <-lapply(all_entities_short, gsub, pattern = "Magnolia Iiliiflora", replacement = "Magnolia liliiflora")
all_entities_short[] <-lapply(all_entities_short, gsub, pattern = "Pittosporum phylliraeoides", replacement = "Pittosporum phillyraeoides")

# remove the category 'synonym'

# these are the problemativ ones
# Species with multiple synonyms that have 5 min traits: Abelia uniflora, Myoporum tenuifolium

# Species and synonyms that have 5 min traits: Bauhinia variegata, Coronidium scorpioides,
# Cupressus arizonica, Eucalyptus leucoxylon, Ficinia nodosa, Melaleuca fulgens,
# Syringa vulgaris, Syzygium tierneyanum, Virgilia oroboides, Pittosporum phillyraeoides

# summary of what we have so far

summary_original <- all_entities_short %>%
  select(entity, category) %>%
  distinct(entity, category) %>%
  group_by(category) %>%
  summarise(frequency = n())

# exclude the problematic ones

syn_good <- all_entities_short %>%
  filter(scientificNameStd != "Abelia uniflora" & scientificNameStd != "Myoporum tenuifolium" & scientificNameStd != "Pittosporum phillyraeoides" 
         & scientificNameStd != "Bauhinia variegata" & scientificNameStd != "Coronidium scorpioides" & scientificNameStd != "Cupressus arizonica" 
         & scientificNameStd != "Eucalyptus leucoxylon" & scientificNameStd != "Ficinia nodosa" & scientificNameStd != "Melaleuca fulgens" 
         & scientificNameStd != "Syringa vulgaris" & scientificNameStd != "Syzygium tierneyanum" & scientificNameStd != "Virgilia oroboides")

# add back the GC, H, HC
others <- all_entities_short %>%
  filter(category == "H" | category == "HC" | category == "GC")

syn_good <- bind_rows(syn_good, others)

# filter out just the synonyms
syn_good_change <- filter(syn_good, category == "SYN")

# get rid of these from syn good
syn_good <- filter(syn_good, category == "CULVAR" | category == "GC" | category == "H" | category == "HC" | category == "SP" | category == "SSP")

# make the entity and scinames the same
syn_good_change$entity <- syn_good_change$scientificNameStd

# change category into species
syn_good_change$category <- "SP"

# species with multiple synonyms and 5 min traits
# Abelia uniflora, Myoporum tenuifolium
multi_syn <- all_entities_short %>%
  filter(scientificNameStd == "Abelia uniflora" | scientificNameStd == "Myoporum tenuifolium")

multi_syn <- multi_syn %>%
  mutate_if(is.factor, as.character) %>%
  mutate(synonym = if_else(entity == "Abelia engleriana", "Abelia engleriana, Abelia schumannii", synonym),
         synonym = if_else(entity == "Myoporum acuminatum", "Myoporum acuminatum, Myoporum montanum", synonym))

multi_syn <- multi_syn %>%
  filter(entity != "Abelia schumannii" & entity != "Myoporum montanum")

# make sciname and entity name the same
multi_syn$entity <- multi_syn$scientificNameStd
# make syn into sp
multi_syn$category <- "SP"

# species with 5 min traits for species and synonyms
sp_syn <- all_entities_short %>%
  filter(scientificNameStd == "Pittosporum phillyraeoides" | scientificNameStd == "Bauhinia variegata" | scientificNameStd == "Coronidium scorpioides" | scientificNameStd == "Cupressus arizonica" 
         | scientificNameStd == "Eucalyptus leucoxylon" | scientificNameStd == "Ficinia nodosa" | scientificNameStd == "Melaleuca fulgens" 
         | scientificNameStd == "Syringa vulgaris" | scientificNameStd == "Syzygium tierneyanum" | scientificNameStd == "Virgilia oroboides")

# salvage the cultivars
sp_syn_cultivars <- filter(sp_syn, category == "CULVAR")

# remove the cultivars and species
sp_syn <- filter(sp_syn, category != "CULVAR" & category != "SP")

sp_syn$entity <- sp_syn$scientificNameStd
sp_syn$category <- "SP"

# join everything back together
all_entities_short_check <- bind_rows(syn_good, syn_good_change, multi_syn, sp_syn, sp_syn_cultivars)

# check everything is still there
summary_new <- all_entities_short_check %>%
  select(entity, category) %>%
  distinct(entity, category) %>%
  group_by(category) %>%
  summarise(frequency = n()) # seems to all be there

all_entities_short <- all_entities_short_check
all_entities_short <- arrange(all_entities_short, entity, trait_name, value)

check_syn <- all_entities_short %>%
  filter(synonym != "NA") %>%
  distinct(entity) # 166 synonyms which is 168 from original minus the two species I combined

# change entity to plant name
names(all_entities_short)[names(all_entities_short) == 'entity'] <- 'plant_name'

##### remove medicinal and apiary from usage

# first check coverage of traits

species <- all_entities_short %>%
  distinct(plant_name)
# 2624 plant names (2637 - 13 problematic synonyms = 2624!!!)

traits <- all_entities_short %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2624)*100) # all relevant traits still 100%

# remove medicinal from usage

all_entities_short <- all_entities_short %>%
  filter(value != "medicinal")

# check coverage again

traits <- all_entities_short %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2624)*100) # usage still 100%

# check apiary

no_apiary <- all_entities_short %>%
  filter(value != "apiary")

# check coverage again

traits <- no_apiary %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2624)*100) # usage still 100%

# if apiary then add '1' to pollinator

all_entities_short$pollinator <- ifelse(all_entities_short$value == "apiary", 1, 
                                        all_entities_short$pollinator)

# check
check_pollinator <- all_entities_short %>%
  filter(value == "apiary") %>%
  select(plant_name, pollinator, value) # all accounted for

# remove apiary from dataset as a usage
all_entities_short <- all_entities_short %>%
  filter(value != "apiary")

# rearrange columns to make more sense
all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, plantType, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, value,
         bird, insect, lizard, native_mammal, pollinator, ecological_value, height_min, height_max, width_min, width_max, shade_value, shade_index, carbon_value, carbon_index)

# need to calculate a shade value
all_entities_short$shade_value <- "NA"

all_entities_short$width_max <- as.numeric(as.character(all_entities_short$width_max))

all_entities_short$shade_value <- ifelse(all_entities_short$plantType == "Tree", pi*all_entities_short$width_max^2,
                                         all_entities_short$shade_value)


############

# FINAL FORMAT OF THE DATABASE

# first check glasshouse species

gh <- all_entities_short %>%
  distinct(plant_name, exp_tested) %>%
  group_by(exp_tested) %>%
  summarise(frequency = n())
# 111 plants from glasshouse, some may be missing...
# remove old drought classifications and add new ones....

# add a Koppen zone column

all_entities_short$Koppen_zone <- ""

all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, Koppen_zone, plantType, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, value,
         bird, insect, lizard, native_mammal, pollinator, ecological_value, height_min, height_max, width_min, width_max, shade_value, shade_index, carbon_value, carbon_index)

# change the column names

names(all_entities_short)[names(all_entities_short) == 'plantType'] <- 'growth_form'

# select the traits that we want

all_entities_short <- all_entities_short %>%
  filter(trait_name == "common_name" | trait_name == "flower_colour" | trait_name == "flower_period" | trait_name == "leaf_loss" 
         | trait_name == "light_level" | trait_name == "placement" | trait_name == "usage" | trait_name == "height_max" 
         | trait_name == "height_min" | trait_name == "height_average" | trait_name == "width_max" | trait_name == "width_min" | trait_name == "width_average"
         | trait_name == "soil_type" | trait_name == "soil_pH" | trait_name == "supp_watering" | trait_name == "ideal_conditions" | trait_name == "frost_tolerance" | trait_name == "drought_tolerance" 
         | trait_name == "coastal_tolerance" | trait_name == "habit_canopy" | trait_name == "growth_rate" | trait_name == "foliage_colour" | trait_name == "risk" 
         | trait_name == "weed_status")

# check they are all there
trait_name_check <- all_entities_short %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n())

# create new columns to change the trait_name and value
all_entities_short$trait_name_new <- all_entities_short$trait_name
all_entities_short$value_new <- all_entities_short$value

# rearrange columns to make more sense
all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, Koppen_zone, growth_form, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, trait_name_new, value,
         value_new, bird, insect, lizard, native_mammal, pollinator, ecological_value, height_min, height_max, width_min, width_max, shade_value, shade_index, carbon_value, carbon_index)

# COMMON NAME
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "common_name", "common name", trait_name_new))

# make common names capitalised
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(trait_name == "common_name", gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", all_entities_short$value_new, perl=TRUE),
                             all_entities_short$value_new))

# GROWTH FORM
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(growth_form = if_else(growth_form == "Bromeliad", "Shrub", growth_form),
         growth_form = if_else(growth_form == "Bulb", "Herb", growth_form),
         growth_form = if_else(growth_form == "Orchid", "Herb", growth_form),
         growth_form = if_else(growth_form == "Vegetable", "Herb", growth_form),
         growth_form = if_else(growth_form == "Strap-leaved", "Herb", growth_form))

# FLOWER COLOUR
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "flower_colour", "flower colour", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "gold", "yellow", value_new),
         value_new = if_else(value == "golden", "yellow", value_new),
         value_new = if_else(value == "grey", "black", value_new), 
         value_new = if_else(value == "insignificant", "inconspicuous", value_new),
         value_new = if_else(value == "magenta", "pink", value_new),
         value_new = if_else(value == "mauve", "purple", value_new),
         value_new = if_else(value == "not_applicable", "does not flower", value_new),
         value_new = if_else(value == "violet", "purple", value_new))

# FLOWER PERIOD
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "flower_period", "flower period", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "Any_time", "any time", value_new),
         value_new = if_else(value == "not_applicable", "does not flower", value_new))

# make the first letters capital
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "summer", gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", all_entities_short$value_new, perl=TRUE), value_new),
         value_new = if_else(value == "autumn", gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", all_entities_short$value_new, perl=TRUE), value_new),
         value_new = if_else(value == "winter", gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", all_entities_short$value_new, perl=TRUE), value_new),
         value_new = if_else(value == "spring", gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", all_entities_short$value_new, perl=TRUE), value_new))

# PLACEMENT
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "placement", "urban context", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "avenue", "street", value_new),
         value_new = if_else(value == "largegarden", "park", value_new),
         value_new = if_else(value == "powerlines", "under powerlines", value_new))

# USAGE
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "usage", "uses", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "cutflower", "cut flowers", value_new),
         value_new = if_else(value == "cutflowers", "cut flowers", value_new),
         value_new = if_else(value == "cutflowers", "cut flowers", value_new),
         value_new = if_else(value == "edible", "putatively edible", value_new),
         value_new = if_else(value == "erosion", "erosion control", value_new),
         value_new = if_else(value == "featureshrub", "feature", value_new),
         value_new = if_else(value == "featuretree", "feature", value_new),
         value_new = if_else(value == "featuretropical", "feature", value_new),
         value_new = if_else(value == "fire_retardant", "putatively fire retardant", value_new),
         value_new = if_else(value == "groundcover", "ground cover", value_new),
         value_new = if_else(value == "massplanting", "mass planting", value_new),
         value_new = if_else(value == "playgroundfriendly", "playground friendly", value_new),
         value_new = if_else(value == "sensorycolour", "playground friendly", value_new),
         value_new = if_else(value == "sensorytouch", "playground friendly", value_new))

# HEIGHT AND WIDTH
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "height_average", "average height", trait_name_new),
         trait_name_new = if_else(trait_name == "width_average", "average width", trait_name_new),
         trait_name_new = if_else(trait_name == "height_max", "maximum height", trait_name_new),
         trait_name_new = if_else(trait_name == "height_min", "minimum height", trait_name_new),
         trait_name_new = if_else(trait_name == "width_max", "maximum width", trait_name_new),
         trait_name_new = if_else(trait_name == "width_min", "minimum width", trait_name_new))


# SOIL
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "soil_type", "soil type", trait_name_new),
         trait_name_new = if_else(trait_name == "soil_pH", "soil pH", trait_name_new))


# PLANTING AND MAINTENANCE
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "supp_watering", "planting and maintenance", trait_name_new),
         trait_name_new = if_else(trait_name == "ideal_conditions", "planting and maintenance", trait_name_new))


all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "high", "high water needs", value_new),
         value_new = if_else(value == "low", "low water needs", value_new),
         value_new = if_else(value == "medium", "medium water needs", value_new),
         value_new = if_else(value == "none", "low water needs", value_new),
         value_new = if_else(value == "fertile", "fertile soil", value_new),
         value_new = if_else(value == "fertile", "fertile soil", value_new),
         value_new = if_else(value == "lateral_space", "lateral space", value_new),
         value_new = if_else(value == "poorly_drained", "poorly drained soil", value_new),
         value_new = if_else(value == "sheltered", "protected", value_new),
         value_new = if_else(value == "well_drained", "well drained soil", value_new))


# change back the growth rate medium values
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(trait_name == "growth_rate" & value == "medium", "medium", value_new))

# filter out the rest
remove_maintenance <- all_entities_short %>%
  filter(trait_name == "ideal_conditions" & value == "acidic" | trait_name == "ideal_conditions" & value == "alkaline" | 
           trait_name == "ideal_conditions" & value == "clay_soil" | trait_name == "ideal_conditions" & value == "clay_soils" | 
           trait_name == "ideal_conditions" & value == "dry" | trait_name == "ideal_conditions" & value == "flowers_fullsun" | 
           trait_name == "ideal_conditions" & value == "fullshade" | trait_name == "ideal_conditions" & value == "fullsun" | 
           trait_name == "ideal_conditions" & value == "gravelly_soil" | trait_name == "ideal_conditions" & value == "high" | 
           trait_name == "ideal_conditions" & value == "humid" | trait_name == "ideal_conditions" & value == "large_rootspace" | 
           trait_name == "ideal_conditions" & value == "loam_soil" | trait_name == "ideal_conditions" & value == "loamy_soil" | 
           trait_name == "ideal_conditions" & value == "loamy_soils" | trait_name == "ideal_conditions" & value == "natural_pH" | 
           trait_name == "ideal_conditions" & value == "neutral_pH" | trait_name == "ideal_conditions" & value == "partshade" | 
           trait_name == "ideal_conditions" & value == "sandy_soil" | trait_name == "ideal_conditions" & value == "shaded" | 
           trait_name == "ideal_conditions" & value == "variable" | trait_name == "ideal_conditions" & value == "moist")

# remove
all_entities_short <- anti_join(all_entities_short, remove_maintenance)

# LEAF LOSS
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "leaf_loss", "leaf loss", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "semi_deciduous", "semi-deciduous", value_new))

# LIGHT LEVEL
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "light_level", "shade tolerance", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "fullshade", "full shade", value_new),
         value_new = if_else(value == "fullsun", "full sun", value_new),
         value_new = if_else(value == "partshade", "part shade", value_new))


# TOLERANCES
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "frost_tolerance", "frost tolerance", trait_name_new),
         trait_name_new = if_else(trait_name == "drought_tolerance", "drought tolerance", trait_name_new),
         trait_name_new = if_else(trait_name == "drought_tolerance", "drought tolerance", trait_name_new),
         trait_name_new = if_else(trait_name == "coastal_tolerance", "coastal tolerance", trait_name_new))

# GROWTH RATE
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "growth_rate", "growth rate", trait_name_new))

# FOLIAGE COLOUR
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "foliage_colour", "leaf colour", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "bluegreen", "blue-green", value_new),
         value_new = if_else(value == "darkgreen", "green", value_new),
         value_new = if_else(value == "dullgreen", "green", value_new),
         value_new = if_else(value == "glossygreen", "green", value_new),
         value_new = if_else(value == "greygreen", "grey-green", value_new),
         value_new = if_else(value == "lightgreen", "green", value_new),
         value_new = if_else(value == "redpink", "red", value_new),
         value_new = if_else(value == "pinkred", "red", value_new),
         value_new = if_else(value == "silver", "silvery", value_new),
         value_new = if_else(value == "silver_foliage", "silvery", value_new),
         value_new = if_else(value == "silvergreen", "silvery", value_new),
         value_new = if_else(value == "silvergrey", "silvery", value_new),
         value_new = if_else(value == "yellowgreen", "yellow", value_new),
         value_new = if_else(value == "variagations", "variagated", value_new))

# filter out rest
remove_foliage <- all_entities_short %>%
  filter(trait_name == "foliage_colour" & value == "bluegrey" | trait_name == "foliage_colour" & value == "bronze" | 
           trait_name == "foliage_colour" & value == "burgundy" | trait_name == "foliage_colour" & value == "copper" | 
           trait_name == "foliage_colour" & value == "cream" | trait_name == "foliage_colour" & value == "gold" | 
           trait_name == "foliage_colour" & value == "grey" | trait_name == "foliage_colour" & value == "high" |
           trait_name == "foliage_colour" & value == "orange" | trait_name == "foliage_colour" & value == "white")

# remove
all_entities_short <- anti_join(all_entities_short, remove_foliage)

# HABIT CANOPY
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "habit_canopy", "canopy shape", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "columnar", "upright", value_new),
         value_new = if_else(value == "domed", "rounded", value_new),
         value_new = if_else(value == "hedging_possible", "hedging possible", value_new),
         value_new = if_else(value == "narrow", "upright", value_new),
         value_new = if_else(value == "oval", "rounded", value_new),
         value_new = if_else(value == "prostrate", "upright", value_new))

# filter out rest
remove_habit_canopy <- all_entities_short %>%
  filter(trait_name == "habit_canopy" & value == "arborescent" | trait_name == "habit_canopy" & value == "branching" | 
           trait_name == "habit_canopy" & value == "clumping" | trait_name == "habit_canopy" & value == "compact" | 
           trait_name == "habit_canopy" & value == "conical" | trait_name == "habit_canopy" & value == "vase") 

# remove
all_entities_short <- anti_join(all_entities_short, remove_habit_canopy)

# RISK
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "mildallergen", "allergen", value_new),
         value_new = if_else(value == "moderateallergen", "allergen", value_new),
         value_new = if_else(value == "poison", "poisonous or toxic", value_new),
         value_new = if_else(value == "severeallergen", "allergen", value_new))

# filter the rest
remove_risk <- all_entities_short %>%
  filter(trait_name == "risk" & value == "branchdrop" | trait_name == "risk" & value == "disease_prone" | 
           trait_name == "risk" & value == "fruitfall" | trait_name == "risk" & value == "highly_flammable" | 
           trait_name == "risk" & value == "infrastructure_damage" | trait_name == "risk" & value == "litterfall" | 
           trait_name == "risk" & value == "maloderous" | trait_name == "risk" & value == "malodorous" | 
           trait_name == "risk" & value == "parasitic" | trait_name == "risk" & value == "possible_weed" | 
           trait_name == "risk" & value == "sap_fall" | trait_name == "risk" & value == "suckering")

# remove
all_entities_short <- anti_join(all_entities_short, remove_risk)

# WEED
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "weed_status", "weed status", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "act", "Australian Capital Territory", value_new),
         value_new = if_else(value == "nsw", "New South Wales", value_new),
         value_new = if_else(value == "nt", "Northern Territory", value_new),
         value_new = if_else(value == "qld", "Queensland", value_new),
         value_new = if_else(value == "sa", "South Australia", value_new),
         value_new = if_else(value == "tas", "Tasmania", value_new),
         value_new = if_else(value == "wa", "Western Australia", value_new),
         value_new = if_else(value == "vic", "Victoria", value_new))

# filter the rest
remove_weed <- all_entities_short %>%
  filter(trait_name == "weed_status" & value == "potentially_act" | trait_name == "weed_status" & value == "potentially_nsw" | 
           trait_name == "weed_status" & value == "potentially_nt" | trait_name == "weed_status" & value == "potentially_qld" | 
           trait_name == "weed_status" & value == "potentially_sa" | trait_name == "weed_status" & value == "potentially_tas" |
           trait_name == "weed_status" & value == "potentially_vic" | trait_name == "weed_status" & value == "potentially_wa")

# remove
all_entities_short <- anti_join(all_entities_short, remove_weed)

# new trait and value columns
all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, Koppen_zone, growth_form, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name_new, value_new,
         bird, insect, lizard, native_mammal, pollinator, ecological_value, height_min, height_max, width_min, width_max, shade_value, shade_index, carbon_value, carbon_index)

# end need to distinct everything
all_entities_short <- all_entities_short %>%
  distinct(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, Koppen_zone, growth_form, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name_new,
           value_new, bird, insect, lizard, native_mammal, pollinator, ecological_value, height_min, height_max, width_min, width_max, shade_value, shade_index, carbon_value, carbon_index)

names(all_entities_short)[names(all_entities_short) == 'trait_name_new'] <- 'trait_name'
names(all_entities_short)[names(all_entities_short) == 'value_new'] <- 'value'

# arrange in alphabetical order
all_entities_short <- arrange(all_entities_short, plant_name, trait_name, value)

write.csv(all_entities_short,"Master_database_output/FINAL/trait_database_ST_FINAL_10.3.2021_vers1.1.csv",row.names=FALSE)

# do some checks

summary_new_new <- all_entities_short_check %>%
  select(entity, category) %>%
  distinct(entity, category) %>%
  group_by(category) %>%
  summarise(frequency = n())
# 2624 entities

# check gh species

gh_species <- all_entities_short %>%
  filter(exp_tested == "Y") %>%
  select(plant_name) %>%
  distinct(plant_name)

####################################################################################################################################

############### Version 1.2
### removed Calodendrum capense as a species we checked in the gh
### added height and width data to versioning
### created pages for the cultivars we tested in the glasshouse (if they were missing)
### changed ecological_value to biodiversity_value
### added dehydration and heat tolerance variables for gh species
### populated the model_type column with sdm, niche and NA
### fixed species with high and low water usage and species with high water needs and drought tolerant


library(tidyverse)

everything <- read.csv("Master_database_input/EVERYTHING_traits_14Mar2021.csv")

everything_gh <- read.csv("Master_database_input/EVERYTHING_gh_14Mar2021.csv")

all_entities <- bind_rows(everything, everything_gh)

all_entities_short <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value) %>%
  distinct(scientificNameStd, species, category, exp_tested, trait_name, value)

check <- distinct(all_entities_short, species) # 2636 species, haven't lost anything

# remove the old height and width dimensions

all_entities_short <- all_entities_short %>%
  filter(trait_name != "max_height", trait_name != "height", trait_name != "min_height", 
         trait_name != "max_width", trait_name != "width", trait_name != "min_width")

check <- distinct(all_entities_short, species) # 2636 species, haven't lost anything

##### new height and width data

measurements <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  filter(trait_name == "max_height" | trait_name == "height" | trait_name == "min_height" | 
           trait_name == "max_width" | trait_name == "width" | trait_name == "min_width") %>%
  select(scientificNameStd, species, category, exp_tested, plantType, trait_name, value)

measurements$trait_name_new <- measurements$trait_name # create a new variable to trait name

measurements <- select(measurements, scientificNameStd, species, category, exp_tested, plantType, trait_name, trait_name_new, value)

measurements$trait_name_new <- ifelse(measurements$trait_name == "max_height", "height", 
                                      measurements$trait_name_new)

measurements$trait_name_new <- ifelse(measurements$trait_name == "min_height", "height", 
                                      measurements$trait_name_new)

measurements$trait_name_new <- ifelse(measurements$trait_name == "max_width", "width", 
                                      measurements$trait_name_new)

measurements$trait_name_new <- ifelse(measurements$trait_name == "min_width", "width", 
                                      measurements$trait_name_new)

glimpse(measurements)

measurements$value <- as.numeric(as.character(measurements$value))

height_width <- measurements %>%
  group_by(scientificNameStd, species, category, exp_tested, plantType, trait_name_new) %>%
  summarise(max = max(value), min = min(value), average = mean(value), range = max - min)

height_width$average <- format(round(height_width$average, 1), nsmall = 2) # make values one decimal place

glimpse(height_width)
height_width$average <- as.numeric(as.character(height_width$average))

filter(height_width, range < 0) # no mistakes

names <- distinct(height_width, species) # 2637, am not missing anything!

# write.csv(height_width, "Master_database_output/final_data/height_width_all_ST_5Mar2021.csv", row.names = FALSE)

height_width <- select(height_width, -range)

# change to long format
# http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/

height_width_long <- height_width %>%
  gather(trait_name, value, max:average)

height_width_long <- height_width_long %>%
  mutate(trait_name_new_new = paste0(trait_name_new, "_", trait_name))

height_width_long <- select(height_width_long, scientificNameStd, species, category, exp_tested, trait_name_new_new, value)  

# remove plant type

height_width_long <- height_width_long[,2:7]
  
# change the column name
names(height_width_long)[names(height_width_long) == 'trait_name_new_new'] <- 'trait_name'

glimpse(height_width_long)
height_width_long$value <- as.character(height_width_long$value)

# join to master dataset

all_entities_short <- bind_rows(all_entities_short, height_width_long)
all_entities_short <- arrange(all_entities_short, scientificNameStd, species, trait_name, value)

check <- distinct(all_entities_short, species) # 2636 species, haven't lost anything

### do drought classifications

drought <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  filter(trait_name == "drought_tolerance") %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value)

drought_summary <- drought %>%
  group_by(scientificNameStd, species) %>%
  summarise(number_records = n())
# 1743 records

drought["number"] <- 1 # add new column populated by '1'

drought_summary <- drought %>%
  group_by(scientificNameStd, species, category, exp_tested, trait_name, value) %>%
  summarise(number_records = sum(number))

drought_long <- drought_summary %>%
  spread(key = value, value = number_records, fill = 0) %>%
  mutate(total_records = sum(No, Yes)) %>%
  arrange(desc(total_records)) %>%
  group_by(total_records) %>%
  mutate(number_species = n())

# create proportions
drought_long <- drought_long %>%
  mutate(no_proportion = (No/total_records) * 100, 
         yes_protortion = (Yes/total_records) * 100)

# apply the consensus approach
drought_long <- drought_long %>%
  mutate(value = case_when(no_proportion >= 75 ~ "putatively no",
                           yes_protortion >= 75 ~ "putatively high",
                           TRUE ~ "putatively moderate")) %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value)

# remove 'total records'
drought_long <- drought_long[,2:7]

# remove the old drought data

all_entities_short <- all_entities_short %>%
  filter(trait_name != "drought_tolerance")

# join to master dataset
all_entities_short <- bind_rows(all_entities_short, drought_long)
all_entities_short <- arrange(all_entities_short, scientificNameStd, species, trait_name, value)

# do frost classifications

frost <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  filter(trait_name == "frost_tolerance") %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value)

# change light to yes
frost <- frost %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value = if_else(value == "light", "Yes", value))

# make all the values upper case
frost$value <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", frost$value, perl=TRUE)

frost_summary <- frost %>%
  group_by(scientificNameStd, species) %>%
  summarise(number_records = n())
# 1852 records

frost["number"] <- 1 # add new column populated by '1'

frost_summary <- frost %>%
  group_by(scientificNameStd, species, category, exp_tested, trait_name, value) %>%
  summarise(number_records = sum(number))

frost_long <- frost_summary %>%
  spread(key = value, value = number_records, fill = 0) %>%
  mutate(total_records = sum(No, Yes)) %>%
  arrange(desc(total_records)) %>%
  group_by(total_records) %>%
  mutate(number_species = n())

# create proportions
frost_long <- frost_long %>%
  mutate(no_proportion = (No/total_records) * 100, 
         yes_protortion = (Yes/total_records) * 100)

# apply the consensus approach
frost_long <- frost_long %>%
  mutate(value = case_when(no_proportion >= 75 ~ "putatively no",
                           yes_protortion >= 75 ~ "putatively high",
                           TRUE ~ "putatively moderate")) %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value)

# remove 'total records'
frost_long <- frost_long[,2:7]

# remove the old frost data

all_entities_short <- all_entities_short %>%
  filter(trait_name != "frost_tolerance")

# join to master dataset
all_entities_short <- bind_rows(all_entities_short, frost_long)
all_entities_short <- arrange(all_entities_short, scientificNameStd, species, trait_name, value)

# do coastal tolerance classifications

coastal <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  filter(trait_name == "coastal_tolerance") %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value)

# make all the values upper case
coastal$value <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", coastal$value, perl=TRUE)

coastal_summary <- coastal %>%
  group_by(scientificNameStd, species) %>%
  summarise(number_records = n())
# 874 records

coastal["number"] <- 1 # add new column populated by '1'

coastal_summary <- coastal %>%
  group_by(scientificNameStd, species, category, exp_tested, trait_name, value) %>%
  summarise(number_records = sum(number))

coastal_long <- coastal_summary %>%
  spread(key = value, value = number_records, fill = 0) %>%
  mutate(total_records = sum(No, Yes)) %>%
  arrange(desc(total_records)) %>%
  group_by(total_records) %>%
  mutate(number_species = n())

# create proportions
coastal_long <- coastal_long %>%
  mutate(no_proportion = (No/total_records) * 100, 
         yes_protortion = (Yes/total_records) * 100)

# apply the consensus approach
coastal_long <- coastal_long %>%
  mutate(value = case_when(no_proportion >= 75 ~ "putatively no",
                           yes_protortion >= 75 ~ "putatively high",
                           TRUE ~ "putatively moderate")) %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value)

# remove 'total records'
coastal_long <- coastal_long[,2:7]

# remove the old coastal data

all_entities_short <- all_entities_short %>%
  filter(trait_name != "coastal_tolerance")

# join to master dataset
all_entities_short <- bind_rows(all_entities_short, coastal_long)
all_entities_short <- arrange(all_entities_short, scientificNameStd, species, trait_name, value)

##### add back plant type and origin

plant_type_origin <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  select(species, plantType, origin) %>%
  distinct(species, plantType, origin) # 2636 plants

# join to main dataset
all_entities_short <- left_join(all_entities_short, plant_type_origin, by = "species")

all_entities_short <- all_entities_short %>%
  select(scientificNameStd, species, plantType, origin, category, exp_tested, trait_name, value) %>%
  filter(trait_name != "native_exotic") # do not remove 'form' as will need that later

###### separate the species and genus names
# https://stackoverflow.com/questions/4350440/split-data-frame-string-column-into-multiple-columns

# first change species col name
names(all_entities_short)[names(all_entities_short) == 'species'] <- 'entity'

all_entities_short <- all_entities_short %>%
  separate(scientificNameStd, c("genus", "species"), " ", remove = FALSE)

# need to fix GC, H and HC

all_entities_short$genus <- ifelse(all_entities_short$category == "H", word(all_entities_short$entity, 1), 
                                   all_entities_short$genus)
all_entities_short$genus <- ifelse(all_entities_short$category == "GC", word(all_entities_short$entity, 1), 
                                   all_entities_short$genus)
all_entities_short$genus <- ifelse(all_entities_short$category == "HC", word(all_entities_short$entity, 1), 
                                   all_entities_short$genus)

#### attach the family names

## use old list I created which should be the same

family <- read.csv("Master_database_output/taxonomy_checks/taxonstandcheck_ST_1.3.2021.csv")

family <- select(family, Taxon, Family)

names(family)[names(family) == 'Taxon'] <- 'scientificNameStd'
names(family)[names(family) == 'Family'] <- 'family'

all_entities_short <- left_join(all_entities_short, family, by = "scientificNameStd")

all_entities_short <- select(all_entities_short, scientificNameStd, family, genus, species, entity, plantType, origin, category, exp_tested, trait_name, value)
# family is blank for H, GC, HC

#### add synonym column

all_entities_short$synonym <- "NA"

all_entities_short$synonym <- ifelse(all_entities_short$category == "SYN", all_entities_short$entity, 
                                     all_entities_short$synonym)

all_entities_short <- select(all_entities_short, scientificNameStd, family, genus, species, entity, synonym, plantType, origin, category, exp_tested, trait_name, value)

####### extract the ecological services data

eco <- all_entities_short %>%
  filter(trait_name == "ecological_services") %>%
  select(entity, trait_name, value)

eco$score <- "1"

# change from long to wide

eco_wide <- eco %>%
  spread(value, score, fill = 0) 

glimpse(eco_wide) 

eco_wide$bird <- as.numeric(as.character(eco_wide$bird))
eco_wide$insect <- as.numeric(as.character(eco_wide$insect))
eco_wide$lizard <- as.numeric(as.character(eco_wide$lizard))
eco_wide$native_mammal <- as.numeric(as.character(eco_wide$native_mammal))
eco_wide$pollinator <- as.numeric(as.character(eco_wide$pollinator))  

eco_wide <- eco_wide %>%
  mutate(biodiversity_score = bird + insect + lizard + native_mammal + pollinator) %>%
  select(-trait_name)

# join to main database
all_entities_short <- left_join(all_entities_short, eco_wide, by = "entity")

# replace the NAs with 0

all_entities_short$bird[is.na(all_entities_short$bird)] <- 0
all_entities_short$insect[is.na(all_entities_short$insect)] <- 0
all_entities_short$lizard[is.na(all_entities_short$lizard)] <- 0
all_entities_short$native_mammal[is.na(all_entities_short$native_mammal)] <- 0
all_entities_short$pollinator[is.na(all_entities_short$pollinator)] <- 0
all_entities_short$biodiversity_score[is.na(all_entities_short$biodiversity_score)] <- 0

# remove the ecological services trait
all_entities_short <- all_entities_short %>%
  filter(trait_name != "ecological_services")

##### extract the plant form data

form <- all_entities_short %>%
  filter(trait_name == "form") %>%
  select(entity, trait_name, value)

# fix up
form <- form %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value = if_else(value == "aquatic", "herb", value),
         value = if_else(value == "bromeliad", "herb", value),
         value = if_else(value == "bulb", "herb", value),
         value = if_else(value == "vegetable", "herb", value),
         value = if_else(value == "orchid", "herb", value),
         value = if_else(value == "strap-leaved", "herb", value))

form <- distinct(form, entity, trait_name, value)

form$score <- "1"

# change from long to wide

form_wide <- form %>%
  spread(value, score, fill = 0) 

form_wide <- select(form_wide , -trait_name)

# join to main database
all_entities_short <- left_join(all_entities_short, form_wide, by = "entity")

# remove the form trait
all_entities_short <- all_entities_short %>%
  filter(trait_name != "form")

# add the dummy columns for the biodiversity benefits and model type

all_entities_short$model_type <- ""
all_entities_short$shade_value <- ""
all_entities_short$shade_index <- ""
all_entities_short$carbon_value <- ""
all_entities_short$carbon_index <-""

names(all_entities_short)[names(all_entities_short) == 'biodiversity_score'] <- 'biodiversity_value'

# rearrange all the columns
all_entities_short <- select(all_entities_short, scientificNameStd, family, genus, species, entity, synonym, model_type, plantType, climber, 
                             cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, category, exp_tested, trait_name, value,
                             bird, insect, lizard, native_mammal, pollinator, biodiversity_value, shade_value, shade_index, carbon_value, carbon_index)

#### add the max height and width for shade and carbon values

height_width2 <- select(height_width, -average)

# change from wide to long
# http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/

height_width_long2 <- height_width2 %>%
  gather(trait_name, value, max:min)

height_width_long2 <- height_width_long2 %>%
  mutate(trait_name_new_new = paste0(trait_name_new, "_", trait_name))

height_width_long2 <- select(height_width_long2, -trait_name, -trait_name_new)

# change from long to wide
height_width_wide <- height_width_long2 %>%
  spread(trait_name_new_new, value)

height_width_wide <- select(height_width_wide, -scientificNameStd, -category, -plantType, -exp_tested)

# remove these columns
height_width_wide <- height_width_wide[, 5:9]

names(height_width_wide)[names(height_width_wide) == 'species'] <- 'entity'

# join to main dataset
all_entities_short <- left_join(all_entities_short, height_width_wide, by = "entity")

# rearrange all the columns
all_entities_short <- select(all_entities_short, scientificNameStd, family, genus, species, entity, synonym, model_type, plantType, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, category, exp_tested, trait_name, value,
                             bird, insect, lizard, native_mammal, pollinator, biodiversity_value, height_min, height_max, width_min, width_max, shade_value, shade_index, carbon_value, carbon_index)


##### fix up family names for H, HC, GC

family_fix <- all_entities_short %>%
  filter(category == "H"| category == "HC"| category == "GC") %>%
  select(family, genus, species, entity, category) %>%
  distinct(family, genus, species, entity, category)

genus_family <- all_entities_short %>%
  filter(category == "SP") %>%
  select(family, genus)

# check no genus is in two families
sum <- genus_family %>%
  distinct(family, genus) %>%
  group_by(family, genus) %>%
  summarise(frequency = n()) # don't think so
# but there are missing families for Cephalotaxus and Chenopodium

# https://stackoverflow.com/questions/51398814/r-if-else-with-multiple-conditions-for-character-vectors-with-nas
### THIS WILL BE IMPORTANT WHEN CHANGING TRAIT LEVELS!!!!

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(family = if_else(genus == "Cephalotaxus", "Taxaceae", family))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(family = if_else(genus == "Chenopodium", "Amaranthaceae", family))

# check if fixed
genus_family <- all_entities_short %>%
  filter(category == "SP") %>%
  select(family, genus)

sum <- genus_family %>%
  distinct(family, genus) %>%
  group_by(family, genus) %>%
  summarise(frequency = n()) # fixed!

# filter out H, HC and GC from database
missing <- all_entities_short %>%
  filter(category == "H"| category == "HC"| category == "GC") %>%
  select(-family)

found <- left_join(missing, sum, by = "genus")

# some missing families, Michelia, Rheum

found <- found %>%
  mutate_if(is.factor, as.character) %>%
  mutate(family = if_else(genus == "Michelia", "Magnoliaceae", family))

found <- found %>%
  mutate_if(is.factor, as.character) %>%
  mutate(family = if_else(genus == "Rheum", "Polygonaceae", family))

## add parents of Gc, H, HC

parents <- read.csv("Master_database_input/hybrids_genus_cultivars_parents.csv")

parents <- select(parents, -plantType)

names(parents)[names(parents) == 'Species'] <- 'entity'

# join
found <- left_join(found, parents, by = "entity")

# rearrange the columns

found <- found %>%
  select(-frequency) %>%
  select(scientificNameStd, family, genus, species, entity, synonym, Parent_1, Parent_2,Parent_3, Parent_4, model_type, plantType, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, category, exp_tested, trait_name, value,
         bird, insect, lizard, native_mammal, pollinator, biodiversity_value, height_min, height_max, width_min, width_max, shade_value, shade_index, carbon_value, carbon_index)

# replace the blank parent cells with NA

found <- found %>%
  mutate_if(is.factor, as.character) %>%
  mutate(Parent_2 = if_else(Parent_2 == "", "NA", Parent_2))

found <- found %>%
  mutate_if(is.factor, as.character) %>%
  mutate(Parent_3 = if_else(Parent_3 == "", "NA", Parent_3))

found <- found %>%
  mutate_if(is.factor, as.character) %>%
  mutate(Parent_4 = if_else(Parent_4 == "", "NA", Parent_4))

# remove H, HC, Gc from database

all_entities_short <- all_entities_short %>%
  filter(category != "H" & category != "GC" & category != "HC")

# add parent columns to database

all_entities_short$Parent_1 <- "NA"
all_entities_short$Parent_2 <- "NA"
all_entities_short$Parent_3 <- "NA"
all_entities_short$Parent_4 <- "NA"

# rearrange columns

all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, entity, synonym, Parent_1, Parent_2,Parent_3, Parent_4, model_type, plantType, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, category, exp_tested, trait_name, value,
         bird, insect, lizard, native_mammal, pollinator, biodiversity_value, height_min, height_max, width_min, width_max, shade_value, shade_index, carbon_value, carbon_index)

# join back the H, HC, GCs

all_entities_short <- bind_rows(all_entities_short, found)

all_entities_short <- arrange(all_entities_short, entity, trait_name, value)

# some synonyms with missing family
syn <- all_entities_short %>%
  filter(family == "") %>%
  select(entity) %>%
  distinct(entity) #17

# find and fix

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(family = if_else(entity == "Abutilon megapotamicum", "Malvaceae", family), 
         family = if_else(entity == "Aloe variegata", "Asphodelaceae", family),
         family = if_else(entity == "Anemone hupehensis", "Ranunculaceae", family),
         family = if_else(entity == "Anemone hupehensis japonica", "Ranunculaceae", family),
         family = if_else(entity == "Anemone tomentosa", "Ranunculaceae", family),
         family = if_else(entity == "Babingtonia behrii", "Myrtaceae", family),
         family = if_else(entity == "Baeckea crassifolia", "Myrtaceae", family),
         family = if_else(entity == "Beaufortia sparsa", "Myrtaceae", family),
         family = if_else(entity == "Beaufortia squarrosa", "Myrtaceae", family),
         family = if_else(entity == "Calothamnus quadrifidus", "Myrtaceae", family),
         family = if_else(entity == "Calothamnus sanguineus", "Myrtaceae", family),
         family = if_else(entity == "Calothamnus villosus", "Myrtaceae", family),
         family = if_else(entity == "Eremaea beaufortioides", "Myrtaceae", family),
         family = if_else(entity == "Jasminum fruticans", "Oleaceae", family),
         family = if_else(entity == "Leucopogon parviflorus", "Ericaceae", family),
         family = if_else(entity == "Quisqualis indica", "Myrtaceae", family),
         family = if_else(entity == "Regelia velutina", "Myrtaceae", family))

# check

syn_check <- filter(all_entities_short, family == "") # all fixed

# filter out cultivars
# check if sciename = entity
# check if they are all synonyms

same <- all_entities_short %>%
  filter(category != "CULVAR") %>%
  filter(scientificNameStd != entity) %>%
  select(entity, category) %>%
  distinct(entity, category)

# fix the species
all_entities_short[] <-lapply(all_entities_short, gsub, pattern = "Magnolia Iiliiflora", replacement = "Magnolia liliiflora")
all_entities_short[] <-lapply(all_entities_short, gsub, pattern = "Pittosporum phylliraeoides", replacement = "Pittosporum phillyraeoides")

# remove the category 'synonym'

# these are the problematic ones
# Species with multiple synonyms that have 5 min traits: Abelia uniflora, Myoporum tenuifolium

# Species and synonyms that have 5 min traits: Bauhinia variegata, Coronidium scorpioides,
# Cupressus arizonica, Eucalyptus leucoxylon, Ficinia nodosa, Melaleuca fulgens,
# Syringa vulgaris, Syzygium tierneyanum, Virgilia oroboides, Pittosporum phillyraeoides

# summary of what we have so far

summary_original <- all_entities_short %>%
  select(entity, category) %>%
  distinct(entity, category) %>%
  group_by(category) %>%
  summarise(frequency = n())

# exclude the problematic ones

syn_good <- all_entities_short %>%
  filter(scientificNameStd != "Abelia uniflora" & scientificNameStd != "Myoporum tenuifolium" & scientificNameStd != "Pittosporum phillyraeoides" 
         & scientificNameStd != "Bauhinia variegata" & scientificNameStd != "Coronidium scorpioides" & scientificNameStd != "Cupressus arizonica" 
         & scientificNameStd != "Eucalyptus leucoxylon" & scientificNameStd != "Ficinia nodosa" & scientificNameStd != "Melaleuca fulgens" 
         & scientificNameStd != "Syringa vulgaris" & scientificNameStd != "Syzygium tierneyanum" & scientificNameStd != "Virgilia oroboides")

# add back the GC, H, HC
others <- all_entities_short %>%
  filter(category == "H" | category == "HC" | category == "GC")

# and also Myoporum montanum Arid form
myo <- all_entities_short %>%
  filter(entity == "Myoporum montanum Arid Form")

syn_good <- bind_rows(syn_good, others, myo)

# filter out just the synonyms
syn_good_change <- filter(syn_good, category == "SYN")

# get rid of these from syn good
syn_good <- filter(syn_good, category == "CULVAR" | category == "GC" | category == "H" | category == "HC" | category == "SP" | category == "SSP")

# make the entity and scinames the same
syn_good_change$entity <- syn_good_change$scientificNameStd

# change category into species
syn_good_change$category <- "SP"

# species with multiple synonyms and 5 min traits
# Abelia uniflora, Myoporum tenuifolium
multi_syn <- all_entities_short %>%
  filter(scientificNameStd == "Abelia uniflora" | category == "SYN" & scientificNameStd == "Myoporum tenuifolium")

multi_syn <- multi_syn %>%
  mutate_if(is.factor, as.character) %>%
  mutate(synonym = if_else(entity == "Abelia engleriana", "Abelia engleriana, Abelia schumannii", synonym),
         synonym = if_else(entity == "Myoporum acuminatum", "Myoporum acuminatum, Myoporum montanum", synonym))

multi_syn <- multi_syn %>%
  filter(entity != "Abelia schumannii" & entity != "Myoporum montanum")

# make sciname and entity name the same
multi_syn$entity <- multi_syn$scientificNameStd
# make syn into sp
multi_syn$category <- "SP"

# species with 5 min traits for species and synonyms
sp_syn <- all_entities_short %>%
  filter(scientificNameStd == "Pittosporum phillyraeoides" | scientificNameStd == "Bauhinia variegata" | scientificNameStd == "Coronidium scorpioides" | scientificNameStd == "Cupressus arizonica" 
         | scientificNameStd == "Eucalyptus leucoxylon" | scientificNameStd == "Ficinia nodosa" | scientificNameStd == "Melaleuca fulgens" 
         | scientificNameStd == "Syringa vulgaris" | scientificNameStd == "Syzygium tierneyanum" | scientificNameStd == "Virgilia oroboides")

# salvage the cultivars
sp_syn_cultivars <- filter(sp_syn, category == "CULVAR")

# remove the cultivars and species
sp_syn <- filter(sp_syn, category != "CULVAR" & category != "SP")

sp_syn$entity <- sp_syn$scientificNameStd
sp_syn$category <- "SP"

# join everything back together
all_entities_short_check <- bind_rows(syn_good, syn_good_change, multi_syn, sp_syn, sp_syn_cultivars)

# check everything is still there
summary_new <- all_entities_short_check %>%
  select(entity, category) %>%
  distinct(entity, category) %>%
  group_by(category) %>%
  summarise(frequency = n()) # seems to all be there

all_entities_short <- all_entities_short_check
all_entities_short <- arrange(all_entities_short, entity, trait_name, value)

check_syn <- all_entities_short %>%
  filter(synonym != "NA") %>%
  distinct(entity) # 166 synonyms which is 168 from original minus the two species I combined

# change entity to plant name
names(all_entities_short)[names(all_entities_short) == 'entity'] <- 'plant_name'

##### remove medicinal and apiary from usage

# first check coverage of traits

species <- all_entities_short %>%
  distinct(plant_name)
# 2624 plant names (2637 - 13 problematic synonyms = 2624!!!)

traits <- all_entities_short %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2624)*100) # all relevant traits still 100%

# remove medicinal from usage

all_entities_short <- all_entities_short %>%
  filter(value != "medicinal")

# check coverage again

traits <- all_entities_short %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2624)*100) # usage still 100%

# check apiary

no_apiary <- all_entities_short %>%
  filter(value != "apiary")

# check coverage again

traits <- no_apiary %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2624)*100) # usage still 100%

# if apiary then add '1' to pollinator

all_entities_short$pollinator <- ifelse(all_entities_short$value == "apiary", 1, 
                                        all_entities_short$pollinator)

# check
check_pollinator <- all_entities_short %>%
  filter(value == "apiary") %>%
  select(plant_name, pollinator, value) # all accounted for

# remove apiary from dataset as a usage
all_entities_short <- all_entities_short %>%
  filter(value != "apiary")

# rearrange columns to make more sense
all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, plantType, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, value,
         bird, insect, lizard, native_mammal, pollinator, biodiversity_value, height_min, height_max, width_min, width_max, shade_value, shade_index, carbon_value, carbon_index)

# need to calculate a shade value
all_entities_short$shade_value <- "NA"

all_entities_short$width_max <- as.numeric(as.character(all_entities_short$width_max))

all_entities_short$shade_value <- ifelse(all_entities_short$plantType == "Tree", pi*all_entities_short$width_max^2,
                                         all_entities_short$shade_value)


############

# FINAL FORMAT OF THE DATABASE

# first check glasshouse species

gh <- all_entities_short %>%
  distinct(plant_name, exp_tested) %>%
  group_by(exp_tested) %>%
  summarise(frequency = n())
# 111 plants from glasshouse, some may be missing...
# remove old drought classifications and add new ones....

# add a Koppen zone column

all_entities_short$Koppen_zone <- ""

all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, Koppen_zone, plantType, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, value,
         bird, insect, lizard, native_mammal, pollinator, biodiversity_value, height_min, height_max, width_min, width_max, shade_value, shade_index, carbon_value, carbon_index)

# change the column names

names(all_entities_short)[names(all_entities_short) == 'plantType'] <- 'growth_form'

# select the traits that we want

all_entities_short <- all_entities_short %>%
  filter(trait_name == "common_name" | trait_name == "flower_colour" | trait_name == "flower_period" | trait_name == "leaf_loss" 
         | trait_name == "light_level" | trait_name == "placement" | trait_name == "usage" | trait_name == "height_max" 
         | trait_name == "height_min" | trait_name == "height_average" | trait_name == "width_max" | trait_name == "width_min" | trait_name == "width_average"
         | trait_name == "soil_type" | trait_name == "soil_pH" | trait_name == "supp_watering" | trait_name == "ideal_conditions" | trait_name == "frost_tolerance" | trait_name == "drought_tolerance" 
         | trait_name == "coastal_tolerance" | trait_name == "habit_canopy" | trait_name == "growth_rate" | trait_name == "foliage_colour" | trait_name == "risk" 
         | trait_name == "weed_status")

# check they are all there
trait_name_check <- all_entities_short %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n())

# create new columns to change the trait_name and value
all_entities_short$trait_name_new <- all_entities_short$trait_name
all_entities_short$value_new <- all_entities_short$value

# rearrange columns to make more sense
all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, Koppen_zone, growth_form, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, trait_name_new, value,
         value_new, bird, insect, lizard, native_mammal, pollinator, biodiversity_value, height_min, height_max, width_min, width_max, shade_value, shade_index, carbon_value, carbon_index)

# COMMON NAME
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "common_name", "common name", trait_name_new))

# make common names capitalised
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(trait_name == "common_name", gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", all_entities_short$value_new, perl=TRUE),
                             all_entities_short$value_new))

# GROWTH FORM
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(growth_form = if_else(growth_form == "Bromeliad", "Shrub", growth_form),
         growth_form = if_else(growth_form == "Bulb", "Herb", growth_form),
         growth_form = if_else(growth_form == "Orchid", "Herb", growth_form),
         growth_form = if_else(growth_form == "Vegetable", "Herb", growth_form),
         growth_form = if_else(growth_form == "Strap-leaved", "Herb", growth_form))

# FLOWER COLOUR
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "flower_colour", "flower colour", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "gold", "yellow", value_new),
         value_new = if_else(value == "golden", "yellow", value_new),
         value_new = if_else(value == "grey", "black", value_new), 
         value_new = if_else(value == "insignificant", "inconspicuous", value_new),
         value_new = if_else(value == "magenta", "pink", value_new),
         value_new = if_else(value == "mauve", "purple", value_new),
         value_new = if_else(value == "not_applicable", "does not flower", value_new),
         value_new = if_else(value == "violet", "purple", value_new))

# FLOWER PERIOD
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "flower_period", "flower period", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "Any_time", "any time", value_new),
         value_new = if_else(value == "not_applicable", "does not flower", value_new))

# make the first letters capital
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "summer", gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", all_entities_short$value_new, perl=TRUE), value_new),
         value_new = if_else(value == "autumn", gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", all_entities_short$value_new, perl=TRUE), value_new),
         value_new = if_else(value == "winter", gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", all_entities_short$value_new, perl=TRUE), value_new),
         value_new = if_else(value == "spring", gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", all_entities_short$value_new, perl=TRUE), value_new))

# PLACEMENT
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "placement", "urban context", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "avenue", "street", value_new),
         value_new = if_else(value == "largegarden", "park", value_new),
         value_new = if_else(value == "powerlines", "under powerlines", value_new))

# USAGE
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "usage", "uses", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "cutflower", "cut flowers", value_new),
         value_new = if_else(value == "cutflowers", "cut flowers", value_new),
         value_new = if_else(value == "cutflowers", "cut flowers", value_new),
         value_new = if_else(value == "edible", "putatively edible", value_new),
         value_new = if_else(value == "erosion", "erosion control", value_new),
         value_new = if_else(value == "featureshrub", "feature", value_new),
         value_new = if_else(value == "featuretree", "feature", value_new),
         value_new = if_else(value == "featuretropical", "feature", value_new),
         value_new = if_else(value == "fire_retardant", "putatively fire retardant", value_new),
         value_new = if_else(value == "groundcover", "ground cover", value_new),
         value_new = if_else(value == "massplanting", "mass planting", value_new),
         value_new = if_else(value == "playgroundfriendly", "playground friendly", value_new),
         value_new = if_else(value == "sensorycolour", "playground friendly", value_new),
         value_new = if_else(value == "sensorytouch", "playground friendly", value_new))

# HEIGHT AND WIDTH
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "height_average", "average height", trait_name_new),
         trait_name_new = if_else(trait_name == "width_average", "average width", trait_name_new),
         trait_name_new = if_else(trait_name == "height_max", "maximum height", trait_name_new),
         trait_name_new = if_else(trait_name == "height_min", "minimum height", trait_name_new),
         trait_name_new = if_else(trait_name == "width_max", "maximum width", trait_name_new),
         trait_name_new = if_else(trait_name == "width_min", "minimum width", trait_name_new))


# SOIL
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "soil_type", "soil type", trait_name_new),
         trait_name_new = if_else(trait_name == "soil_pH", "soil pH", trait_name_new))


# PLANTING AND MAINTENANCE
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "supp_watering", "planting and maintenance", trait_name_new),
         trait_name_new = if_else(trait_name == "ideal_conditions", "planting and maintenance", trait_name_new))


all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "high", "high water needs", value_new),
         value_new = if_else(value == "low", "low water needs", value_new),
         value_new = if_else(value == "medium", "medium water needs", value_new),
         value_new = if_else(value == "none", "low water needs", value_new),
         value_new = if_else(value == "fertile", "fertile soil", value_new),
         value_new = if_else(value == "fertile", "fertile soil", value_new),
         value_new = if_else(value == "lateral_space", "lateral space", value_new),
         value_new = if_else(value == "poorly_drained", "poorly drained soil", value_new),
         value_new = if_else(value == "sheltered", "protected", value_new),
         value_new = if_else(value == "well_drained", "well drained soil", value_new))


# change back the growth rate medium values
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(trait_name == "growth_rate" & value == "medium", "medium", value_new))

# filter out the rest
remove_maintenance <- all_entities_short %>%
  filter(trait_name == "ideal_conditions" & value == "acidic" | trait_name == "ideal_conditions" & value == "alkaline" | 
           trait_name == "ideal_conditions" & value == "clay_soil" | trait_name == "ideal_conditions" & value == "clay_soils" | 
           trait_name == "ideal_conditions" & value == "dry" | trait_name == "ideal_conditions" & value == "flowers_fullsun" | 
           trait_name == "ideal_conditions" & value == "fullshade" | trait_name == "ideal_conditions" & value == "fullsun" | 
           trait_name == "ideal_conditions" & value == "gravelly_soil" | trait_name == "ideal_conditions" & value == "high" | 
           trait_name == "ideal_conditions" & value == "humid" | trait_name == "ideal_conditions" & value == "large_rootspace" | 
           trait_name == "ideal_conditions" & value == "loam_soil" | trait_name == "ideal_conditions" & value == "loamy_soil" | 
           trait_name == "ideal_conditions" & value == "loamy_soils" | trait_name == "ideal_conditions" & value == "natural_pH" | 
           trait_name == "ideal_conditions" & value == "neutral_pH" | trait_name == "ideal_conditions" & value == "partshade" | 
           trait_name == "ideal_conditions" & value == "sandy_soil" | trait_name == "ideal_conditions" & value == "shaded" | 
           trait_name == "ideal_conditions" & value == "variable" | trait_name == "ideal_conditions" & value == "moist")

# remove
all_entities_short <- anti_join(all_entities_short, remove_maintenance)

# LEAF LOSS
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "leaf_loss", "leaf loss", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "semi_deciduous", "semi-deciduous", value_new))

# LIGHT LEVEL
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "light_level", "shade tolerance", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "fullshade", "full shade", value_new),
         value_new = if_else(value == "fullsun", "full sun", value_new),
         value_new = if_else(value == "partshade", "part shade", value_new))


# TOLERANCES
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "frost_tolerance", "frost tolerance", trait_name_new),
         trait_name_new = if_else(trait_name == "drought_tolerance", "drought tolerance", trait_name_new),
         trait_name_new = if_else(trait_name == "drought_tolerance", "drought tolerance", trait_name_new),
         trait_name_new = if_else(trait_name == "coastal_tolerance", "coastal tolerance", trait_name_new))

# GROWTH RATE
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "growth_rate", "growth rate", trait_name_new))

# FOLIAGE COLOUR
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "foliage_colour", "leaf colour", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "bluegreen", "blue-green", value_new),
         value_new = if_else(value == "darkgreen", "green", value_new),
         value_new = if_else(value == "dullgreen", "green", value_new),
         value_new = if_else(value == "glossygreen", "green", value_new),
         value_new = if_else(value == "greygreen", "grey-green", value_new),
         value_new = if_else(value == "lightgreen", "green", value_new),
         value_new = if_else(value == "redpink", "red", value_new),
         value_new = if_else(value == "pinkred", "red", value_new),
         value_new = if_else(value == "silver", "silvery", value_new),
         value_new = if_else(value == "silver_foliage", "silvery", value_new),
         value_new = if_else(value == "silvergreen", "silvery", value_new),
         value_new = if_else(value == "silvergrey", "silvery", value_new),
         value_new = if_else(value == "yellowgreen", "yellow", value_new),
         value_new = if_else(value == "variagations", "variagated", value_new))

# filter out rest
remove_foliage <- all_entities_short %>%
  filter(trait_name == "foliage_colour" & value == "bluegrey" | trait_name == "foliage_colour" & value == "bronze" | 
           trait_name == "foliage_colour" & value == "burgundy" | trait_name == "foliage_colour" & value == "copper" | 
           trait_name == "foliage_colour" & value == "cream" | trait_name == "foliage_colour" & value == "gold" | 
           trait_name == "foliage_colour" & value == "grey" | trait_name == "foliage_colour" & value == "high" |
           trait_name == "foliage_colour" & value == "orange" | trait_name == "foliage_colour" & value == "white")

# remove
all_entities_short <- anti_join(all_entities_short, remove_foliage)

# HABIT CANOPY
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "habit_canopy", "canopy shape", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "columnar", "upright", value_new),
         value_new = if_else(value == "domed", "rounded", value_new),
         value_new = if_else(value == "hedging_possible", "hedging possible", value_new),
         value_new = if_else(value == "narrow", "upright", value_new),
         value_new = if_else(value == "oval", "rounded", value_new),
         value_new = if_else(value == "prostrate", "upright", value_new))

# filter out rest
remove_habit_canopy <- all_entities_short %>%
  filter(trait_name == "habit_canopy" & value == "arborescent" | trait_name == "habit_canopy" & value == "branching" | 
           trait_name == "habit_canopy" & value == "clumping" | trait_name == "habit_canopy" & value == "compact" | 
           trait_name == "habit_canopy" & value == "conical" | trait_name == "habit_canopy" & value == "vase") 

# remove
all_entities_short <- anti_join(all_entities_short, remove_habit_canopy)

# RISK
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "mildallergen", "allergen", value_new),
         value_new = if_else(value == "moderateallergen", "allergen", value_new),
         value_new = if_else(value == "poison", "poisonous or toxic", value_new),
         value_new = if_else(value == "severeallergen", "allergen", value_new))

# filter the rest
remove_risk <- all_entities_short %>%
  filter(trait_name == "risk" & value == "branchdrop" | trait_name == "risk" & value == "disease_prone" | 
           trait_name == "risk" & value == "fruitfall" | trait_name == "risk" & value == "highly_flammable" | 
           trait_name == "risk" & value == "infrastructure_damage" | trait_name == "risk" & value == "litterfall" | 
           trait_name == "risk" & value == "maloderous" | trait_name == "risk" & value == "malodorous" | 
           trait_name == "risk" & value == "parasitic" | trait_name == "risk" & value == "possible_weed" | 
           trait_name == "risk" & value == "sap_fall" | trait_name == "risk" & value == "suckering")

# remove
all_entities_short <- anti_join(all_entities_short, remove_risk)

# WEED
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "weed_status", "weed status", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "act", "Australian Capital Territory", value_new),
         value_new = if_else(value == "nsw", "New South Wales", value_new),
         value_new = if_else(value == "nt", "Northern Territory", value_new),
         value_new = if_else(value == "qld", "Queensland", value_new),
         value_new = if_else(value == "sa", "South Australia", value_new),
         value_new = if_else(value == "tas", "Tasmania", value_new),
         value_new = if_else(value == "wa", "Western Australia", value_new),
         value_new = if_else(value == "vic", "Victoria", value_new))

# filter the rest
remove_weed <- all_entities_short %>%
  filter(trait_name == "weed_status" & value == "potentially_act" | trait_name == "weed_status" & value == "potentially_nsw" | 
           trait_name == "weed_status" & value == "potentially_nt" | trait_name == "weed_status" & value == "potentially_qld" | 
           trait_name == "weed_status" & value == "potentially_sa" | trait_name == "weed_status" & value == "potentially_tas" |
           trait_name == "weed_status" & value == "potentially_vic" | trait_name == "weed_status" & value == "potentially_wa")

# remove
all_entities_short <- anti_join(all_entities_short, remove_weed)

# new trait and value columns
all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, Koppen_zone, growth_form, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name_new, value_new,
         bird, insect, lizard, native_mammal, pollinator, biodiversity_value, height_min, height_max, width_min, width_max, shade_value, shade_index, carbon_value, carbon_index)

# end need to distinct everything
all_entities_short <- all_entities_short %>%
  distinct(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, Koppen_zone, growth_form, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name_new,
           value_new, bird, insect, lizard, native_mammal, pollinator, biodiversity_value, height_min, height_max, width_min, width_max, shade_value, shade_index, carbon_value, carbon_index)

names(all_entities_short)[names(all_entities_short) == 'trait_name_new'] <- 'trait_name'
names(all_entities_short)[names(all_entities_short) == 'value_new'] <- 'value'

# arrange in alphabetical order
all_entities_short <- arrange(all_entities_short, plant_name, trait_name, value)

## add Acmena smithii as synonym for Syzygium smithii
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(synonym = if_else(plant_name == "Syzygium smithii", "Acmena smithii", synonym))

# for gh species, remove the drought tolerance trait

gh_drought <- all_entities_short %>%
  filter(exp_tested == "Y") %>%
  filter(trait_name != "drought tolerance")

# load the drought and heat tolerance data

drought_heat <- read.csv("Master_database_input/name_comparisons.csv")

drought_heat <- select(drought_heat, plant_name, drought_tolerance, dehydration_tolerance, heat_tolerance)

# select the drought tolerance (hort) data

drought_hort <- drought_heat %>%
  select(plant_name, drought_tolerance) %>%
  drop_na(drought_tolerance)
  
drought_hort[] <- lapply(drought_hort, gsub, pattern = "drought tolerant", replacement = "putatively high")
drought_hort[] <- lapply(drought_hort, gsub, pattern = "drought intolerant", replacement = "putatively no")
drought_hort[] <- lapply(drought_hort, gsub, pattern = "intermediate", replacement = "putatively moderate")

drought_hort$trait_name <- "drought tolerance"

names(drought_hort)[names(drought_hort) == 'drought_tolerance'] <- 'value'

# select the species with hort info
hort_drought_species <- select(drought_hort, plant_name)

# extract the other info for jusy these species
other_info <- left_join(hort_drought_species, gh_drought, by = "plant_name")
other_info <- other_info %>%
  select(-trait_name , -value) %>%
  distinct(plant_name, .keep_all = TRUE)

# add drought data
gh_all_traits <- left_join(other_info, drought_hort, by = "plant_name")

# rearrange columns

gh_all_traits <- gh_all_traits %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, Koppen_zone, growth_form, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, value,
         bird, insect, lizard, native_mammal, pollinator, biodiversity_value, height_min, height_max, width_min, width_max, shade_value, shade_index, carbon_value, carbon_index)

# join to other data
gh_all <- bind_rows(gh_drought, gh_all_traits)
gh_all <- arrange(gh_all, plant_name, trait_name, value)

# add the dehydration and heat tolerance data
dehydration_heat <- select(drought_heat, -drought_tolerance)
dehydration_heat[] <- lapply(dehydration_heat, gsub, pattern = "Intermed/Tol", replacement = "intermediate - tolerant")
dehydration_heat[[76,3]] <- "NA"

gh_all <- left_join(gh_all, dehydration_heat, by = "plant_name")

# remove gh species from master database
all_entities_short <- all_entities_short %>%
  filter(exp_tested == "N")

# add the new columns
all_entities_short$dehydration_tolerance <- "NA"
all_entities_short$heat_tolerance <- "NA"

# join datasets
all_entities_short <- bind_rows(all_entities_short, gh_all)

all_entities_short <- arrange(all_entities_short, plant_name, trait_name, value)

## populate the model type column

sdm <- read.csv("Master_database_input/LindaFarzin/diff_ST_march_and_min5traits_good_10_03_2021_Clean.csv", strip.white = TRUE)
# get rid of empty cells with white space
# https://stackoverflow.com/questions/2261079/how-can-i-trim-leading-and-trailing-white-space

sdm_possible <- sdm %>%
  select(SDM.is.possible) %>%
  filter(SDM.is.possible != "")
# out of 1914 species, 1469 have data to do sdm, which means 445 are missing

colnames(sdm_possible) <- "speciesName"

# species with missing sdm
sdm_missing <- sdm %>%
  filter(SDM.is.possible == "") %>%
  select(species_list_ST_1Mar2021.csv)

colnames(sdm_missing) <- "speciesName"
  
# load ale's niche data

niche <- read.csv("Master_database_input/Ale/niche_summaries_5414_species.csv")

niche <- niche %>%
  distinct(speciesName)

# diff between missing sdm and niche list
diff_missing_sdm_niche <- setdiff(sdm_missing, niche)
# 77 species do not have a sdm or niche

# diff between models that do not have sdm or niche data and models that do not have sdm data
diff_niches <- setdiff(sdm_missing, diff_missing_sdm_niche)
# 368 species have niche data but not sdm

# populate model type
diff_niches$model_type <- "niche"
diff_missing_sdm_niche$model_type <- "NA"
sdm_possible$model_type <- "sdm"

# join together
model_type <- bind_rows(diff_niches, diff_missing_sdm_niche, sdm_possible)

# join to database
all_entities_short <- select(all_entities_short, -model_type)
names(model_type)[names(model_type) == 'speciesName'] <- 'plant_name'

all_entities_short <- left_join(all_entities_short, model_type, by = "plant_name")
all_entities_short$model_type[is.na(all_entities_short$model_type)] <- "NA"
all_entities_short$scientificNameStd[is.na(all_entities_short$scientificNameStd)] <- "NA"
all_entities_short$species[is.na(all_entities_short$species)] <- "NA"

# rearrange columns

all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, Koppen_zone, growth_form, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, value,
         bird, insect, lizard, native_mammal, pollinator, biodiversity_value, height_min, height_max, width_min, width_max, shade_value, shade_index, carbon_value, carbon_index, dehydration_tolerance, heat_tolerance)

# check that cultivar synonyms are all accounted for
syn_check <- all_entities_short %>%
  filter(scientificNameStd != plant_name)

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(synonym = if_else(plant_name == "Magnolia champaca", "Michelia champaca", synonym),
         synonym = if_else(plant_name == "Chenopodium candolleanum", "Rhagodia candolleana", synonym),
         synonym = if_else(plant_name == "Cassia artemisioides", "Senna artemisioides", synonym))

write.csv(all_entities_short,"Master_database_output/FINAL/trait_database_ST_FINAL_10.3.2021_vers1.2.csv",row.names=FALSE)

# do some checks

summary_new_new <- all_entities_short_check %>%
  select(entity, category) %>%
  distinct(entity, category) %>%
  group_by(category) %>%
  summarise(frequency = n())
# 2630 entities (added 6 new gh cultivars)

# check gh species

gh_species <- all_entities_short %>%
  filter(exp_tested == "Y") %>%
  select(plant_name) %>%
  distinct(plant_name)

#################################################################################################################################################################

######################################## Version 1.3
####### 'canopy cover' = pi x r^2, internal use only
####### 'shade value' = average of canopy cover and (max width x max height), internal use only 
####### 'shade index' = category for shade index
####### 'carbon value' = max height, internal use only
####### 'carbon index' = category for carbon index

library(tidyverse)

everything <- read.csv("Master_database_input/EVERYTHING_traits_18Mar2021.csv")

everything_gh <- read.csv("Master_database_input/EVERYTHING_gh_18Mar2021.csv")

all_entities <- bind_rows(everything, everything_gh)

all_entities_short <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value) %>%
  distinct(scientificNameStd, species, category, exp_tested, trait_name, value)

check <- distinct(all_entities_short, species) # 2636 species, haven't lost anything

# remove the old height and width dimensions

all_entities_short <- all_entities_short %>%
  filter(trait_name != "max_height", trait_name != "height", trait_name != "min_height", 
         trait_name != "max_width", trait_name != "width", trait_name != "min_width")

check <- distinct(all_entities_short, species) # 2636 species, haven't lost anything

##### new height and width data

measurements <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  filter(trait_name == "max_height" | trait_name == "height" | trait_name == "min_height" | 
           trait_name == "max_width" | trait_name == "width" | trait_name == "min_width") %>%
  select(scientificNameStd, species, category, exp_tested, plantType, trait_name, value)

measurements$trait_name_new <- measurements$trait_name # create a new variable to trait name

measurements <- select(measurements, scientificNameStd, species, category, exp_tested, plantType, trait_name, trait_name_new, value)

measurements$trait_name_new <- ifelse(measurements$trait_name == "max_height", "height", 
                                      measurements$trait_name_new)

measurements$trait_name_new <- ifelse(measurements$trait_name == "min_height", "height", 
                                      measurements$trait_name_new)

measurements$trait_name_new <- ifelse(measurements$trait_name == "max_width", "width", 
                                      measurements$trait_name_new)

measurements$trait_name_new <- ifelse(measurements$trait_name == "min_width", "width", 
                                      measurements$trait_name_new)

glimpse(measurements)

measurements$value <- as.numeric(as.character(measurements$value))

height_width <- measurements %>%
  group_by(scientificNameStd, species, category, exp_tested, plantType, trait_name_new) %>%
  summarise(max = max(value), min = min(value), average = mean(value), range = max - min)

height_width$average <- format(round(height_width$average, 1), nsmall = 2) # make values one decimal place

glimpse(height_width)
height_width$average <- as.numeric(as.character(height_width$average))

filter(height_width, range < 0) # no mistakes

names <- distinct(height_width, species) # 2637, am not missing anything!

# write.csv(height_width, "Master_database_output/final_data/height_width_all_ST_5Mar2021.csv", row.names = FALSE)

height_width <- select(height_width, -range)

# change to long format
# http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/

height_width_long <- height_width %>%
  gather(trait_name, value, max:average)

height_width_long <- height_width_long %>%
  mutate(trait_name_new_new = paste0(trait_name_new, "_", trait_name))

height_width_long <- select(height_width_long, scientificNameStd, species, category, exp_tested, trait_name_new_new, value)  

# remove plant type

height_width_long <- height_width_long[,2:7]

# change the column name
names(height_width_long)[names(height_width_long) == 'trait_name_new_new'] <- 'trait_name'

glimpse(height_width_long)
height_width_long$value <- as.character(height_width_long$value)

# join to master dataset

all_entities_short <- bind_rows(all_entities_short, height_width_long)
all_entities_short <- arrange(all_entities_short, scientificNameStd, species, trait_name, value)

check <- distinct(all_entities_short, species) # 2636 species, haven't lost anything

### do drought classifications

drought <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  filter(trait_name == "drought_tolerance") %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value)

drought_summary <- drought %>%
  group_by(scientificNameStd, species) %>%
  summarise(number_records = n())
# 1743 records

drought["number"] <- 1 # add new column populated by '1'

drought_summary <- drought %>%
  group_by(scientificNameStd, species, category, exp_tested, trait_name, value) %>%
  summarise(number_records = sum(number))

drought_long <- drought_summary %>%
  spread(key = value, value = number_records, fill = 0) %>%
  mutate(total_records = sum(No, Yes)) %>%
  arrange(desc(total_records)) %>%
  group_by(total_records) %>%
  mutate(number_species = n())

# create proportions
drought_long <- drought_long %>%
  mutate(no_proportion = (No/total_records) * 100, 
         yes_protortion = (Yes/total_records) * 100)

# apply the consensus approach
drought_long <- drought_long %>%
  mutate(value = case_when(no_proportion >= 75 ~ "putatively no",
                           yes_protortion >= 75 ~ "putatively high",
                           TRUE ~ "putatively moderate")) %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value)

# remove 'total records'
drought_long <- drought_long[,2:7]

# remove the old drought data

all_entities_short <- all_entities_short %>%
  filter(trait_name != "drought_tolerance")

# join to master dataset
all_entities_short <- bind_rows(all_entities_short, drought_long)
all_entities_short <- arrange(all_entities_short, scientificNameStd, species, trait_name, value)

# do frost classifications

frost <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  filter(trait_name == "frost_tolerance") %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value)

# change light to yes
frost <- frost %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value = if_else(value == "light", "Yes", value))

# make all the values upper case
frost$value <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", frost$value, perl=TRUE)

frost_summary <- frost %>%
  group_by(scientificNameStd, species) %>%
  summarise(number_records = n())
# 1852 records

frost["number"] <- 1 # add new column populated by '1'

frost_summary <- frost %>%
  group_by(scientificNameStd, species, category, exp_tested, trait_name, value) %>%
  summarise(number_records = sum(number))

frost_long <- frost_summary %>%
  spread(key = value, value = number_records, fill = 0) %>%
  mutate(total_records = sum(No, Yes)) %>%
  arrange(desc(total_records)) %>%
  group_by(total_records) %>%
  mutate(number_species = n())

# create proportions
frost_long <- frost_long %>%
  mutate(no_proportion = (No/total_records) * 100, 
         yes_protortion = (Yes/total_records) * 100)

# apply the consensus approach
frost_long <- frost_long %>%
  mutate(value = case_when(no_proportion >= 75 ~ "putatively no",
                           yes_protortion >= 75 ~ "putatively high",
                           TRUE ~ "putatively moderate")) %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value)

# remove 'total records'
frost_long <- frost_long[,2:7]

# remove the old frost data

all_entities_short <- all_entities_short %>%
  filter(trait_name != "frost_tolerance")

# join to master dataset
all_entities_short <- bind_rows(all_entities_short, frost_long)
all_entities_short <- arrange(all_entities_short, scientificNameStd, species, trait_name, value)

# do coastal tolerance classifications

coastal <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  filter(trait_name == "coastal_tolerance") %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value)

# make all the values upper case
coastal$value <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", coastal$value, perl=TRUE)

coastal_summary <- coastal %>%
  group_by(scientificNameStd, species) %>%
  summarise(number_records = n())
# 874 records

coastal["number"] <- 1 # add new column populated by '1'

coastal_summary <- coastal %>%
  group_by(scientificNameStd, species, category, exp_tested, trait_name, value) %>%
  summarise(number_records = sum(number))

coastal_long <- coastal_summary %>%
  spread(key = value, value = number_records, fill = 0) %>%
  mutate(total_records = sum(No, Yes)) %>%
  arrange(desc(total_records)) %>%
  group_by(total_records) %>%
  mutate(number_species = n())

# create proportions
coastal_long <- coastal_long %>%
  mutate(no_proportion = (No/total_records) * 100, 
         yes_protortion = (Yes/total_records) * 100)

# apply the consensus approach
coastal_long <- coastal_long %>%
  mutate(value = case_when(no_proportion >= 75 ~ "putatively no",
                           yes_protortion >= 75 ~ "putatively high",
                           TRUE ~ "putatively moderate")) %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value)

# remove 'total records'
coastal_long <- coastal_long[,2:7]

# remove the old coastal data

all_entities_short <- all_entities_short %>%
  filter(trait_name != "coastal_tolerance")

# join to master dataset
all_entities_short <- bind_rows(all_entities_short, coastal_long)
all_entities_short <- arrange(all_entities_short, scientificNameStd, species, trait_name, value)

##### add back plant type and origin

plant_type_origin <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  select(species, plantType, origin) %>%
  distinct(species, plantType, origin) # 2636 plants

# join to main dataset
all_entities_short <- left_join(all_entities_short, plant_type_origin, by = "species")

all_entities_short <- all_entities_short %>%
  select(scientificNameStd, species, plantType, origin, category, exp_tested, trait_name, value) %>%
  filter(trait_name != "native_exotic") # do not remove 'form' as will need that later

###### separate the species and genus names
# https://stackoverflow.com/questions/4350440/split-data-frame-string-column-into-multiple-columns

# first change species col name
names(all_entities_short)[names(all_entities_short) == 'species'] <- 'entity'

all_entities_short <- all_entities_short %>%
  separate(scientificNameStd, c("genus", "species"), " ", remove = FALSE)

# need to fix GC, H and HC

all_entities_short$genus <- ifelse(all_entities_short$category == "H", word(all_entities_short$entity, 1), 
                                   all_entities_short$genus)
all_entities_short$genus <- ifelse(all_entities_short$category == "GC", word(all_entities_short$entity, 1), 
                                   all_entities_short$genus)
all_entities_short$genus <- ifelse(all_entities_short$category == "HC", word(all_entities_short$entity, 1), 
                                   all_entities_short$genus)

#### attach the family names

## use old list I created which should be the same

family <- read.csv("Master_database_output/taxonomy_checks/taxonstandcheck_ST_1.3.2021.csv")

family <- select(family, Taxon, Family)

names(family)[names(family) == 'Taxon'] <- 'scientificNameStd'
names(family)[names(family) == 'Family'] <- 'family'

all_entities_short <- left_join(all_entities_short, family, by = "scientificNameStd")

all_entities_short <- select(all_entities_short, scientificNameStd, family, genus, species, entity, plantType, origin, category, exp_tested, trait_name, value)
# family is blank for H, GC, HC

#### add synonym column

all_entities_short$synonym <- "NA"

all_entities_short$synonym <- ifelse(all_entities_short$category == "SYN", all_entities_short$entity, 
                                     all_entities_short$synonym)

all_entities_short <- select(all_entities_short, scientificNameStd, family, genus, species, entity, synonym, plantType, origin, category, exp_tested, trait_name, value)

####### extract the ecological services data

eco <- all_entities_short %>%
  filter(trait_name == "ecological_services") %>%
  select(entity, trait_name, value)

eco$score <- "1"

# change from long to wide

eco_wide <- eco %>%
  spread(value, score, fill = 0) 

glimpse(eco_wide) 

eco_wide$bird <- as.numeric(as.character(eco_wide$bird))
eco_wide$insect <- as.numeric(as.character(eco_wide$insect))
eco_wide$lizard <- as.numeric(as.character(eco_wide$lizard))
eco_wide$native_mammal <- as.numeric(as.character(eco_wide$native_mammal))
eco_wide$pollinator <- as.numeric(as.character(eco_wide$pollinator))  

eco_wide <- eco_wide %>%
  mutate(biodiversity_score = bird + insect + lizard + native_mammal + pollinator) %>%
  select(-trait_name)

# join to main database
all_entities_short <- left_join(all_entities_short, eco_wide, by = "entity")

# replace the NAs with 0

all_entities_short$bird[is.na(all_entities_short$bird)] <- 0
all_entities_short$insect[is.na(all_entities_short$insect)] <- 0
all_entities_short$lizard[is.na(all_entities_short$lizard)] <- 0
all_entities_short$native_mammal[is.na(all_entities_short$native_mammal)] <- 0
all_entities_short$pollinator[is.na(all_entities_short$pollinator)] <- 0
all_entities_short$biodiversity_score[is.na(all_entities_short$biodiversity_score)] <- 0

# remove the ecological services trait
all_entities_short <- all_entities_short %>%
  filter(trait_name != "ecological_services")

##### extract the plant form data

form <- all_entities_short %>%
  filter(trait_name == "form") %>%
  select(entity, trait_name, value)

# fix up
form <- form %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value = if_else(value == "aquatic", "herb", value),
         value = if_else(value == "bromeliad", "herb", value),
         value = if_else(value == "bulb", "herb", value),
         value = if_else(value == "vegetable", "herb", value),
         value = if_else(value == "orchid", "herb", value),
         value = if_else(value == "strap-leaved", "herb", value))

form <- distinct(form, entity, trait_name, value)

form$score <- "1"

# change from long to wide

form_wide <- form %>%
  spread(value, score, fill = 0) 

form_wide <- select(form_wide , -trait_name)

# join to main database
all_entities_short <- left_join(all_entities_short, form_wide, by = "entity")

# remove the form trait
all_entities_short <- all_entities_short %>%
  filter(trait_name != "form")

# add the dummy columns for the biodiversity benefits and model type

all_entities_short$model_type <- ""
all_entities_short$canopy_cover <- ""
all_entities_short$shade_value <- ""
all_entities_short$shade_index <- ""
all_entities_short$carbon_value <- ""
all_entities_short$carbon_index <-""

names(all_entities_short)[names(all_entities_short) == 'biodiversity_score'] <- 'biodiversity_value'

# rearrange all the columns
all_entities_short <- select(all_entities_short, scientificNameStd, family, genus, species, entity, synonym, model_type, plantType, climber, 
                             cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, category, exp_tested, trait_name, value,
                             bird, insect, lizard, native_mammal, pollinator, biodiversity_value, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)

#### add the max height and width for shade and carbon values

height_width2 <- select(height_width, -average)

# change from wide to long
# http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/

height_width_long2 <- height_width2 %>%
  gather(trait_name, value, max:min)

height_width_long2 <- height_width_long2 %>%
  mutate(trait_name_new_new = paste0(trait_name_new, "_", trait_name))

height_width_long2 <- select(height_width_long2, -trait_name, -trait_name_new)

# change from long to wide
height_width_wide <- height_width_long2 %>%
  spread(trait_name_new_new, value)

height_width_wide <- select(height_width_wide, -scientificNameStd, -category, -plantType, -exp_tested)

# remove these columns
height_width_wide <- height_width_wide[, 5:9]

names(height_width_wide)[names(height_width_wide) == 'species'] <- 'entity'

# join to main dataset
all_entities_short <- left_join(all_entities_short, height_width_wide, by = "entity")

# rearrange all the columns
all_entities_short <- select(all_entities_short, scientificNameStd, family, genus, species, entity, synonym, model_type, plantType, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, category, exp_tested, trait_name, value,
                             bird, insect, lizard, native_mammal, pollinator, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)


##### fix up family names for H, HC, GC

family_fix <- all_entities_short %>%
  filter(category == "H"| category == "HC"| category == "GC") %>%
  select(family, genus, species, entity, category) %>%
  distinct(family, genus, species, entity, category)

genus_family <- all_entities_short %>%
  filter(category == "SP") %>%
  select(family, genus)

# check no genus is in two families
sum <- genus_family %>%
  distinct(family, genus) %>%
  group_by(family, genus) %>%
  summarise(frequency = n()) # don't think so
# but there are missing families for Cephalotaxus and Chenopodium

# https://stackoverflow.com/questions/51398814/r-if-else-with-multiple-conditions-for-character-vectors-with-nas
### THIS WILL BE IMPORTANT WHEN CHANGING TRAIT LEVELS!!!!

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(family = if_else(genus == "Cephalotaxus", "Taxaceae", family))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(family = if_else(genus == "Chenopodium", "Amaranthaceae", family))

# check if fixed
genus_family <- all_entities_short %>%
  filter(category == "SP") %>%
  select(family, genus)

sum <- genus_family %>%
  distinct(family, genus) %>%
  group_by(family, genus) %>%
  summarise(frequency = n()) # fixed!

# filter out H, HC and GC from database
missing <- all_entities_short %>%
  filter(category == "H"| category == "HC"| category == "GC") %>%
  select(-family)

found <- left_join(missing, sum, by = "genus")

# some missing families, Michelia, Rheum

found <- found %>%
  mutate_if(is.factor, as.character) %>%
  mutate(family = if_else(genus == "Michelia", "Magnoliaceae", family))

found <- found %>%
  mutate_if(is.factor, as.character) %>%
  mutate(family = if_else(genus == "Rheum", "Polygonaceae", family))

## add parents of Gc, H, HC

parents <- read.csv("Master_database_input/hybrids_genus_cultivars_parents.csv")

parents <- select(parents, -plantType)

names(parents)[names(parents) == 'Species'] <- 'entity'

# join
found <- left_join(found, parents, by = "entity")

# rearrange the columns

found <- found %>%
  select(-frequency) %>%
  select(scientificNameStd, family, genus, species, entity, synonym, Parent_1, Parent_2,Parent_3, Parent_4, model_type, plantType, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, category, exp_tested, trait_name, value,
         bird, insect, lizard, native_mammal, pollinator, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)

# replace the blank parent cells with NA

found <- found %>%
  mutate_if(is.factor, as.character) %>%
  mutate(Parent_2 = if_else(Parent_2 == "", "NA", Parent_2))

found <- found %>%
  mutate_if(is.factor, as.character) %>%
  mutate(Parent_3 = if_else(Parent_3 == "", "NA", Parent_3))

found <- found %>%
  mutate_if(is.factor, as.character) %>%
  mutate(Parent_4 = if_else(Parent_4 == "", "NA", Parent_4))

# remove H, HC, Gc from database

all_entities_short <- all_entities_short %>%
  filter(category != "H" & category != "GC" & category != "HC")

# add parent columns to database

all_entities_short$Parent_1 <- "NA"
all_entities_short$Parent_2 <- "NA"
all_entities_short$Parent_3 <- "NA"
all_entities_short$Parent_4 <- "NA"

# rearrange columns

all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, entity, synonym, Parent_1, Parent_2,Parent_3, Parent_4, model_type, plantType, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, category, exp_tested, trait_name, value,
         bird, insect, lizard, native_mammal, pollinator, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)

# join back the H, HC, GCs

all_entities_short <- bind_rows(all_entities_short, found)

all_entities_short <- arrange(all_entities_short, entity, trait_name, value)

# some synonyms with missing family
syn <- all_entities_short %>%
  filter(family == "") %>%
  select(entity) %>%
  distinct(entity) #17

# find and fix

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(family = if_else(entity == "Abutilon megapotamicum", "Malvaceae", family), 
         family = if_else(entity == "Aloe variegata", "Asphodelaceae", family),
         family = if_else(entity == "Anemone hupehensis", "Ranunculaceae", family),
         family = if_else(entity == "Anemone hupehensis japonica", "Ranunculaceae", family),
         family = if_else(entity == "Anemone tomentosa", "Ranunculaceae", family),
         family = if_else(entity == "Babingtonia behrii", "Myrtaceae", family),
         family = if_else(entity == "Baeckea crassifolia", "Myrtaceae", family),
         family = if_else(entity == "Beaufortia sparsa", "Myrtaceae", family),
         family = if_else(entity == "Beaufortia squarrosa", "Myrtaceae", family),
         family = if_else(entity == "Calothamnus quadrifidus", "Myrtaceae", family),
         family = if_else(entity == "Calothamnus sanguineus", "Myrtaceae", family),
         family = if_else(entity == "Calothamnus villosus", "Myrtaceae", family),
         family = if_else(entity == "Eremaea beaufortioides", "Myrtaceae", family),
         family = if_else(entity == "Jasminum fruticans", "Oleaceae", family),
         family = if_else(entity == "Leucopogon parviflorus", "Ericaceae", family),
         family = if_else(entity == "Quisqualis indica", "Myrtaceae", family),
         family = if_else(entity == "Regelia velutina", "Myrtaceae", family))

# check

syn_check <- filter(all_entities_short, family == "") # all fixed

# filter out cultivars
# check if sciename = entity
# check if they are all synonyms

same <- all_entities_short %>%
  filter(category != "CULVAR") %>%
  filter(scientificNameStd != entity) %>%
  select(entity, category) %>%
  distinct(entity, category)

# fix the species
all_entities_short[] <-lapply(all_entities_short, gsub, pattern = "Magnolia Iiliiflora", replacement = "Magnolia liliiflora")
all_entities_short[] <-lapply(all_entities_short, gsub, pattern = "Pittosporum phylliraeoides", replacement = "Pittosporum phillyraeoides")

# remove the category 'synonym'

# these are the problematic ones
# Species with multiple synonyms that have 5 min traits: Abelia uniflora, Myoporum tenuifolium

# Species and synonyms that have 5 min traits: Bauhinia variegata, Coronidium scorpioides,
# Cupressus arizonica, Eucalyptus leucoxylon, Ficinia nodosa, Melaleuca fulgens,
# Syringa vulgaris, Syzygium tierneyanum, Virgilia oroboides, Pittosporum phillyraeoides

# summary of what we have so far

summary_original <- all_entities_short %>%
  select(entity, category) %>%
  distinct(entity, category) %>%
  group_by(category) %>%
  summarise(frequency = n())

# exclude the problematic ones

syn_good <- all_entities_short %>%
  filter(scientificNameStd != "Abelia uniflora" & scientificNameStd != "Myoporum tenuifolium" & scientificNameStd != "Pittosporum phillyraeoides" 
         & scientificNameStd != "Bauhinia variegata" & scientificNameStd != "Coronidium scorpioides" & scientificNameStd != "Cupressus arizonica" 
         & scientificNameStd != "Eucalyptus leucoxylon" & scientificNameStd != "Ficinia nodosa" & scientificNameStd != "Melaleuca fulgens" 
         & scientificNameStd != "Syringa vulgaris" & scientificNameStd != "Syzygium tierneyanum" & scientificNameStd != "Virgilia oroboides")

# add back the GC, H, HC
others <- all_entities_short %>%
  filter(category == "H" | category == "HC" | category == "GC")

# and also Myoporum montanum Arid form
myo <- all_entities_short %>%
  filter(entity == "Myoporum montanum Arid Form")

syn_good <- bind_rows(syn_good, others, myo)

# filter out just the synonyms
syn_good_change <- filter(syn_good, category == "SYN")

# get rid of these from syn good
syn_good <- filter(syn_good, category == "CULVAR" | category == "GC" | category == "H" | category == "HC" | category == "SP" | category == "SSP")

# make the entity and scinames the same
syn_good_change$entity <- syn_good_change$scientificNameStd

# change category into species
syn_good_change$category <- "SP"

# species with multiple synonyms and 5 min traits
# Abelia uniflora, Myoporum tenuifolium
multi_syn <- all_entities_short %>%
  filter(scientificNameStd == "Abelia uniflora" | category == "SYN" & scientificNameStd == "Myoporum tenuifolium")

multi_syn <- multi_syn %>%
  mutate_if(is.factor, as.character) %>%
  mutate(synonym = if_else(entity == "Abelia engleriana", "Abelia engleriana, Abelia schumannii", synonym),
         synonym = if_else(entity == "Myoporum acuminatum", "Myoporum acuminatum, Myoporum montanum", synonym))

multi_syn <- multi_syn %>%
  filter(entity != "Abelia schumannii" & entity != "Myoporum montanum")

# make sciname and entity name the same
multi_syn$entity <- multi_syn$scientificNameStd
# make syn into sp
multi_syn$category <- "SP"

# species with 5 min traits for species and synonyms
sp_syn <- all_entities_short %>%
  filter(scientificNameStd == "Pittosporum phillyraeoides" | scientificNameStd == "Bauhinia variegata" | scientificNameStd == "Coronidium scorpioides" | scientificNameStd == "Cupressus arizonica" 
         | scientificNameStd == "Eucalyptus leucoxylon" | scientificNameStd == "Ficinia nodosa" | scientificNameStd == "Melaleuca fulgens" 
         | scientificNameStd == "Syringa vulgaris" | scientificNameStd == "Syzygium tierneyanum" | scientificNameStd == "Virgilia oroboides")

# salvage the cultivars
sp_syn_cultivars <- filter(sp_syn, category == "CULVAR")

# remove the cultivars and species
sp_syn <- filter(sp_syn, category != "CULVAR" & category != "SP")

sp_syn$entity <- sp_syn$scientificNameStd
sp_syn$category <- "SP"

# join everything back together
all_entities_short_check <- bind_rows(syn_good, syn_good_change, multi_syn, sp_syn, sp_syn_cultivars)

# check everything is still there
summary_new <- all_entities_short_check %>%
  select(entity, category) %>%
  distinct(entity, category) %>%
  group_by(category) %>%
  summarise(frequency = n()) # seems to all be there

all_entities_short <- all_entities_short_check
all_entities_short <- arrange(all_entities_short, entity, trait_name, value)

check_syn <- all_entities_short %>%
  filter(synonym != "NA") %>%
  distinct(entity) # 166 synonyms which is 168 from original minus the two species I combined

# change entity to plant name
names(all_entities_short)[names(all_entities_short) == 'entity'] <- 'plant_name'

##### remove medicinal and apiary from usage

# first check coverage of traits

species <- all_entities_short %>%
  distinct(plant_name)
# 2624 plant names (2637 - 13 problematic synonyms = 2624!!!)

traits <- all_entities_short %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2624)*100) # all relevant traits still 100%

# remove medicinal from usage

all_entities_short <- all_entities_short %>%
  filter(value != "medicinal")

# check coverage again

traits <- all_entities_short %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2624)*100) # usage still 100%

# check apiary

no_apiary <- all_entities_short %>%
  filter(value != "apiary")

# check coverage again

traits <- no_apiary %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2624)*100) # usage still 100%

# if apiary then add '1' to pollinator

all_entities_short$pollinator <- ifelse(all_entities_short$value == "apiary", 1, 
                                        all_entities_short$pollinator)

# check
check_pollinator <- all_entities_short %>%
  filter(value == "apiary") %>%
  select(plant_name, pollinator, value) # all accounted for

# remove apiary from dataset as a usage
all_entities_short <- all_entities_short %>%
  filter(value != "apiary")

# rearrange columns to make more sense
all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, plantType, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, value,
         bird, insect, lizard, native_mammal, pollinator, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)

# need to calculate co-benefits
all_entities_short$width_max <- as.numeric(as.character(all_entities_short$width_max))
all_entities_short$width_min <- as.numeric(as.character(all_entities_short$width_min))
all_entities_short$height_max <- as.numeric(as.character(all_entities_short$height_max))
all_entities_short$height_min <- as.numeric(as.character(all_entities_short$height_min))
all_entities_short$tree <- as.numeric(as.character(all_entities_short$tree))
all_entities_short$shrub <- as.numeric(as.character(all_entities_short$shrub))

# canopy cover
all_entities_short$canopy_cover <- "NA"

all_entities_short$canopy_cover <- ifelse(all_entities_short$tree == 1, pi*(all_entities_short$width_max/2)^2,
                                         all_entities_short$canopy_cover)

# shade value
all_entities_short$shade_value <- "NA"

all_entities_short$canopy_cover <- as.numeric(as.character(all_entities_short$canopy_cover))

all_entities_short$shade_value <- ifelse(all_entities_short$tree == 1, ((all_entities_short$canopy_cover + (all_entities_short$width_max*all_entities_short$height_max))/2),
                                         all_entities_short$shade_value)

all_entities_short$shade_value <- as.numeric(as.character(all_entities_short$shade_value))

# carbon value
all_entities_short$carbon_value <- "NA"

all_entities_short$carbon_value <- ifelse(all_entities_short$tree == 1, all_entities_short$height_max, 
                                          all_entities_short$carbon_value)

all_entities_short$carbon_value <- as.numeric(as.character(all_entities_short$carbon_value))

############

# FINAL FORMAT OF THE DATABASE

# first check glasshouse species

gh <- all_entities_short %>%
  distinct(plant_name, exp_tested) %>%
  group_by(exp_tested) %>%
  summarise(frequency = n())
# 111 plants from glasshouse, some may be missing...
# remove old drought classifications and add new ones....

# add a Koppen zone column

all_entities_short$Koppen_zone <- ""

all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, Koppen_zone, plantType, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, value,
         bird, insect, lizard, native_mammal, pollinator, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)

# change the column names

names(all_entities_short)[names(all_entities_short) == 'plantType'] <- 'growth_form'

# select the traits that we want

all_entities_short <- all_entities_short %>%
  filter(trait_name == "common_name" | trait_name == "flower_colour" | trait_name == "flower_period" | trait_name == "leaf_loss" 
         | trait_name == "light_level" | trait_name == "placement" | trait_name == "usage" | trait_name == "height_max" 
         | trait_name == "height_min" | trait_name == "height_average" | trait_name == "width_max" | trait_name == "width_min" | trait_name == "width_average"
         | trait_name == "soil_type" | trait_name == "soil_pH" | trait_name == "supp_watering" | trait_name == "ideal_conditions" | trait_name == "frost_tolerance" | trait_name == "drought_tolerance" 
         | trait_name == "coastal_tolerance" | trait_name == "habit_canopy" | trait_name == "growth_rate" | trait_name == "foliage_colour" | trait_name == "risk" 
         | trait_name == "weed_status")

# check they are all there
trait_name_check <- all_entities_short %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n())

# create new columns to change the trait_name and value
all_entities_short$trait_name_new <- all_entities_short$trait_name
all_entities_short$value_new <- all_entities_short$value

# rearrange columns to make more sense
all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, Koppen_zone, growth_form, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, trait_name_new, value,
         value_new, bird, insect, lizard, native_mammal, pollinator, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)

# COMMON NAME
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "common_name", "common name", trait_name_new))

# make common names capitalised
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(trait_name == "common_name", gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", all_entities_short$value_new, perl=TRUE),
                             all_entities_short$value_new))

# GROWTH FORM
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(growth_form = if_else(growth_form == "Bromeliad", "Shrub", growth_form),
         growth_form = if_else(growth_form == "Bulb", "Herb", growth_form),
         growth_form = if_else(growth_form == "Orchid", "Herb", growth_form),
         growth_form = if_else(growth_form == "Vegetable", "Herb", growth_form),
         growth_form = if_else(growth_form == "Strap-leaved", "Herb", growth_form))

# FLOWER COLOUR
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "flower_colour", "flower colour", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "gold", "yellow", value_new),
         value_new = if_else(value == "golden", "yellow", value_new),
         value_new = if_else(value == "grey", "black", value_new), 
         value_new = if_else(value == "insignificant", "inconspicuous", value_new),
         value_new = if_else(value == "magenta", "pink", value_new),
         value_new = if_else(value == "mauve", "purple", value_new),
         value_new = if_else(value == "not_applicable", "does not flower", value_new),
         value_new = if_else(value == "violet", "purple", value_new))

# FLOWER PERIOD
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "flower_period", "flower period", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "Any_time", "any time", value_new),
         value_new = if_else(value == "not_applicable", "does not flower", value_new))

# make the first letters capital
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "summer", gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", all_entities_short$value_new, perl=TRUE), value_new),
         value_new = if_else(value == "autumn", gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", all_entities_short$value_new, perl=TRUE), value_new),
         value_new = if_else(value == "winter", gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", all_entities_short$value_new, perl=TRUE), value_new),
         value_new = if_else(value == "spring", gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", all_entities_short$value_new, perl=TRUE), value_new))

# PLACEMENT
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "placement", "urban context", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "avenue", "street", value_new),
         value_new = if_else(value == "largegarden", "park", value_new),
         value_new = if_else(value == "powerlines", "under powerlines", value_new))

# USAGE
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "usage", "uses", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "cutflower", "cut flowers", value_new),
         value_new = if_else(value == "cutflowers", "cut flowers", value_new),
         value_new = if_else(value == "cutflowers", "cut flowers", value_new),
         value_new = if_else(value == "edible", "putatively edible", value_new),
         value_new = if_else(value == "erosion", "erosion control", value_new),
         value_new = if_else(value == "featureshrub", "feature", value_new),
         value_new = if_else(value == "featuretree", "feature", value_new),
         value_new = if_else(value == "featuretropical", "feature", value_new),
         value_new = if_else(value == "fire_retardant", "putatively fire retardant", value_new),
         value_new = if_else(value == "groundcover", "ground cover", value_new),
         value_new = if_else(value == "massplanting", "mass planting", value_new),
         value_new = if_else(value == "playgroundfriendly", "playground friendly", value_new),
         value_new = if_else(value == "sensorycolour", "playground friendly", value_new),
         value_new = if_else(value == "sensorytouch", "playground friendly", value_new))

# HEIGHT AND WIDTH
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "height_average", "average height", trait_name_new),
         trait_name_new = if_else(trait_name == "width_average", "average width", trait_name_new),
         trait_name_new = if_else(trait_name == "height_max", "maximum height", trait_name_new),
         trait_name_new = if_else(trait_name == "height_min", "minimum height", trait_name_new),
         trait_name_new = if_else(trait_name == "width_max", "maximum width", trait_name_new),
         trait_name_new = if_else(trait_name == "width_min", "minimum width", trait_name_new))


# SOIL
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "soil_type", "soil type", trait_name_new),
         trait_name_new = if_else(trait_name == "soil_pH", "soil pH", trait_name_new))


# PLANTING AND MAINTENANCE
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "supp_watering", "planting and maintenance", trait_name_new),
         trait_name_new = if_else(trait_name == "ideal_conditions", "planting and maintenance", trait_name_new))


all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "high", "high water needs", value_new),
         value_new = if_else(value == "low", "low water needs", value_new),
         value_new = if_else(value == "medium", "medium water needs", value_new),
         value_new = if_else(value == "none", "low water needs", value_new),
         value_new = if_else(value == "fertile", "fertile soil", value_new),
         value_new = if_else(value == "fertile", "fertile soil", value_new),
         value_new = if_else(value == "lateral_space", "lateral space", value_new),
         value_new = if_else(value == "poorly_drained", "poorly drained soil", value_new),
         value_new = if_else(value == "sheltered", "protected", value_new),
         value_new = if_else(value == "well_drained", "well drained soil", value_new))


# change back the growth rate medium values
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(trait_name == "growth_rate" & value == "medium", "medium", value_new))

# filter out the rest
remove_maintenance <- all_entities_short %>%
  filter(trait_name == "ideal_conditions" & value == "acidic" | trait_name == "ideal_conditions" & value == "alkaline" | 
           trait_name == "ideal_conditions" & value == "clay_soil" | trait_name == "ideal_conditions" & value == "clay_soils" | 
           trait_name == "ideal_conditions" & value == "dry" | trait_name == "ideal_conditions" & value == "flowers_fullsun" | 
           trait_name == "ideal_conditions" & value == "fullshade" | trait_name == "ideal_conditions" & value == "fullsun" | 
           trait_name == "ideal_conditions" & value == "gravelly_soil" | trait_name == "ideal_conditions" & value == "high" | 
           trait_name == "ideal_conditions" & value == "humid" | trait_name == "ideal_conditions" & value == "large_rootspace" | 
           trait_name == "ideal_conditions" & value == "loam_soil" | trait_name == "ideal_conditions" & value == "loamy_soil" | 
           trait_name == "ideal_conditions" & value == "loamy_soils" | trait_name == "ideal_conditions" & value == "natural_pH" | 
           trait_name == "ideal_conditions" & value == "neutral_pH" | trait_name == "ideal_conditions" & value == "partshade" | 
           trait_name == "ideal_conditions" & value == "sandy_soil" | trait_name == "ideal_conditions" & value == "shaded" | 
           trait_name == "ideal_conditions" & value == "variable" | trait_name == "ideal_conditions" & value == "moist")

# remove
all_entities_short <- anti_join(all_entities_short, remove_maintenance)

# LEAF LOSS
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "leaf_loss", "leaf loss", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "semi_deciduous", "semi-deciduous", value_new))

# LIGHT LEVEL
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "light_level", "shade tolerance", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "fullshade", "full shade", value_new),
         value_new = if_else(value == "fullsun", "full sun", value_new),
         value_new = if_else(value == "partshade", "part shade", value_new))


# TOLERANCES
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "frost_tolerance", "frost tolerance", trait_name_new),
         trait_name_new = if_else(trait_name == "drought_tolerance", "drought tolerance", trait_name_new),
         trait_name_new = if_else(trait_name == "drought_tolerance", "drought tolerance", trait_name_new),
         trait_name_new = if_else(trait_name == "coastal_tolerance", "coastal tolerance", trait_name_new))

# GROWTH RATE
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "growth_rate", "growth rate", trait_name_new))

# FOLIAGE COLOUR
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "foliage_colour", "leaf colour", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "bluegreen", "blue-green", value_new),
         value_new = if_else(value == "darkgreen", "green", value_new),
         value_new = if_else(value == "dullgreen", "green", value_new),
         value_new = if_else(value == "glossygreen", "green", value_new),
         value_new = if_else(value == "greygreen", "grey-green", value_new),
         value_new = if_else(value == "lightgreen", "green", value_new),
         value_new = if_else(value == "redpink", "red", value_new),
         value_new = if_else(value == "pinkred", "red", value_new),
         value_new = if_else(value == "silver", "silvery", value_new),
         value_new = if_else(value == "silver_foliage", "silvery", value_new),
         value_new = if_else(value == "silvergreen", "silvery", value_new),
         value_new = if_else(value == "silvergrey", "silvery", value_new),
         value_new = if_else(value == "yellowgreen", "yellow", value_new),
         value_new = if_else(value == "variagations", "variagated", value_new))

# filter out rest
remove_foliage <- all_entities_short %>%
  filter(trait_name == "foliage_colour" & value == "bluegrey" | trait_name == "foliage_colour" & value == "bronze" | 
           trait_name == "foliage_colour" & value == "burgundy" | trait_name == "foliage_colour" & value == "copper" | 
           trait_name == "foliage_colour" & value == "cream" | trait_name == "foliage_colour" & value == "gold" | 
           trait_name == "foliage_colour" & value == "grey" | trait_name == "foliage_colour" & value == "high" |
           trait_name == "foliage_colour" & value == "orange" | trait_name == "foliage_colour" & value == "white")

# remove
all_entities_short <- anti_join(all_entities_short, remove_foliage)

# HABIT CANOPY
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "habit_canopy", "canopy shape", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "columnar", "upright", value_new),
         value_new = if_else(value == "domed", "rounded", value_new),
         value_new = if_else(value == "hedging_possible", "hedging possible", value_new),
         value_new = if_else(value == "narrow", "upright", value_new),
         value_new = if_else(value == "oval", "rounded", value_new),
         value_new = if_else(value == "prostrate", "upright", value_new))

# filter out rest
remove_habit_canopy <- all_entities_short %>%
  filter(trait_name == "habit_canopy" & value == "arborescent" | trait_name == "habit_canopy" & value == "branching" | 
           trait_name == "habit_canopy" & value == "clumping" | trait_name == "habit_canopy" & value == "compact" | 
           trait_name == "habit_canopy" & value == "conical" | trait_name == "habit_canopy" & value == "vase") 

# remove
all_entities_short <- anti_join(all_entities_short, remove_habit_canopy)

# RISK
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "mildallergen", "allergen", value_new),
         value_new = if_else(value == "moderateallergen", "allergen", value_new),
         value_new = if_else(value == "poison", "poisonous or toxic", value_new),
         value_new = if_else(value == "severeallergen", "allergen", value_new))

# filter the rest
remove_risk <- all_entities_short %>%
  filter(trait_name == "risk" & value == "branchdrop" | trait_name == "risk" & value == "disease_prone" | 
           trait_name == "risk" & value == "fruitfall" | trait_name == "risk" & value == "highly_flammable" | 
           trait_name == "risk" & value == "infrastructure_damage" | trait_name == "risk" & value == "litterfall" | 
           trait_name == "risk" & value == "maloderous" | trait_name == "risk" & value == "malodorous" | 
           trait_name == "risk" & value == "parasitic" | trait_name == "risk" & value == "possible_weed" | 
           trait_name == "risk" & value == "sap_fall" | trait_name == "risk" & value == "suckering")

# remove
all_entities_short <- anti_join(all_entities_short, remove_risk)

# WEED
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "weed_status", "weed status", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "act", "Australian Capital Territory", value_new),
         value_new = if_else(value == "nsw", "New South Wales", value_new),
         value_new = if_else(value == "nt", "Northern Territory", value_new),
         value_new = if_else(value == "qld", "Queensland", value_new),
         value_new = if_else(value == "sa", "South Australia", value_new),
         value_new = if_else(value == "tas", "Tasmania", value_new),
         value_new = if_else(value == "wa", "Western Australia", value_new),
         value_new = if_else(value == "vic", "Victoria", value_new))

# filter the rest
remove_weed <- all_entities_short %>%
  filter(trait_name == "weed_status" & value == "potentially_act" | trait_name == "weed_status" & value == "potentially_nsw" | 
           trait_name == "weed_status" & value == "potentially_nt" | trait_name == "weed_status" & value == "potentially_qld" | 
           trait_name == "weed_status" & value == "potentially_sa" | trait_name == "weed_status" & value == "potentially_tas" |
           trait_name == "weed_status" & value == "potentially_vic" | trait_name == "weed_status" & value == "potentially_wa")

# remove
all_entities_short <- anti_join(all_entities_short, remove_weed)

# new trait and value columns
all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, Koppen_zone, growth_form, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name_new, value_new,
         bird, insect, lizard, native_mammal, pollinator, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)

# end need to distinct everything
all_entities_short <- all_entities_short %>%
  distinct(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, Koppen_zone, growth_form, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name_new,
           value_new, bird, insect, lizard, native_mammal, pollinator, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)

names(all_entities_short)[names(all_entities_short) == 'trait_name_new'] <- 'trait_name'
names(all_entities_short)[names(all_entities_short) == 'value_new'] <- 'value'

# arrange in alphabetical order
all_entities_short <- arrange(all_entities_short, plant_name, trait_name, value)

## add Acmena smithii as synonym for Syzygium smithii
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(synonym = if_else(plant_name == "Syzygium smithii", "Acmena smithii", synonym))

# for gh species, remove the drought tolerance trait

gh_drought <- all_entities_short %>%
  filter(exp_tested == "Y") %>%
  filter(trait_name != "drought tolerance")

# load the drought and heat tolerance data

drought_heat <- read.csv("Master_database_input/name_comparisons.csv")

drought_heat <- select(drought_heat, plant_name, drought_tolerance, dehydration_tolerance, heat_tolerance)

# select the drought tolerance (hort) data

drought_hort <- drought_heat %>%
  select(plant_name, drought_tolerance) %>%
  drop_na(drought_tolerance)

drought_hort[] <- lapply(drought_hort, gsub, pattern = "drought tolerant", replacement = "putatively high")
drought_hort[] <- lapply(drought_hort, gsub, pattern = "drought intolerant", replacement = "putatively no")
drought_hort[] <- lapply(drought_hort, gsub, pattern = "intermediate", replacement = "putatively moderate")

drought_hort$trait_name <- "drought tolerance"

names(drought_hort)[names(drought_hort) == 'drought_tolerance'] <- 'value'

# select the species with hort info
hort_drought_species <- select(drought_hort, plant_name)

# extract the other info for just these species
other_info <- left_join(hort_drought_species, gh_drought, by = "plant_name")
other_info <- other_info %>%
  select(-trait_name , -value) %>%
  distinct(plant_name, .keep_all = TRUE)

# add drought data
gh_all_traits <- left_join(other_info, drought_hort, by = "plant_name")

# rearrange columns
gh_all_traits <- gh_all_traits %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, Koppen_zone, growth_form, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, value,
         bird, insect, lizard, native_mammal, pollinator, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)

# join to other data
gh_all <- bind_rows(gh_drought, gh_all_traits)
gh_all <- arrange(gh_all, plant_name, trait_name, value)

# add the dehydration and heat tolerance data
dehydration_heat <- select(drought_heat, -drought_tolerance)
dehydration_heat[] <- lapply(dehydration_heat, gsub, pattern = "Intermed/Tol", replacement = "intermediate - tolerant")
dehydration_heat[[76,3]] <- "NA"

gh_all <- left_join(gh_all, dehydration_heat, by = "plant_name")

# remove gh species from master database
all_entities_short <- all_entities_short %>%
  filter(exp_tested == "N")

# add the new columns
all_entities_short$dehydration_tolerance <- "NA"
all_entities_short$heat_tolerance <- "NA"

# join datasets
all_entities_short <- bind_rows(all_entities_short, gh_all)

all_entities_short <- arrange(all_entities_short, plant_name, trait_name, value)

## populate the model type column

sdm <- read.csv("Master_database_input/LindaFarzin/diff_ST_march_and_min5traits_good_10_03_2021_Clean.csv", strip.white = TRUE)
# get rid of empty cells with white space
# https://stackoverflow.com/questions/2261079/how-can-i-trim-leading-and-trailing-white-space

sdm_possible <- sdm %>%
  select(SDM.is.possible) %>%
  filter(SDM.is.possible != "")
# out of 1914 species, 1469 have data to do sdm, which means 445 are missing

colnames(sdm_possible) <- "speciesName"

# species with missing sdm
sdm_missing <- sdm %>%
  filter(SDM.is.possible == "") %>%
  select(species_list_ST_1Mar2021.csv)

colnames(sdm_missing) <- "speciesName"

# load ale's niche data

niche <- read.csv("Master_database_input/Ale/niche_summaries_5414_species.csv")

niche <- niche %>%
  distinct(speciesName)

# diff between missing sdm and niche list
diff_missing_sdm_niche <- setdiff(sdm_missing, niche)
# 77 species do not have a sdm or niche

# diff between models that do not have sdm or niche data and models that do not have sdm data
diff_niches <- setdiff(sdm_missing, diff_missing_sdm_niche)
# 368 species have niche data but not sdm

# populate model type
diff_niches$model_type <- "niche"
diff_missing_sdm_niche$model_type <- "NA"
sdm_possible$model_type <- "sdm"

# join together
model_type <- bind_rows(diff_niches, diff_missing_sdm_niche, sdm_possible)

# join to database
all_entities_short <- select(all_entities_short, -model_type)
names(model_type)[names(model_type) == 'speciesName'] <- 'plant_name'

all_entities_short <- left_join(all_entities_short, model_type, by = "plant_name")
all_entities_short$model_type[is.na(all_entities_short$model_type)] <- "NA"
all_entities_short$scientificNameStd[is.na(all_entities_short$scientificNameStd)] <- "NA"
all_entities_short$species[is.na(all_entities_short$species)] <- "NA"
all_entities_short$canopy_cover[is.na(all_entities_short$canopy_cover)] <- "NA"
all_entities_short$shade_value[is.na(all_entities_short$shade_value)] <- "NA"
all_entities_short$carbon_value[is.na(all_entities_short$carbon_value)] <- "NA"

# rearrange columns

all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, Koppen_zone, growth_form, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, value,
         bird, insect, lizard, native_mammal, pollinator, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index, dehydration_tolerance, heat_tolerance)

# check that cultivar synonyms are all accounted for
syn_check <- all_entities_short %>%
  filter(scientificNameStd != plant_name)

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(synonym = if_else(plant_name == "Magnolia champaca", "Michelia champaca", synonym),
         synonym = if_else(plant_name == "Chenopodium candolleanum", "Rhagodia candolleana", synonym),
         synonym = if_else(plant_name == "Cassia artemisioides", "Senna artemisioides", synonym))

# add Ale's shade and carbon index categories
categories <- read.csv("Master_database_input/Ale/co_benefit_analysis_ST_17.3.2021_AO.csv")

categories <- select(categories, plant_name, shade_index, carbon_index)

categories[] <- lapply(categories, gsub, pattern = "1_very low", replacement = "very low")
categories[] <- lapply(categories, gsub, pattern = "1_low", replacement = "low")
categories[] <- lapply(categories, gsub, pattern = "2_low", replacement = "low")
categories[] <- lapply(categories, gsub, pattern = "2_medium", replacement = "medium")
categories[] <- lapply(categories, gsub, pattern = "3_medium", replacement = "medium")
categories[] <- lapply(categories, gsub, pattern = "3_high", replacement = "high")
categories[] <- lapply(categories, gsub, pattern = "4_high", replacement = "high")

# join to main database
all_entities_short <- select(all_entities_short, -shade_index, -carbon_index)

all_entities_short <- left_join(all_entities_short, categories, by = "plant_name")
all_entities_short$shade_index[is.na(all_entities_short$shade_index)] <- "NA"
all_entities_short$carbon_index[is.na(all_entities_short$carbon_index)] <- "NA"

# rearrange columns
all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, Koppen_zone, growth_form, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, value,
         bird, insect, lizard, native_mammal, pollinator, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index, dehydration_tolerance, heat_tolerance)

write.csv(all_entities_short,"Master_database_output/FINAL/trait_database_ST_FINAL_17.3.2021_vers1.3.csv",row.names=FALSE)

# extract species with 0 for eco services
# glimpse(all_entities_short)
# all_entities_short$biodiversity_value <- as.numeric(as.character(all_entities_short$biodiversity_value))

# nothing <- all_entities_short %>%
#  filter(biodiversity_value == 0) %>%
#  distinct(family, genus, plant_name) %>%
#  group_by(family, genus) %>%
#  summarise(frequency = n()) %>%
#  arrange(family)

# write.csv(nothing,"Master_database_output/missing_biodiversity_value.csv",row.names=FALSE)

# do some checks

summary_new_new <- all_entities_short_check %>%
  select(entity, category) %>%
  distinct(entity, category) %>%
  group_by(category) %>%
  summarise(frequency = n())
# 2630 entities (added 6 new gh cultivars)

# check gh species

gh_species <- all_entities_short %>%
  filter(exp_tested == "Y") %>%
  select(plant_name) %>%
  distinct(plant_name)

# fail safe the database

# species with risk and playground friendly
risk_failsafe <- all_entities_short %>%
  group_by(plant_name) %>%
  filter(trait_name == "risk" & value == "playground friendly") %>%
  distinct(plant_name)
# no plants with risks are playground friendly

# species as both herbs and trees
herb_tree <- all_entities_short %>%
  group_by(plant_name) %>%
  filter(herb == 1 & tree == 1) %>%
  distinct(plant_name)
# no plants are herbs and trees

# species as both herb and shrub
herb_shrub <- all_entities_short %>%
  group_by(plant_name) %>%
  filter(herb == 1 & shrub == 1) %>%
  distinct(plant_name)
# 0 species

# species with high drought tolerance and high water needs
drought_failsafe <- all_entities_short %>%
  filter(trait_name == "drought_tolerance" | trait_name == "planting and maintenance") %>%
  group_by(plant_name) %>%
  filter(value == "puatively high" & value == "high water needs") %>%
  distinct(plant_name)
# 0 plants

# species with high and low water needs
waterneeds_failsafe <- all_entities_short %>%
  group_by(plant_name) %>%
  filter(value == "high water needs" & value == "low water needs") %>%
  distinct(plant_name)
# 0 plants

# species with slow and fast growth rate
growthrate_failsafe <- all_entities_short %>%
  group_by(plant_name) %>%
  filter(value == "slow" & value == "fast") %>%
  distinct(plant_name)
# 0 plants

# cross check urban context with height

############ Rony's species list

# rony <- read.csv("Master_database_input/Rony/spp_list_rony.csv")

# rony <- distinct(rony)
# 356 species

# colnames(rony) <- "plant_name"

# rony_traits <- left_join(rony, all_entities_short, by = "plant_name")

# rony_traits <- select(rony_traits, family, plant_name, origin, trait_name, value)

# rony_traits <- drop_na(rony_traits)

# plants <- distinct(rony_traits, plant_name)
# 261 species

# write.csv(rony_traits,"Master_database_output/Rony/rony_traits.csv",row.names=FALSE)




