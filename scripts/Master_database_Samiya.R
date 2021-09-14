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
  summarise(frequency = n()) %>%
  arrange(desc(frequency))

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
####### removed 'under powerlines' from the 'uses' trait and instead added a new column 'suitability_under_powerlines'

library(tidyverse)

everything <- read.csv("Master_database_input/EVERYTHING_traits_26Mar2021.csv")

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
  distinct(entity) #18

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
         family = if_else(entity == "Regelia velutina", "Myrtaceae", family),
         family = if_else(entity == "Citrus maxima", "Rutaceae", family))

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
  summarise(frequency = n()) %>%
  arrange(desc(frequency))

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
         value_new = if_else(value == "insignificant", "inconspicuous flowers", value_new),
         value_new = if_else(value == "inconspicuous", "inconspicuous flowers", value_new),
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
  mutate(trait_name_new = if_else(trait_name == "soil_type", "soil texture", trait_name_new),
         trait_name_new = if_else(trait_name == "soil_pH", "soil pH", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "acid", "acidic", value_new))

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
         value_new = if_else(value == "severeallergen", "allergen", value_new),
         value_new = if_else(value == "spikey", "spikey or spiny", value_new))

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
  mutate(trait_name_new = if_else(trait_name == "weed_status", "weed status in Australia", trait_name_new))

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

## powerlines
# remove 'under powerlines' as a value from the database
all_entities_short <- all_entities_short %>%
  filter(value != "under powerlines")

# filter out all the shrubs and trees <= 3m max height
shrubsandtrees_3 <- all_entities_short %>%
  filter(shrub == 1 | tree == 1) %>%
  filter(height_max <= 3)

shrubsandtrees_3$suitability_under_powerlines <- "suitable"

# filter out all the shrubs and trees with max height between 3 and 6m
shrubsandtrees_3_6 <- all_entities_short %>%
  filter(shrub == 1 | tree == 1) %>%
  filter(3 < height_max & height_max <= 6)

shrubsandtrees_3_6$suitability_under_powerlines <- "suitable with pruning"

# filter out all the shrubs and trees with max height greater than 6m

shrubsandtrees_6 <- all_entities_short %>%
  filter(shrub == 1 | tree == 1) %>%
  filter(height_max > 6)

shrubsandtrees_6$suitability_under_powerlines <- "not suitable"

# join everything together
shrubsandtrees <- bind_rows(shrubsandtrees_3, shrubsandtrees_3_6, shrubsandtrees_6)

# filter out the shrubs and trees from the database
all_entities_short <- all_entities_short %>%
  filter(shrub == 0 & tree == 0)

all_entities_short$suitability_under_powerlines <- "NA"

# join everything together
all_entities_short <- bind_rows(all_entities_short, shrubsandtrees)

# rearrange columns
all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, Koppen_zone, growth_form, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, value,
         bird, insect, lizard, native_mammal, pollinator, biodiversity_value, height_min, height_max, width_min, width_max, suitability_under_powerlines, canopy_cover, shade_value, shade_index, carbon_value, carbon_index, dehydration_tolerance, heat_tolerance)

all_entities_short <- arrange(all_entities_short, scientificNameStd, plant_name, trait_name, value)

# attach the synonyms found through Taxonstand

taxonomy <- read.csv("Master_database_output/taxonomy_checks/taxonstandcheck_ST_1.3.2021.csv")

taxonomy <- taxonomy %>% 
  select(Taxon, Taxonomic.status, New.Genus, New.Species) %>%
  filter(Taxonomic.status == "Synonym") %>%
  mutate(synonym = paste0(New.Genus, " ", New.Species)) %>%
  select(Taxon, synonym)

taxonomy <- taxonomy %>% 
  select(Taxon, Taxonomic.status, New.Genus, New.Species, New.Infraspecific.rank, New.Infraspecific) %>%
  filter(Taxonomic.status == "Synonym") %>%
  mutate(synonym = if_else(New.Infraspecific.rank != "", paste0(New.Genus, " ", New.Species, " ", New.Infraspecific.rank, " ",
                                                                New.Infraspecific), paste0(New.Genus, " ", New.Species))) %>%
  select(Taxon, synonym)

# remove the dots
taxonomy[] <- lapply(taxonomy, gsub, pattern = "var.", replacement = "var")
taxonomy[] <- lapply(taxonomy, gsub, pattern = "subsp.", replacement = "subsp")

# make sure the synonyms are not actually species
  
plant_name_database <- all_entities_short %>%
  filter(category == "SP") %>%
  distinct(plant_name)

synonym_names <- select(taxonomy, synonym)

same <- inner_join(plant_name_database, synonym_names, by = c("plant_name" = "synonym"))

# add the synonyms (if plants already don't have them)
no_syn <- all_entities_short %>%
  filter(synonym == "NA")

# extract the species from 'taxonomy'
names(taxonomy)[names(taxonomy) == 'Taxon'] <- 'plant_name'

no_syn_taxonomy <- inner_join(taxonomy, no_syn, by = "plant_name")

no_syn_taxonomy$synonym.y <- no_syn_taxonomy$synonym.x

no_syn_taxonomy <- select(no_syn_taxonomy, -synonym.x)
names(no_syn_taxonomy)[names(no_syn_taxonomy) == 'synonym.y'] <- 'synonym'

# remove these species from the database
all_entities_short <- anti_join(all_entities_short, no_syn_taxonomy, by = "plant_name")

# add back together
all_entities_short <- bind_rows(all_entities_short, no_syn_taxonomy)
all_entities_short <- arrange(all_entities_short, scientificNameStd, plant_name, trait_name, value)

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

# check all the traits are there
trait_name_check <- all_entities_short %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency))

# write.csv(trait_name_check,"Master_database_output/trait_frequency.csv",row.names=FALSE)

value_name_check <- all_entities_short %>%
  distinct(plant_name, trait_name, value) %>%
  group_by(trait_name, value) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency))

value_remove <- value_name_check %>%
  filter(trait_name == "common name" | trait_name == "minimum width" | trait_name == "minimum height" 
         | trait_name == "average height" | trait_name == "average width" | trait_name == "maximum height" 
         | trait_name == "maximum width")

value_name_check <- anti_join(value_name_check, value_remove)

## write.csv(value_name_check,"Master_database_output/value_frequency.csv",row.names=FALSE)

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

urbancontext_height <- all_entities_short %>%
  filter(value == "street") %>%
  distinct(plant_name, .keep_all = TRUE) %>%
  select(plant_name, height_max, width_max)
# anything with max height >=30m and/or max width >20m I have changed to 'park'
# check with Michelle before changing

##### trying to extract synonyms from gbif
# https://data-blog.gbif.org/post/2019-07-11-downloading-long-species-lists-on-gbif_files/global_tree_search_trees_1_3.csv

# test_names <- read.csv("Master_database_input/test_names.csv")
# 
# gbif_taxon_keys <- read.csv("Master_database_input/test_names.csv") %>%
#   pull("Taxon.name") %>%
#   taxize::get_gbifid_(method="backbone")
#   
# test_names$Taxon.name <- as.character(test_names$Taxon.name)
# 
# avector <- pull(test_names, Taxon.name)
# glimpse(avector)
# 
# x <- synonyms(avector, db = "itis")
# y <- synonyms_df(x)

# try on the real database
glimpse(all_entities_short)
plant_names <- all_entities_short %>%
  filter(category == "SP") %>%
  distinct(plant_name)

# pull out 200 plants
one_200 <- plant_names[1:200, ]

library(taxize)
z <- synonyms(one_200, db = "itis")
one_200 <- synonyms_df(z)

# pull out 201 - 400
two01_400 <- plant_names[201:400, ]

z <- synonyms(two01_400, db = "itis")
two01_400 <- synonyms_df(z)

# pull out 401 - 600
four01_600 <- plant_names[401:600, ]

z <- synonyms(four01_600, db = "itis")
four01_600 <- synonyms_df(z)

# pull out 601 - 800
six01_800 <- plant_names[601:800, ]

z <- synonyms(six01_800, db = "itis")
six01_800 <- synonyms_df(z)

# pull out 801 - 1000
eight01_1000 <- plant_names[801:1000, ]

z <- synonyms(eight01_1000, db = "itis")
eight01_1000 <- synonyms_df(z)

# pull out 1001 - 1200
ten01_1200 <- plant_names[1001:1200, ]

z <- synonyms(ten01_1200, db = "itis")
ten01_1000 <- synonyms_df(z)

# pull out 1201 - 1400
twelve01_1400 <- plant_names[1201:1400, ]

z <- synonyms(twelve01_1400, db = "itis")
twelve01_1400 <- synonyms_df(z)

# pull out 1401 - 1600
fourteen01_1600 <- plant_names[1401:1600, ]

z <- synonyms(fourteen01_1600, db = "itis")
fourteen01_1600 <- synonyms_df(z)

# pull out 1601 - 1914
sixteen01_1914 <- plant_names[1601:1914, ]

z <- synonyms(sixteen01_1914, db = "itis")
sixteen01_1914 <- synonyms_df(z)

# combine all together
synonyms_itis <- bind_rows(one_200, two01_400, four01_600, six01_800, eight01_1000, ten01_1000, twelve01_1400,
                           fourteen01_1600, sixteen01_1914)

write.csv(synonyms_itis,"Master_database_output/taxonomy_checks/synonyms_itis.csv",row.names=FALSE)

# try a different database (only other database I think is relevant to plants is tropicos)

plant_names <- all_entities_short %>%
  filter(category == "SP") %>%
  distinct(plant_name)

# pull out 200 plants
one_200 <- plant_names[1:200, ]

library(taxize)
z <- synonyms(one_200, db = "tropicos")
one_200 <- synonyms_df(z)

# try this
# https://gist.github.com/sckott/4507348

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

############ Jaco traits

# jaco <- all_entities_short %>%
#  filter(category == "CULVAR") %>%
#  distinct(scientificNameStd)

# pull out traits for these species from the database
# jaco <- left_join(jaco, all_entities_short, by = "scientificNameStd")

# jaco <- select(jaco, scientificNameStd, plant_name, synonym, category, trait_name, value)

# jaco <- jaco %>%
#  filter(trait_name == "average height" | trait_name == "average width" | trait_name == "maximum height" | 
#           trait_name == "maximum width" | trait_name == "minimum height" | trait_name == "weed status in Australia")

# write.csv(jaco,"Master_database_output/Jaco/cultivar_traits.csv",row.names=FALSE)

# extract the traits for hybrids and parents

# first get list of parents

# hybrid_parents <- all_entities_short %>%
#   filter(category == "H" | category == "HC") %>%
#   distinct(plant_name, .keep_all = TRUE) %>%
#   select(Parent_1, Parent_2, Parent_3, Parent_4)

# Parent_1 <- select(hybrid_parents, Parent_1)
# colnames(Parent_1) <- "parent"
# 
# Parent_2 <- select(hybrid_parents, Parent_2)
# Parent_2 <- filter(Parent_2, Parent_2 != "NA")
# colnames(Parent_2) <- "parent"
# 
# Parent_3 <- select(hybrid_parents, Parent_3)
# Parent_3 <- filter(Parent_3, Parent_3 != "NA")
# colnames(Parent_3) <- "parent"
# 
# Parent_4 <- select(hybrid_parents, Parent_4)
# Parent_4 <- filter(Parent_4, Parent_4 != "NA")
# colnames(Parent_4) <- "parent"
# 
# parent_names <- bind_rows(Parent_1, Parent_2, Parent_3, Parent_4)
# 
# parent_names <- distinct(parent_names)
# colnames(parent_names) <- "plant_name"

# extract traits for these parents from the database

# parent_traits <- left_join(parent_names, all_entities_short, by = "plant_name")

# extract the hybrids
# hybrids <- all_entities_short %>%
#   filter(category == "H" | category == "HC")

# join together
# jaco_hybrids <- bind_rows(parent_traits, hybrids)
# 
# jaco_hybrids <- select(jaco_hybrids, scientificNameStd, plant_name, synonym, category, Parent_1, Parent_2, Parent_3, Parent_4, trait_name, value)
# 
# jaco_hybrids <- jaco_hybrids %>%
#    filter(trait_name == "average height" | trait_name == "average width" | trait_name == "maximum height" | 
#           trait_name == "maximum width" | trait_name == "minimum height" | trait_name == "weed status in Australia")
#   
# write.csv(jaco_hybrids,"Master_database_output/Jaco/hybrid_traits.csv",row.names=FALSE)

#########
# Check overlap with wpw images

plant_name_database <- all_entities_short %>%
  distinct(plant_name)

# load image names from Norwood
norwood <- read.csv("Master_database_input/photos/images.csv")

norwood <- norwood %>%
  distinct(Image.Name)
  
colnames(norwood) <- "plant_name"

# check similarity without case sensitivity
# https://stackoverflow.com/questions/57502990/compare-two-columns-without-case-sensitivity

plant_name_database <- tolower(plant_name_database$plant_name)

plant_name_database <- as.data.frame(plant_name_database, stringsAsFactors = FALSE)

norwood <- tolower(norwood$plant_name)

norwood <- as.data.frame(norwood, stringsAsFactors = FALSE)

same <- inner_join(plant_name_database, norwood, by = c("plant_name_database" = "norwood"))
# have photos for 1645 species
# (1645/2630)*100 = 62% coverage

####################################

# check AI with hort classifications of drought

drought_class <- all_entities_short %>%
  filter(exp_tested == "N") %>%
  filter(trait_name == "drought tolerance") %>%
  select(plant_name, value)

names(drought_class)[names(drought_class) == 'value'] <- 'drought_tolerance'

# load Ale data

Ale_ai <- read.csv("Master_database_input/Ale/niche_summaries_5414_species.csv")

Ale_ai <- Ale_ai %>%
  filter(var == "ai") %>%
  select(speciesName, p0, mean, p100)

names(Ale_ai)[names(Ale_ai) == 'speciesName'] <- 'plant_name'

Ale_ai <- Ale_ai %>%
  mutate(AI_min = p0/1000,
         AI_mean = mean/1000,
         AI_max = p100/1000) %>%
  select(plant_name, AI_min, AI_mean, AI_max)

# make into long format
# http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
Ale_ai_long <- gather(Ale_ai, AI_type, AI, AI_min:AI_max)

# join together

ai_drought_class <- inner_join(drought_class, Ale_ai_long, by = "plant_name")

# graph

library(ggplot2)

plot <- ggplot(ai_drought_class, aes(x = drought_tolerance, y = AI, fill = AI_type)) +
  geom_boxplot()
plot
######### have to ask Ale if I did the transformations correctly

##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################

################ Vers 1.4

# removed 'suitability under powerlines'
# removed the 'growth_form' column
# changed wet to WSUD
# changed dehydration tolerance
# removed gravel
# removed container and indoors
# changed herb to herbaceous
# simplified canopy shape


library(tidyverse)

everything <- read.csv("Master_database_input/EVERYTHING_traits_7Apr2021.csv")

everything_gh <- read.csv("Master_database_input/EVERYTHING_gh_7Apr2021.csv")

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
  distinct(entity) #18

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
         family = if_else(entity == "Regelia velutina", "Myrtaceae", family),
         family = if_else(entity == "Citrus maxima", "Rutaceae", family))

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
# 2630 plant names

traits <- all_entities_short %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2630)*100) # all relevant traits still 100%

# remove medicinal from usage

all_entities_short <- all_entities_short %>%
  filter(value != "medicinal")

# check coverage again

traits <- all_entities_short %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2630)*100) # usage still 100%

# check apiary

no_apiary <- all_entities_short %>%
  filter(value != "apiary")

# check coverage again

traits <- no_apiary %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2630)*100) # usage still 100%

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
  summarise(frequency = n()) %>%
  arrange(desc(frequency))

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
         value_new = if_else(value == "insignificant", "inconspicuous flowers", value_new),
         value_new = if_else(value == "inconspicuous", "inconspicuous flowers", value_new),
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
         value_new = if_else(value == "powerlines", "under powerlines", value_new),
         value_new = if_else(value == "wet", "WSUD", value_new),
         value_new = if_else(value == "container", "garden", value_new),
         value_new = if_else(value == "indoor", "garden", value_new))

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
  mutate(trait_name_new = if_else(trait_name == "soil_type", "soil texture", trait_name_new),
         trait_name_new = if_else(trait_name == "soil_pH", "soil pH", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "acid", "acidic", value_new))

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
  mutate(value_new = if_else(value == "prostrate", "upright", value_new),
         value_new = if_else(value == "domed", "rounded", value_new),
         value_new = if_else(value == "narrow", "upright", value_new),
         value_new = if_else(value == "oval", "rounded", value_new))

# filter out rest
remove_habit_canopy <- all_entities_short %>%
  filter(trait_name == "habit_canopy" & value == "arborescent" | trait_name == "habit_canopy" & value == "branching" | 
           trait_name == "habit_canopy" & value == "clumping" | trait_name == "habit_canopy" & value == "compact" | 
           trait_name == "habit_canopy" & value == "conical" | trait_name == "habit_canopy" & value == "vase" |
           trait_name == "habit_canopy" & value == "bushy" | trait_name == "habit_canopy" & value == "dense" |
           trait_name == "habit_canopy" & value == "hedging_possible" | trait_name == "habit_canopy" & value == "open" |
           trait_name == "habit_canopy" & value == "variable" | trait_name == "habit_canopy" & value == "weeping") 

# remove
all_entities_short <- anti_join(all_entities_short, remove_habit_canopy)

# RISK
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "mildallergen", "allergen", value_new),
         value_new = if_else(value == "moderateallergen", "allergen", value_new),
         value_new = if_else(value == "poison", "poisonous or toxic", value_new),
         value_new = if_else(value == "severeallergen", "allergen", value_new),
         value_new = if_else(value == "spikey", "spikey or spiny", value_new))

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
  mutate(trait_name_new = if_else(trait_name == "weed_status", "weed status in Australia", trait_name_new))

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

# attach the synonyms found through Taxonstand

taxonomy <- read.csv("Master_database_output/taxonomy_checks/taxonstandcheck_ST_1.3.2021.csv")

taxonomy <- taxonomy %>% 
  select(Taxon, Taxonomic.status, New.Genus, New.Species) %>%
  filter(Taxonomic.status == "Synonym") %>%
  mutate(synonym = paste0(New.Genus, " ", New.Species)) %>%
  select(Taxon, synonym)

taxonomy <- taxonomy %>% 
  select(Taxon, Taxonomic.status, New.Genus, New.Species, New.Infraspecific.rank, New.Infraspecific) %>%
  filter(Taxonomic.status == "Synonym") %>%
  mutate(synonym = if_else(New.Infraspecific.rank != "", paste0(New.Genus, " ", New.Species, " ", New.Infraspecific.rank, " ",
                                                                New.Infraspecific), paste0(New.Genus, " ", New.Species))) %>%
  select(Taxon, synonym)

# remove the dots
taxonomy[] <- lapply(taxonomy, gsub, pattern = "var.", replacement = "var")
taxonomy[] <- lapply(taxonomy, gsub, pattern = "subsp.", replacement = "subsp")

# make sure the synonyms are not actually species

plant_name_database <- all_entities_short %>%
  filter(category == "SP") %>%
  distinct(plant_name)

synonym_names <- select(taxonomy, synonym)

same <- inner_join(plant_name_database, synonym_names, by = c("plant_name" = "synonym"))

# add the synonyms (if plants already don't have them)
no_syn <- all_entities_short %>%
  filter(synonym == "NA")

# extract the species from 'taxonomy'
names(taxonomy)[names(taxonomy) == 'Taxon'] <- 'plant_name'

no_syn_taxonomy <- inner_join(taxonomy, no_syn, by = "plant_name")

no_syn_taxonomy$synonym.y <- no_syn_taxonomy$synonym.x

no_syn_taxonomy <- select(no_syn_taxonomy, -synonym.x)
names(no_syn_taxonomy)[names(no_syn_taxonomy) == 'synonym.y'] <- 'synonym'

# remove these species from the database
all_entities_short <- anti_join(all_entities_short, no_syn_taxonomy, by = "plant_name")

# add back together
all_entities_short <- bind_rows(all_entities_short, no_syn_taxonomy)
all_entities_short <- arrange(all_entities_short, scientificNameStd, plant_name, trait_name, value)

#### feedback from the advisory board + remove erroneous columns
## powerlines
# remove 'under powerlines' as a value from the database
all_entities_short <- all_entities_short %>%
  filter(value != "under powerlines")

# remove the 'growth_form' column
all_entities_short <- all_entities_short %>%
  select(-growth_form)

# change 'herb' into 'herbaceous'
names(all_entities_short)[names(all_entities_short) == 'herb'] <- 'herbaceous'

# remove 'gravel' as a 'soil texture'
all_entities_short <- all_entities_short %>%
  filter(value != "gravel")

# shrubs with min height > 5m should be trees
# all_entities_short <- all_entities_short %>%
#   mutate_if(is.factor, as.character) %>%
#   mutate(tree = if_else(shrub == 1 & height_min >= 5, 1, tree))
### CAN'T CHANGE YET, NEED ALE TO RECALCULATE VALUES

# fix shade and carbon for Citrus grandis
all_entities_short <- all_entities_short %>%
     mutate_if(is.factor, as.character) %>%
  mutate(carbon_index = if_else(plant_name == "Citrus grandis", "low", carbon_index),
         shade_index = if_else(plant_name == "Citrus grandis", "low", shade_index))

# fix the gh species drought tolerance
all_entities_short[] <- lapply(all_entities_short, gsub, pattern = "dehydration tolerator", replacement = "tolerator")
all_entities_short[] <- lapply(all_entities_short, gsub, pattern = "dehydration avoider", replacement = "avoider")
all_entities_short[] <- lapply(all_entities_short, gsub, pattern = "intermediate", replacement = "tolerator/avoider")
  
names(all_entities_short)[names(all_entities_short) == 'dehydration_tolerance'] <- 'drought_strategy'

write.csv(all_entities_short,"Master_database_output/FINAL/trait_database_ST_FINAL_07.4.2021_vers1.4.csv",row.names=FALSE)

###########################################################################################################

##################### version 1.5

## added more synonyms
## added more biodiversity
## fixed up trees - new shade and carbon index
## AI and drought tolerance

library(tidyverse)

everything <- read.csv("Master_database_input/EVERYTHING_traits_14Apr2021.csv")

everything_gh <- read.csv("Master_database_input/EVERYTHING_gh_14Apr2021.csv")

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
  distinct(entity) #18

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
         family = if_else(entity == "Regelia velutina", "Myrtaceae", family),
         family = if_else(entity == "Citrus maxima", "Rutaceae", family))

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
# 2630 plant names

traits <- all_entities_short %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2630)*100) # all relevant traits still 100%

# remove medicinal from usage

all_entities_short <- all_entities_short %>%
  filter(value != "medicinal")

# check coverage again

traits <- all_entities_short %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2630)*100) # usage still 100%

# check apiary

no_apiary <- all_entities_short %>%
  filter(value != "apiary")

# check coverage again

traits <- no_apiary %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2630)*100) # usage still 100%

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

# shrubs with min height > 5m should be trees (Michelle decided)
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(tree = if_else(shrub == 1 & height_min >= 5, 1, tree))

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
  summarise(frequency = n()) %>%
  arrange(desc(frequency))

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
         value_new = if_else(value == "insignificant", "inconspicuous flowers", value_new),
         value_new = if_else(value == "inconspicuous", "inconspicuous flowers", value_new),
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
         value_new = if_else(value == "powerlines", "under powerlines", value_new),
         value_new = if_else(value == "wet", "WSUD", value_new),
         value_new = if_else(value == "container", "garden", value_new),
         value_new = if_else(value == "indoor", "garden", value_new))

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
  mutate(trait_name_new = if_else(trait_name == "soil_type", "soil texture", trait_name_new),
         trait_name_new = if_else(trait_name == "soil_pH", "soil pH", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "acid", "acidic", value_new))

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
  mutate(value_new = if_else(value == "prostrate", "upright", value_new),
         value_new = if_else(value == "domed", "rounded", value_new),
         value_new = if_else(value == "narrow", "upright", value_new),
         value_new = if_else(value == "oval", "rounded", value_new))

# filter out rest
remove_habit_canopy <- all_entities_short %>%
  filter(trait_name == "habit_canopy" & value == "arborescent" | trait_name == "habit_canopy" & value == "branching" | 
           trait_name == "habit_canopy" & value == "clumping" | trait_name == "habit_canopy" & value == "compact" | 
           trait_name == "habit_canopy" & value == "conical" | trait_name == "habit_canopy" & value == "vase" |
           trait_name == "habit_canopy" & value == "bushy" | trait_name == "habit_canopy" & value == "dense" |
           trait_name == "habit_canopy" & value == "hedging_possible" | trait_name == "habit_canopy" & value == "open" |
           trait_name == "habit_canopy" & value == "variable" | trait_name == "habit_canopy" & value == "weeping") 

# remove
all_entities_short <- anti_join(all_entities_short, remove_habit_canopy)

# RISK
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "mildallergen", "allergen", value_new),
         value_new = if_else(value == "moderateallergen", "allergen", value_new),
         value_new = if_else(value == "poison", "poisonous or toxic", value_new),
         value_new = if_else(value == "severeallergen", "allergen", value_new),
         value_new = if_else(value == "spikey", "spikey or spiny", value_new))

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
  mutate(trait_name_new = if_else(trait_name == "weed_status", "weed status in Australia", trait_name_new))

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

# add Ale's shade and carbon index categories (NEED TO REDO)

# extract info for Ale to recalculate
# co_benefit <- all_entities_short %>%
#   filter(tree == 1) %>%
#   distinct(scientificNameStd, plant_name, .keep_all = TRUE) %>%
#   select(scientificNameStd, plant_name, tree, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)
# 
# write.csv(co_benefit,"Master_database_output/Ale/co_benefit_analysis_ST_12.4.2021.csv",row.names = FALSE)

categories <- read.csv("Master_database_input/Ale/co_benefit_analysis_ST_12.4.2021_AO.csv")

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

# attach the synonyms found through Taxonstand

taxonomy <- read.csv("Master_database_output/taxonomy_checks/taxonstandcheck_ST_1.3.2021.csv")

taxonomy <- taxonomy %>% 
  select(Taxon, Taxonomic.status, New.Genus, New.Species) %>%
  filter(Taxonomic.status == "Synonym") %>%
  mutate(synonym = paste0(New.Genus, " ", New.Species)) %>%
  select(Taxon, synonym)

taxonomy <- taxonomy %>% 
  select(Taxon, Taxonomic.status, New.Genus, New.Species, New.Infraspecific.rank, New.Infraspecific) %>%
  filter(Taxonomic.status == "Synonym") %>%
  mutate(synonym = if_else(New.Infraspecific.rank != "", paste0(New.Genus, " ", New.Species, " ", New.Infraspecific.rank, " ",
                                                                New.Infraspecific), paste0(New.Genus, " ", New.Species))) %>%
  select(Taxon, synonym)

# remove the dots
taxonomy[] <- lapply(taxonomy, gsub, pattern = "var.", replacement = "var")
taxonomy[] <- lapply(taxonomy, gsub, pattern = "subsp.", replacement = "subsp")

# make sure the synonyms are not actually species

plant_name_database <- all_entities_short %>%
  filter(category == "SP") %>%
  distinct(plant_name)

synonym_names <- select(taxonomy, synonym)

same <- inner_join(plant_name_database, synonym_names, by = c("plant_name" = "synonym"))

# add the synonyms (if plants already don't have them)
no_syn <- all_entities_short %>%
  filter(synonym == "NA")

# extract the species from 'taxonomy'
names(taxonomy)[names(taxonomy) == 'Taxon'] <- 'plant_name'

no_syn_taxonomy <- inner_join(taxonomy, no_syn, by = "plant_name")

no_syn_taxonomy$synonym.y <- no_syn_taxonomy$synonym.x

no_syn_taxonomy <- select(no_syn_taxonomy, -synonym.x)
names(no_syn_taxonomy)[names(no_syn_taxonomy) == 'synonym.y'] <- 'synonym'

# remove these species from the database
all_entities_short <- anti_join(all_entities_short, no_syn_taxonomy, by = "plant_name")

# add back together
all_entities_short <- bind_rows(all_entities_short, no_syn_taxonomy)
all_entities_short <- arrange(all_entities_short, scientificNameStd, plant_name, trait_name, value)

#### feedback from the advisory board + remove erroneous columns
## powerlines
# remove 'under powerlines' as a value from the database
all_entities_short <- all_entities_short %>%
  filter(value != "under powerlines")

# remove the 'growth_form' column
all_entities_short <- all_entities_short %>%
  select(-growth_form)

# change 'herb' into 'herbaceous'
names(all_entities_short)[names(all_entities_short) == 'herb'] <- 'herbaceous'

# remove 'gravel' as a 'soil texture'
all_entities_short <- all_entities_short %>%
  filter(value != "gravel")

# fix shade and carbon for Citrus grandis
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(carbon_index = if_else(plant_name == "Citrus grandis", "low", carbon_index),
         shade_index = if_else(plant_name == "Citrus grandis", "low", shade_index))

# fix the gh species drought tolerance
all_entities_short[] <- lapply(all_entities_short, gsub, pattern = "dehydration tolerator", replacement = "tolerator")
all_entities_short[] <- lapply(all_entities_short, gsub, pattern = "dehydration avoider", replacement = "avoider")
all_entities_short[] <- lapply(all_entities_short, gsub, pattern = "intermediate", replacement = "tolerator/avoider")

names(all_entities_short)[names(all_entities_short) == 'dehydration_tolerance'] <- 'drought_strategy'

# try synonyms again
# https://github.com/ropensci/rgbif/issues/289

# library(rgbif)
# 
# plant_names <- all_entities_short %>%
#   filter(category == "SP") %>%
#   distinct(plant_name)
# 
# colnames(plant_names) <- "scientificName"
# 
# write.csv(plant_names,"Master_database_output/plant_names.csv",row.names = FALSE)

# run through this to get key
# https://www.gbif.org/tools/species-lookup
# save output

# load output
# gbif_species <- read.csv("Master_database_input/synonyms/gbif_species.csv")
# 
# syns <- lapply(gbif_species$key[1:1914], name_usage, data = "synonyms")
# 
# x_unlist <- unlist(syns, recursive = FALSE) # https://stackoverflow.com/questions/43591029/convert-nested-list-elements-into-data-frame-and-bind-the-result-into-one-data-f/43592335
# 
# # only want he 'data' elements
# y <- x_unlist[grep("data", names(x_unlist))] # https://stackoverflow.com/questions/39983986/filter-or-subset-list-by-partial-object-name-in-r  
# 
# z <- rbindlist(y, fill = TRUE) # bind the lists and fill in the missing columns
# 
# synonyms <- z %>%
#   select(species, canonicalName, rank, taxonomicStatus) %>%
#   filter(taxonomicStatus == "SYNONYM") %>%
#   filter(rank == "SPECIES") %>%
#   select(species, canonicalName)
# 
# write.csv(synonyms,"Master_database_input/synonyms/gbif_synonyms.csv",row.names = FALSE)

# synonyms

gbif_synonyms <- read.csv("Master_database_input/synonyms/gbif_synonyms.csv")

# add the species names from gbif that are already apparently synonyms
gbif_species <- read.csv("Master_database_input/synonyms/gbif_species.csv")

gbif_species <- gbif_species %>%
  filter(status == "SYNONYM") %>%
  select(verbatimScientificName, species)

names(gbif_species)[names(gbif_species) == 'species'] <- 'canonicalName'
names(gbif_species)[names(gbif_species) == 'verbatimScientificName'] <- 'species'

# join
gbif_synonyms <- bind_rows(gbif_synonyms, gbif_species)

# check the synonyms I already have in the database

syn_already_have <- all_entities_short %>%
  filter(synonym != "NA") %>%
  distinct(plant_name, synonym)

# fix up Myoporum tenuifolium (Myoporum acuminatum, Myoporum montanum) and Abelia uniflora (Abelia engleriana, Abelia schumannii)
syn_already_have <- syn_already_have %>%
  filter(plant_name != "Abelia uniflora" & plant_name != "Myoporum tenuifolium")

syn_already_have <- syn_already_have %>%
  add_row(plant_name = "Abelia uniflora", synonym = "Abelia engleriana")
syn_already_have <- syn_already_have %>%
  add_row(plant_name = "Abelia uniflora", synonym = "Abelia schumannii")
syn_already_have <- syn_already_have %>%
  add_row(plant_name = "Myoporum tenuifolium", synonym = "Myoporum acuminatum")
syn_already_have <- syn_already_have %>%
  add_row(plant_name = "Myoporum tenuifolium", synonym = "Myoporum montanum")

names(syn_already_have)[names(syn_already_have) == 'plant_name'] <- 'species'
names(syn_already_have)[names(syn_already_have) == 'synonym'] <- 'canonicalName'

# add to what I have
gbif_synonyms <- bind_rows(gbif_synonyms, syn_already_have)
gbif_synonyms <- distinct(gbif_synonyms, species, canonicalName)

# filter and fix
gbif_synonyms$species <- as.character(gbif_synonyms$species)
gbif_synonyms$canonicalName <- as.character(gbif_synonyms$canonicalName)

gbif_synonyms <- filter(gbif_synonyms, species != canonicalName)

# make sure the synonyms are not actually species

plant_name_database <- all_entities_short %>%
  filter(category == "SP") %>%
  distinct(plant_name)

synonym_names <- select(gbif_synonyms, canonicalName)
synonym_names <- distinct(synonym_names) # two different species can have the same synonym, why are they not syns of each other?

same <- inner_join(plant_name_database, synonym_names, by = c("plant_name" = "canonicalName"))  
# 16 differ

# filter these out
names(same)[names(same) == 'plant_name'] <- 'canonicalName'

# remove
gbif_synonyms <- anti_join(gbif_synonyms, same)

# check
synonym_names <- select(gbif_synonyms, canonicalName)
synonym_names <- distinct(synonym_names) # two different species can have the same synonym, why are they not syns of each other?

same <- inner_join(plant_name_database, synonym_names, by = c("plant_name" = "canonicalName"))
# all removed

# two species with the same synonym
gbif_synonyms_check <- gbif_synonyms %>%
  group_by(canonicalName) %>%
  summarise(frequency = n())
# I would remove these, there are 39

gbif_synonyms_remove <- gbif_synonyms_check %>%
  filter(frequency > 1) %>%
  select(canonicalName)

# remove
gbif_synonyms <- anti_join(gbif_synonyms, gbif_synonyms_remove)

# there are weird canonicalNames with 'publ' at the end, filter those out
# https://stackoverflow.com/questions/22850026/filter-rows-which-contain-a-certain-string

gbif_synonyms <- filter(gbif_synonyms, !grepl("publ", canonicalName))

# IT IS ALL CLEANED, NOW TO JOIN TOGETHER
# https://stackoverflow.com/questions/38514988/concatenate-strings-by-group-with-dplyr

gbif_synonyms_join <- gbif_synonyms %>%
  group_by(species) %>%
  mutate(synonyms = paste0(canonicalName, collapse = ", ")) %>%
  distinct(species, synonyms)
# we have synonyms for 1294/1914 species!!!!

# join to master database
names(gbif_synonyms_join)[names(gbif_synonyms_join) == 'species'] <- 'plant_name'
names(gbif_synonyms_join)[names(gbif_synonyms_join) == 'synonyms'] <- 'synonym'

all_entities_short <- select(all_entities_short, -synonym)

all_entities_short <- left_join(all_entities_short, gbif_synonyms_join, by = "plant_name")

all_entities_short$synonym[is.na(all_entities_short$synonym)] <- "NA"

# rearrange columns
all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, Koppen_zone, climber, cycad, fern, grass, herbaceous, palm, shrub, succulent, tree, origin, trait_name, value,
         bird, insect, lizard, native_mammal, pollinator, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index, drought_strategy, heat_tolerance)

write.csv(all_entities_short,"Master_database_output/FINAL/trait_database_ST_FINAL_14.4.2021_vers1.5.csv",row.names=FALSE)

# check AI with hort classifications of drought

drought_class <- all_entities_short %>%
  filter(exp_tested == "N") %>%
  filter(trait_name == "drought tolerance") %>%
  select(plant_name, value)

names(drought_class)[names(drought_class) == 'value'] <- 'drought_tolerance'

# load Ale data

Ale_ai <- read.csv("Master_database_input/Ale/niche_summaries_5414_species.csv")

Ale_ai <- Ale_ai %>%
  filter(var == "ai") %>%
  select(speciesName, p0, mean, p100)

names(Ale_ai)[names(Ale_ai) == 'speciesName'] <- 'plant_name'

Ale_ai <- Ale_ai %>%
  mutate(AI_min = p0/1000,
         AI_mean = mean/1000,
         AI_max = p100/1000) %>%
  select(plant_name, AI_min, AI_mean, AI_max)

# make into long format
# http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
Ale_ai_long <- gather(Ale_ai, AI_type, AI, AI_min:AI_max)

# join together

ai_drought_class <- inner_join(drought_class, Ale_ai_long, by = "plant_name")

# graph

library(ggplot2)

plot <- ggplot(ai_drought_class, aes(x = drought_tolerance, y = AI, fill = AI_type)) +
  geom_boxplot()
plot
######### have to ask Ale if I did the transformations correctly

###########################################################################################################

##################### version 1.6

## fixed errors in traits based on internal beta version feedback
# 'grass' changed to 'grass-like'
# 'planting and maintenance' - 'protected' should be 'sheltered' (correct on the metadata)
# canopy cover now only whole numbers

library(tidyverse)

everything <- read.csv("Master_database_input/EVERYTHING_traits_26Apr2021.csv")

everything_gh <- read.csv("Master_database_input/EVERYTHING_gh_26Apr2021.csv")

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
  distinct(entity) #18

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
         family = if_else(entity == "Regelia velutina", "Myrtaceae", family),
         family = if_else(entity == "Citrus maxima", "Rutaceae", family))

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
# 2630 plant names

traits <- all_entities_short %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2630)*100) # all relevant traits still 100%

# remove medicinal from usage

all_entities_short <- all_entities_short %>%
  filter(value != "medicinal")

# check coverage again

traits <- all_entities_short %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2630)*100) # usage still 100%

# check apiary

no_apiary <- all_entities_short %>%
  filter(value != "apiary")

# check coverage again

traits <- no_apiary %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2630)*100) # usage still 100%

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

# shrubs with min height > 5m should be trees (Michelle decided)
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(tree = if_else(shrub == 1 & height_min >= 5, 1, tree))

# canopy cover
# all_entities_short$canopy_cover <- "NA"

all_entities_short$canopy_cover <- ifelse(all_entities_short$tree == 1, pi*(all_entities_short$width_max/2)^2,
                                          all_entities_short$canopy_cover)

# round to the nearest whole number
glimpse(all_entities_short)
all_entities_short$canopy_cover <- as.numeric(as.character(all_entities_short$canopy_cover))
all_entities_short$canopy_cover <- ceiling(all_entities_short$canopy_cover)

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
  summarise(frequency = n()) %>%
  arrange(desc(frequency))

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

# replace the '-' with a space
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(trait_name == "common_name", gsub("-", " ", all_entities_short$value_new, perl=TRUE),
                             all_entities_short$value_new))

# some other subs
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(trait_name == "common_name", gsub("Sheoak", "She Oak", all_entities_short$value_new, perl=TRUE, ignore.case = TRUE),
                             all_entities_short$value_new))
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(trait_name == "common_name", gsub("Teatree", "Tea Tree", all_entities_short$value_new, perl=TRUE, ignore.case = TRUE),
                             all_entities_short$value_new))
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(trait_name == "common_name", gsub("Lillypilly", "Lilly Pilly", all_entities_short$value_new, perl=TRUE, ignore.case = TRUE),
                             all_entities_short$value_new))

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
         value_new = if_else(value == "insignificant", "inconspicuous flowers", value_new),
         value_new = if_else(value == "inconspicuous", "inconspicuous flowers", value_new),
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
         value_new = if_else(value == "powerlines", "under powerlines", value_new),
         value_new = if_else(value == "wet", "WSUD", value_new),
         value_new = if_else(value == "container", "garden", value_new),
         value_new = if_else(value == "indoor", "garden", value_new))

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
  mutate(trait_name_new = if_else(trait_name == "soil_type", "soil texture", trait_name_new),
         trait_name_new = if_else(trait_name == "soil_pH", "soil pH", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "acid", "acidic", value_new))

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
         value_new = if_else(value == "protected", "sheltered", value_new),
         value_new = if_else(value == "poorly_drained", "poorly drained soil", value_new),
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
  mutate(value_new = if_else(value == "prostrate", "upright", value_new),
         value_new = if_else(value == "domed", "rounded", value_new),
         value_new = if_else(value == "narrow", "upright", value_new),
         value_new = if_else(value == "oval", "rounded", value_new))

# filter out rest
remove_habit_canopy <- all_entities_short %>%
  filter(trait_name == "habit_canopy" & value == "arborescent" | trait_name == "habit_canopy" & value == "branching" | 
           trait_name == "habit_canopy" & value == "clumping" | trait_name == "habit_canopy" & value == "compact" | 
           trait_name == "habit_canopy" & value == "conical" | trait_name == "habit_canopy" & value == "vase" |
           trait_name == "habit_canopy" & value == "bushy" | trait_name == "habit_canopy" & value == "dense" |
           trait_name == "habit_canopy" & value == "hedging_possible" | trait_name == "habit_canopy" & value == "open" |
           trait_name == "habit_canopy" & value == "variable" | trait_name == "habit_canopy" & value == "weeping") 

# remove
all_entities_short <- anti_join(all_entities_short, remove_habit_canopy)

# RISK
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "mildallergen", "allergen", value_new),
         value_new = if_else(value == "moderateallergen", "allergen", value_new),
         value_new = if_else(value == "poison", "poisonous or toxic", value_new),
         value_new = if_else(value == "severeallergen", "allergen", value_new),
         value_new = if_else(value == "spikey", "spikey or spiny", value_new))

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
  mutate(trait_name_new = if_else(trait_name == "weed_status", "weed status in Australia", trait_name_new))

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

# add Ale's shade and carbon index categories (NEED TO REDO)

# extract info for Ale to recalculate
# co_benefit <- all_entities_short %>%
#   filter(tree == 1) %>%
#   distinct(scientificNameStd, plant_name, .keep_all = TRUE) %>%
#   select(scientificNameStd, plant_name, tree, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)
# 
# write.csv(co_benefit,"Master_database_output/Ale/co_benefit_analysis_ST_12.4.2021.csv",row.names = FALSE)

categories <- read.csv("Master_database_input/Ale/co_benefit_analysis_ST_12.4.2021_AO.csv")

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

# attach the synonyms found through Taxonstand

taxonomy <- read.csv("Master_database_output/taxonomy_checks/taxonstandcheck_ST_1.3.2021.csv")

taxonomy <- taxonomy %>% 
  select(Taxon, Taxonomic.status, New.Genus, New.Species) %>%
  filter(Taxonomic.status == "Synonym") %>%
  mutate(synonym = paste0(New.Genus, " ", New.Species)) %>%
  select(Taxon, synonym)

taxonomy <- taxonomy %>% 
  select(Taxon, Taxonomic.status, New.Genus, New.Species, New.Infraspecific.rank, New.Infraspecific) %>%
  filter(Taxonomic.status == "Synonym") %>%
  mutate(synonym = if_else(New.Infraspecific.rank != "", paste0(New.Genus, " ", New.Species, " ", New.Infraspecific.rank, " ",
                                                                New.Infraspecific), paste0(New.Genus, " ", New.Species))) %>%
  select(Taxon, synonym)

# remove the dots
taxonomy[] <- lapply(taxonomy, gsub, pattern = "var.", replacement = "var")
taxonomy[] <- lapply(taxonomy, gsub, pattern = "subsp.", replacement = "subsp")

# make sure the synonyms are not actually species

plant_name_database <- all_entities_short %>%
  filter(category == "SP") %>%
  distinct(plant_name)

synonym_names <- select(taxonomy, synonym)

same <- inner_join(plant_name_database, synonym_names, by = c("plant_name" = "synonym"))

# add the synonyms (if plants already don't have them)
no_syn <- all_entities_short %>%
  filter(synonym == "NA")

# extract the species from 'taxonomy'
names(taxonomy)[names(taxonomy) == 'Taxon'] <- 'plant_name'

no_syn_taxonomy <- inner_join(taxonomy, no_syn, by = "plant_name")

no_syn_taxonomy$synonym.y <- no_syn_taxonomy$synonym.x

no_syn_taxonomy <- select(no_syn_taxonomy, -synonym.x)
names(no_syn_taxonomy)[names(no_syn_taxonomy) == 'synonym.y'] <- 'synonym'

# remove these species from the database
all_entities_short <- anti_join(all_entities_short, no_syn_taxonomy, by = "plant_name")

# add back together
all_entities_short <- bind_rows(all_entities_short, no_syn_taxonomy)
all_entities_short <- arrange(all_entities_short, scientificNameStd, plant_name, trait_name, value)

#### feedback from the advisory board + remove erroneous columns
## powerlines
# remove 'under powerlines' as a value from the database
all_entities_short <- all_entities_short %>%
  filter(value != "under powerlines")

# remove the 'growth_form' column
all_entities_short <- all_entities_short %>%
  select(-growth_form)

# change 'herb' into 'herbaceous'
names(all_entities_short)[names(all_entities_short) == 'herb'] <- 'herbaceous'

# remove 'gravel' as a 'soil texture'
all_entities_short <- all_entities_short %>%
  filter(value != "gravel")

# fix shade and carbon for Citrus grandis
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(carbon_index = if_else(plant_name == "Citrus grandis", "low", carbon_index),
         shade_index = if_else(plant_name == "Citrus grandis", "low", shade_index))

# fix the gh species drought tolerance
all_entities_short[] <- lapply(all_entities_short, gsub, pattern = "dehydration tolerator", replacement = "tolerator")
all_entities_short[] <- lapply(all_entities_short, gsub, pattern = "dehydration avoider", replacement = "avoider")
all_entities_short[] <- lapply(all_entities_short, gsub, pattern = "intermediate", replacement = "tolerator/avoider")

names(all_entities_short)[names(all_entities_short) == 'dehydration_tolerance'] <- 'drought_strategy'

# try synonyms again
# https://github.com/ropensci/rgbif/issues/289

# library(rgbif)
# 
# plant_names <- all_entities_short %>%
#   filter(category == "SP") %>%
#   distinct(plant_name)
# 
# colnames(plant_names) <- "scientificName"
# 
# write.csv(plant_names,"Master_database_output/plant_names.csv",row.names = FALSE)

# run through this to get key
# https://www.gbif.org/tools/species-lookup
# save output

# load output
# gbif_species <- read.csv("Master_database_input/synonyms/gbif_species.csv")
# 
# syns <- lapply(gbif_species$key[1:1914], name_usage, data = "synonyms")
# 
# x_unlist <- unlist(syns, recursive = FALSE) # https://stackoverflow.com/questions/43591029/convert-nested-list-elements-into-data-frame-and-bind-the-result-into-one-data-f/43592335
# 
# # only want he 'data' elements
# y <- x_unlist[grep("data", names(x_unlist))] # https://stackoverflow.com/questions/39983986/filter-or-subset-list-by-partial-object-name-in-r  
# 
# z <- rbindlist(y, fill = TRUE) # bind the lists and fill in the missing columns
# 
# synonyms <- z %>%
#   select(species, canonicalName, rank, taxonomicStatus) %>%
#   filter(taxonomicStatus == "SYNONYM") %>%
#   filter(rank == "SPECIES") %>%
#   select(species, canonicalName)
# 
# write.csv(synonyms,"Master_database_input/synonyms/gbif_synonyms.csv",row.names = FALSE)

# synonyms

gbif_synonyms <- read.csv("Master_database_input/synonyms/gbif_synonyms.csv")

# add the species names from gbif that are already apparently synonyms
gbif_species <- read.csv("Master_database_input/synonyms/gbif_species.csv")

gbif_species <- gbif_species %>%
  filter(status == "SYNONYM") %>%
  select(verbatimScientificName, species)

names(gbif_species)[names(gbif_species) == 'species'] <- 'canonicalName'
names(gbif_species)[names(gbif_species) == 'verbatimScientificName'] <- 'species'

# join
gbif_synonyms <- bind_rows(gbif_synonyms, gbif_species)

# check the synonyms I already have in the database

syn_already_have <- all_entities_short %>%
  filter(synonym != "NA") %>%
  distinct(plant_name, synonym)

# fix up Myoporum tenuifolium (Myoporum acuminatum, Myoporum montanum) and Abelia uniflora (Abelia engleriana, Abelia schumannii)
syn_already_have <- syn_already_have %>%
  filter(plant_name != "Abelia uniflora" & plant_name != "Myoporum tenuifolium")

syn_already_have <- syn_already_have %>%
  add_row(plant_name = "Abelia uniflora", synonym = "Abelia engleriana")
syn_already_have <- syn_already_have %>%
  add_row(plant_name = "Abelia uniflora", synonym = "Abelia schumannii")
syn_already_have <- syn_already_have %>%
  add_row(plant_name = "Myoporum tenuifolium", synonym = "Myoporum acuminatum")
syn_already_have <- syn_already_have %>%
  add_row(plant_name = "Myoporum tenuifolium", synonym = "Myoporum montanum")

names(syn_already_have)[names(syn_already_have) == 'plant_name'] <- 'species'
names(syn_already_have)[names(syn_already_have) == 'synonym'] <- 'canonicalName'

# add to what I have
gbif_synonyms <- bind_rows(gbif_synonyms, syn_already_have)
gbif_synonyms <- distinct(gbif_synonyms, species, canonicalName)

# filter and fix
gbif_synonyms$species <- as.character(gbif_synonyms$species)
gbif_synonyms$canonicalName <- as.character(gbif_synonyms$canonicalName)

gbif_synonyms <- filter(gbif_synonyms, species != canonicalName)

# make sure the synonyms are not actually species

plant_name_database <- all_entities_short %>%
  filter(category == "SP") %>%
  distinct(plant_name)

synonym_names <- select(gbif_synonyms, canonicalName)
synonym_names <- distinct(synonym_names) # two different species can have the same synonym, why are they not syns of each other?

same <- inner_join(plant_name_database, synonym_names, by = c("plant_name" = "canonicalName"))  
# 16 differ

# filter these out
names(same)[names(same) == 'plant_name'] <- 'canonicalName'

# remove
gbif_synonyms <- anti_join(gbif_synonyms, same)

# check
synonym_names <- select(gbif_synonyms, canonicalName)
synonym_names <- distinct(synonym_names) # two different species can have the same synonym, why are they not syns of each other?

same <- inner_join(plant_name_database, synonym_names, by = c("plant_name" = "canonicalName"))
# all removed

# two species with the same synonym
gbif_synonyms_check <- gbif_synonyms %>%
  group_by(canonicalName) %>%
  summarise(frequency = n())
# I would remove these, there are 39

gbif_synonyms_remove <- gbif_synonyms_check %>%
  filter(frequency > 1) %>%
  select(canonicalName)

# remove
gbif_synonyms <- anti_join(gbif_synonyms, gbif_synonyms_remove)

# there are weird canonicalNames with 'publ' at the end, filter those out
# https://stackoverflow.com/questions/22850026/filter-rows-which-contain-a-certain-string

gbif_synonyms <- filter(gbif_synonyms, !grepl("publ", canonicalName))

# IT IS ALL CLEANED, NOW TO JOIN TOGETHER
# https://stackoverflow.com/questions/38514988/concatenate-strings-by-group-with-dplyr

gbif_synonyms_join <- gbif_synonyms %>%
  group_by(species) %>%
  mutate(synonyms = paste0(canonicalName, collapse = ", ")) %>%
  distinct(species, synonyms)
# we have synonyms for 1294/1914 species!!!!

# join to master database
names(gbif_synonyms_join)[names(gbif_synonyms_join) == 'species'] <- 'plant_name'
names(gbif_synonyms_join)[names(gbif_synonyms_join) == 'synonyms'] <- 'synonym'

all_entities_short <- select(all_entities_short, -synonym)

all_entities_short <- left_join(all_entities_short, gbif_synonyms_join, by = "plant_name")

all_entities_short$synonym[is.na(all_entities_short$synonym)] <- "NA"

# rearrange columns
all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, Koppen_zone, climber, cycad, fern, grass, herbaceous, palm, shrub, succulent, tree, origin, trait_name, value,
         bird, insect, lizard, native_mammal, pollinator, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index, drought_strategy, heat_tolerance)

# internal beta testing feedback
# Michelle: change 'grass' to 'grass-like'
names(all_entities_short)[names(all_entities_short) == 'grass'] <- 'grass-like'


write.csv(all_entities_short,"Master_database_output/FINAL/trait_database_ST_FINAL_26.4.2021_vers1.6.csv",row.names=FALSE)


# more fail safes

# species with no drought tolerance and low water needs
no_drought_tolerance <- all_entities_short %>%
  filter(trait_name == "drought tolerance") %>%
  filter(value == "putatively no") %>%
  distinct(plant_name)

low_water_needs <- all_entities_short %>%
  filter(trait_name == "planting and maintenance") %>%
  filter(value == "low water needs") %>%
  distinct(plant_name)

same <- inner_join(low_water_needs, no_drought_tolerance, by = "plant_name")
# 28 species, fixed them up

# check what I did previously....
high_drought_tolerance <- all_entities_short %>%
  filter(trait_name == "drought tolerance") %>%
  filter(value == "putatively high") %>%
  distinct(plant_name)

high_water_needs <- all_entities_short %>%
  filter(trait_name == "planting and maintenance") %>%
  filter(value == "high water needs") %>%
  distinct(plant_name)

same <- inner_join(high_water_needs, high_drought_tolerance, by = "plant_name")
# 0 plants, phew!!!

# species that are full sun and full shade
full_shade <- all_entities_short %>%
  filter(value == "full shade") %>%
  distinct(plant_name)

full_sun <- all_entities_short %>%
  filter(value == "full sun") %>%
  distinct(plant_name)

same <- inner_join(full_sun, full_shade, by = "plant_name") # 147 plants

shade_check <- left_join(same, all_entities_short, by = "plant_name")

shade_check <- shade_check %>%
  filter(trait_name == "shade tolerance") %>%
  group_by(plant_name) %>%
  summarise(frequency = n()) # 8/147 plants are full sun and full shade, fixed them up

# species with risk and playground friendly
risk <- all_entities_short %>%
  filter(trait_name == "risk") %>%
  distinct(plant_name)

playground <- all_entities_short %>%
  filter(value == "playground friendly") %>%
  distinct(plant_name)

same <- inner_join(risk, playground, by = "plant_name") # 12 plants, fixed them up

# species as both herbs and trees
# checked manually, no plants are herbs and trees

# species as both herb and shrub
# checked manually, no plants are herbs and shrubs

# species with high and low water needs
high_water <- all_entities_short %>%
  filter(value == "high water needs") %>%
  distinct(plant_name)

low_water <- all_entities_short %>%
  filter(value == "low water needs") %>%
  distinct(plant_name)

same <- inner_join(low_water, high_water, by = "plant_name") # 0 plants

# species with slow and fast growth rate
slow_growth <- all_entities_short %>%
  filter(value == "slow") %>%
  distinct(plant_name)

fast_growth <- all_entities_short %>%
  filter(value == "fast") %>%
  distinct(plant_name)

same <- inner_join(fast_growth, slow_growth, by = "plant_name") # 60 plants

growth_check <- left_join(same, all_entities_short, by = "plant_name")

growth_check <- growth_check %>%
  filter(trait_name == "growth rate") %>%
  group_by(plant_name) %>%
  summarise(frequency = n()) # 30/60 plants, fixed them up

# species that are deciduous and evergreen
deciduous <- all_entities_short %>%
  filter(value == "deciduous") %>%
  distinct(plant_name)

evergreen <- all_entities_short %>%
  filter(value == "evergreen") %>%
  distinct(plant_name)

same <- inner_join(evergreen, deciduous, by = "plant_name") # 32 plants

leaf_check <- left_join(same, all_entities_short, by = "plant_name")

leaf_check <- leaf_check %>%
  filter(trait_name == "leaf loss") %>%
  group_by(plant_name) %>%
  summarise(frequency = n()) # 19/32 plants, fixed them up

## do box plots of min, max and average heights and widths of all the growth forms

library(ggplot2)

boxplot_data <- all_entities_short %>%
  select(plant_name, climber:tree, trait_name, value) %>%
  filter(trait_name == "average height" | trait_name == "average width" | trait_name == "maximum height" | 
           trait_name == "maximum width" | trait_name == "minimum height" | trait_name == "minimum width")


glimpse(boxplot_data)
boxplot_data$value <- as.numeric(as.character(boxplot_data$value))
boxplot_data$trait_name <- as.factor(boxplot_data$trait_name)
boxplot_data[,'trait_name'] <- factor(boxplot_data[,'trait_name'], levels = c("minimum height", "average height", "maximum height", 
                                                                              "minimum width", "average width", "maximum width"))
# climber
climber <- boxplot_data %>%
  filter(climber == 1)

climber_plot <- ggplot(climber, aes(x = trait_name, y = value)) +
  geom_boxplot() +
  labs(title = "climber")

climber_plot

# cycad
cycad <- boxplot_data %>%
  filter(cycad == 1)

cycad_plot <- ggplot(cycad, aes(x = trait_name, y = value)) +
  geom_boxplot() +
  labs(title = "cycad")

cycad_plot

# fern
fern <- boxplot_data %>%
  filter(fern == 1)

fern_plot <- ggplot(fern, aes(x = trait_name, y = value)) +
  geom_boxplot() +
  labs(title = "fern")

fern_plot

# grass-like
names(boxplot_data)[names(boxplot_data) == 'grass-like'] <- 'grass'

grass <- boxplot_data %>%
  filter(grass == 1)

grass_plot <- ggplot(grass, aes(x = trait_name, y = value)) +
  geom_boxplot() +
  labs(title = "grass-like")

grass_plot # might need some fixing

# herbaceous
herb <- boxplot_data %>%
  filter(herbaceous == 1)

herb_plot <- ggplot(herb, aes(x = trait_name, y = value)) +
  geom_boxplot() +
  labs(title = "herbaceous")

herb_plot # might need some fixing

# palm
palm <- boxplot_data %>%
  filter(palm == 1)

palm_plot <- ggplot(palm, aes(x = trait_name, y = value)) +
  geom_boxplot() +
  labs(title = "palm")

palm_plot # need to look at

# shrub
shrub <- boxplot_data %>%
  filter(shrub == 1)

shrub_plot <- ggplot(shrub, aes(x = trait_name, y = value)) +
  geom_boxplot() +
  labs(title = "shrub")

shrub_plot

# succulent
succulent <- boxplot_data %>%
  filter(succulent == 1)

succulent_plot <- ggplot(succulent, aes(x = trait_name, y = value)) +
  geom_boxplot() +
  labs(title = "succulent")

succulent_plot # check

# tree
tree <- boxplot_data %>%
  filter(tree == 1)

tree_plot <- ggplot(tree, aes(x = trait_name, y = value)) +
  geom_boxplot() +
  labs(title = "tree")

tree_plot

####### extract updated species list for Ale
names(all_entities_short)[names(all_entities_short) == 'grass-like'] <- 'grass'

updated_list <- all_entities_short %>%
  filter(category == "SP") %>%
  select(scientificNameStd, climber, cycad, fern, grass, herbaceous, palm, shrub, succulent, tree) %>%
  distinct(scientificNameStd, .keep_all = TRUE)

write.csv(updated_list,"Master_database_output/Farzin/species_list_ST_5May2021.csv",row.names=FALSE)

######### extract the biodiversity values for Paul

biodiversity <- all_entities_short %>%
  select(plant_name, climber, cycad, fern, grass, herbaceous, palm, shrub, succulent, tree, bird, insect, lizard, native_mammal, pollinator, trait_name, value) %>%
  filter(trait_name == "flower colour") %>%
  distinct(plant_name, trait_name, value, .keep_all = TRUE)

write.csv(biodiversity,"Master_database_output/Paul/growth_form_biodiversity_flower_colour.csv", row.names = FALSE)

######### extract plant names for Gwilym Griffiths

gwilym_species <- all_entities_short %>%
  distinct(plant_name)

write.csv(gwilym_species,"Master_database_output/Gwilym/WPW_plant_list.csv", row.names = FALSE)

##########
##########

########## check the arboretum list with our photo list to see which photos we are missing

library(tidyverse)

arboretum <- read.csv("Master_database_input/photos/mq_arboretum.csv")

photo_list <- read.csv("Master_database_input/photos/species_list_photos_13May2021.csv")

# check all the species are there
photo_list_species <- photo_list %>%
  distinct(plant_name) # extra species

# what's different
diff <- anti_join(gwilym_species, photo_list_species, by = "plant_name") # all plants accounted for!
diff <- anti_join(photo_list_species, gwilym_species, by = "plant_name") # all plants accounted for!

# match the arboretum species with the db species
names(arboretum)[names(arboretum) == 'Species'] <- 'plant_name'

arboretum_species <- arboretum %>%
  select(plant_name)

same_species <- inner_join(photo_list_species, arboretum_species, by = "plant_name")

# join the location info
same_species <- left_join(same_species, arboretum, by = "plant_name")

# join the photo notes info
photo_notes <- photo_list %>%
  select(plant_name, Notes)

same_species <- left_join(same_species, photo_notes, by = "plant_name")

same_species <- distinct(same_species, plant_name, Location.on.campus, Notes)

same_species <- arrange(same_species, Location.on.campus)

write.csv(same_species,"Master_database_output/photos/arboretum_species.csv", row.names = FALSE)

# species that don't match up (try and rescue the cultivars)

diff_species <- anti_join(arboretum_species, photo_list_species, by = "plant_name")

### I have added these in manually

#### match list of photo names with the metadata

metadata <- read.csv("Master_database_input/photos/species_list_photos_13May2021.csv")

list_names <- read.csv("Master_database_input/photos/disused_file_photo_list_13May2021.csv") # 1474 entries

metadata <- metadata %>%
  filter(uploaded_to_sharedrive == "yes" | uploaded_to_sharedrive == "Yes") %>%
  select(New.file.name) #1463 entries

# find differences

diff <- setdiff(list_names, metadata)
diff2 <- setdiff(metadata, list_names) # all sorted.

# check how many plants we have images for
metadata <- read.csv("Master_database_input/photos/species_list_photos_13May2021.csv")

images <- metadata %>%
  filter(uploaded_to_sharedrive == "yes" | uploaded_to_sharedrive == "Yes") %>%
  distinct(plant_name) # 784 plants!

#########
#########

##### biodiversity calculations

# load Ale's tree hollow species

WPW_plant_list <- read.csv("Master_database_output/Gwilym/WPW_plant_list.csv")

hollows <- read.csv("Master_database_input/Ale/tree_hollow_species.csv")

hollows <- select(hollows, Species)

names(hollows)[names(hollows) == 'Species'] <- 'plant_name'

# compare with WPW species list

same <- inner_join(WPW_plant_list, hollows, by = "plant_name")

same <- distinct(same, plant_name)

write.csv(same,"Master_database_output/ecological_value/hollow_bearing_species.csv", row.names = FALSE)

# have a look at data on fruit

fruit <- bind_rows(everything, everything_gh)

fruit <- fruit %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  select(species, trait_name, value) %>%
  filter(trait_name == "fruit_colour") %>%
  distinct(species, trait_name, value)

names(fruit)[names(fruit) == 'species'] <- 'plant_name'

write.csv(fruit,"Master_database_output/ecological_value/fruit_colour.csv", row.names = FALSE)

# check AI with hort classifications of drought (according to Ale, ai is actually PET (potential evapotranspiration))

drought_class <- all_entities_short %>%
  filter(exp_tested == "N") %>%
  filter(trait_name == "drought tolerance") %>%
  select(plant_name, value)

names(drought_class)[names(drought_class) == 'value'] <- 'drought_tolerance'

# load Ale data

Ale_ai <- read.csv("Master_database_input/Ale/niche_summaries_5414_species.csv")

Ale_ai <- Ale_ai %>%
  filter(var == "ai") %>%
  select(speciesName, p5, mean, p95)

names(Ale_ai)[names(Ale_ai) == 'speciesName'] <- 'plant_name'
names(Ale_ai)[names(Ale_ai) == 'p5'] <- 'PET_min'
names(Ale_ai)[names(Ale_ai) == 'mean'] <- 'PET_mean'
names(Ale_ai)[names(Ale_ai) == 'p95'] <- 'PET_max'

# Ale_ai <- Ale_ai %>%
#   mutate(AI_min = p0/1000,
#          AI_mean = mean/1000,
#          AI_max = p100/1000) %>%
#   select(plant_name, AI_min, AI_mean, AI_max)

# make into long format
# http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
Ale_ai_long <- gather(Ale_ai, PET_type, PET, PET_min:PET_max)

# join together

ai_drought_class <- inner_join(drought_class, Ale_ai_long, by = "plant_name")

# graph

library(ggplot2)

plot <- ggplot(ai_drought_class, aes(x = drought_tolerance, y = PET, fill = PET_type)) +
  geom_boxplot()
plot

# anova to see if things are statistically different
# http://www.sthda.com/english/wiki/one-way-anova-test-in-r
# extract just the PET_max data
PET_max <- ai_drought_class %>%
  filter(PET_type == "PET_max") %>%
  select(-PET_type, -plant_name)
names(PET_max)[names(PET_max) == 'PET'] <- 'PET_max'

glimpse(PET_max)
PET_max$drought_tolerance <- as.factor(PET_max$drought_tolerance)
levels(PET_max$drought_tolerance)
PET_max$drought_tolerance <- ordered(PET_max$drought_tolerance, levels = c("putatively no", "putatively moderate", "putatively high"))

res.aov <- aov(PET_max ~ drought_tolerance, data = PET_max)
summary(res.aov) # there is a significant difference

TukeyHSD(res.aov) # all are significantly different

# check assumptions
plot(res.aov, 1)
library(car)
leveneTest(PET_max ~ drought_tolerance, data = PET_max) # no evidence that variance across groups is significantly different

##################################################################################################

############## Version 1.7 (final upload)

#### Things that have changed
# removed several inappropriate species
# removed water requirements from 'planting and maintenance'
# Remove drought tolerance for glasshouse species
# Added 'timber' as a 'use'
# Removed 'habitat' as a 'use'
# Removed 'model_type' column
# Removed 'Koppen_zone' column
# changed the column names for 'biodiversity' functions

#### Things to do
# check weeds with Michelle

library(tidyverse)

everything <- read.csv("Master_database_input/EVERYTHING_traits_17Jun2021.csv")

everything_gh <- read.csv("Master_database_input/EVERYTHING_gh_17Jun2021.csv")

all_entities <- bind_rows(everything, everything_gh)

all_entities_short <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value) %>%
  distinct(scientificNameStd, species, category, exp_tested, trait_name, value)

check <- distinct(all_entities_short, species) # 2605 entities

# remove the old height and width dimensions

all_entities_short <- all_entities_short %>%
  filter(trait_name != "max_height", trait_name != "height", trait_name != "min_height", 
         trait_name != "max_width", trait_name != "width", trait_name != "min_width")

check <- distinct(all_entities_short, species) # 2605 entities, haven't lost anything

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

# change min, max and average values into integars
height_width <- height_width %>%
  mutate_if(is.factor, as.character) %>%
  mutate(max_new = if_else(max > 1, round(max), max),
         min_new = if_else(min > 1, round(min), min),
         average_new = if_else(average > 1, round(average), average))

height_width <- select(height_width, -max, -min, -average)

names(height_width)[names(height_width) == 'max_new'] <- 'max'
names(height_width)[names(height_width) == 'min_new'] <- 'min'
names(height_width)[names(height_width) == 'average_new'] <- 'average'

glimpse(height_width)
height_width$average <- as.numeric(as.character(height_width$average))

filter(height_width, range < 0) # no mistakes

names <- distinct(height_width, species) # 2605, am not missing anything!

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

check <- distinct(all_entities_short, species) # 2605 entities, haven't lost anything

### do drought classifications

drought <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  filter(trait_name == "drought_tolerance") %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value)

# make all the values upper case
drought$value <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", drought$value, perl=TRUE)

drought_summary <- drought %>%
  group_by(scientificNameStd, species) %>%
  summarise(number_records = n())
# 1799 records

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
# 1875 records

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
# 945 records

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
  distinct(species, plantType, origin) # 2605 entities

# join to main dataset
all_entities_short <- left_join(all_entities_short, plant_type_origin, by = "species")

all_entities_short <- all_entities_short %>%
  select(scientificNameStd, species, plantType, origin, category, exp_tested, trait_name, value) %>%
  filter(trait_name != "native_exotic")

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

# rearrange all the columns
all_entities_short <- select(all_entities_short, scientificNameStd, family, genus, species, entity, synonym, model_type, plantType, climber, 
                             cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, category, exp_tested, trait_name, value,
                             canopy_cover, shade_value, shade_index, carbon_value, carbon_index)

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
                             height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)

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

## add parents of GC, H, HC

parents <- read.csv("Master_database_input/hybrids_genus_cultivars_parents_new.csv")

parents <- select(parents, -plantType)

names(parents)[names(parents) == 'Species'] <- 'entity'

# join
found <- left_join(found, parents, by = "entity")

# rearrange the columns

found <- found %>%
  select(-frequency) %>%
  select(scientificNameStd, family, genus, species, entity, synonym, Parent_1, Parent_2,Parent_3, Parent_4, model_type, plantType, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, category, exp_tested, trait_name, value,
        height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)

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

# remove H, HC, GC from database

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
         height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)

# join back the H, HC, GCs

all_entities_short <- bind_rows(all_entities_short, found)

all_entities_short <- arrange(all_entities_short, entity, trait_name, value)

# some synonyms with missing family
syn <- all_entities_short %>%
  filter(family == "") %>%
  select(entity) %>%
  distinct(entity) # 17

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
         family = if_else(entity == "Regelia velutina", "Myrtaceae", family),
         family = if_else(entity == "Citrus maxima", "Rutaceae", family),
         family = if_else(genus == "Syzygium", "Myrtaceae", family),
         family = if_else(genus == "Aloe", "Asphodelaceae", family))

# check

syn_check <- filter(all_entities_short, family == "") # all fixed

# filter out cultivars
# check if sciname = entity
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
  distinct(entity) # 162 synonyms which is 164 from original minus the two species I combined

# change entity to plant name
names(all_entities_short)[names(all_entities_short) == 'entity'] <- 'plant_name'

##### remove medicinal and apiary from usage

# first check coverage of traits

species <- all_entities_short %>%
  distinct(plant_name)
# 2593 entities

traits <- all_entities_short %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2593)*100) # all relevant traits still 100%

# remove medicinal from usage

all_entities_short <- all_entities_short %>%
  filter(value != "medicinal")

# check coverage again

traits <- all_entities_short %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2593)*100) # usage still 100%

# check apiary

no_apiary <- all_entities_short %>%
  filter(value != "apiary")

# check coverage again

traits <- no_apiary %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2593)*100) # usage still 100%

# remove apiary from dataset as a usage
all_entities_short <- all_entities_short %>%
  filter(value != "apiary")

# rearrange columns to make more sense
all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, plantType, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, value,
         height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)

# need to calculate co-benefits
all_entities_short$width_max <- as.numeric(as.character(all_entities_short$width_max))
all_entities_short$width_min <- as.numeric(as.character(all_entities_short$width_min))
all_entities_short$height_max <- as.numeric(as.character(all_entities_short$height_max))
all_entities_short$height_min <- as.numeric(as.character(all_entities_short$height_min))
all_entities_short$tree <- as.numeric(as.character(all_entities_short$tree))
all_entities_short$shrub <- as.numeric(as.character(all_entities_short$shrub))

# shrubs with min height > 5m should be trees (Michelle decided)
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(tree = if_else(shrub == 1 & height_min >= 5, 1, tree))

# canopy cover
# all_entities_short$canopy_cover <- "NA"

all_entities_short$canopy_cover <- ifelse(all_entities_short$tree == 1, pi*(all_entities_short$width_max/2)^2,
                                          all_entities_short$canopy_cover)

# round to the nearest whole number
glimpse(all_entities_short)
all_entities_short$canopy_cover <- as.numeric(as.character(all_entities_short$canopy_cover))
all_entities_short$canopy_cover <- round(all_entities_short$canopy_cover)

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

all_entities_short$canopy_cover[is.na(all_entities_short$canopy_cover)] <- "NA"
all_entities_short$shade_value[is.na(all_entities_short$shade_value)] <- "NA"
all_entities_short$carbon_value[is.na(all_entities_short$carbon_value)] <- "NA"

# add the data that Paul and Sally sent

biodiversity <- read.csv("Master_database_input/biodiversity/Biodiversity_values_PR_Sp_17Jun2017.csv")

# fix up some mistakes
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Acacia longifolia subsp Longifolia", replacement = "Acacia longifolia subsp longifolia")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Alyogne huegelii Delightfully Double", replacement = "Alyogyne huegelii Delightfully Double")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Lagerstroemia indica Commanchee", replacement = "Lagerstroemia indica Commanche")

biodiversity <- biodiversity %>%
  add_row(plant_name = "Syzygium australe", insect = "1", bird = "1", mammal_lizard = "0", animal_pollinated = "1",
          habitat = "1", biodiversity_value = "4")

# join to main database
all_entities_short <- left_join(all_entities_short, biodiversity, by = "plant_name")

# rearrange columns
all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, plantType, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, value,
         bird, insect, mammal_lizard, animal_pollinated, habitat, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)

############

# FINAL FORMAT OF THE DATABASE

# first check glasshouse species

gh <- all_entities_short %>%
  distinct(plant_name, exp_tested) %>%
  group_by(exp_tested) %>%
  summarise(frequency = n())
# 116 plants from glasshouse

# remove plantType column

all_entities_short <- select(all_entities_short, -plantType)

# select the traits that we want

all_entities_short <- all_entities_short %>%
  filter(trait_name == "common_name" | trait_name == "flower_colour" | trait_name == "flower_period" | trait_name == "leaf_loss" 
         | trait_name == "light_level" | trait_name == "placement" | trait_name == "usage" | trait_name == "height_max" 
         | trait_name == "height_min" | trait_name == "height_average" | trait_name == "width_max" | trait_name == "width_min" | trait_name == "width_average"
         | trait_name == "soil_type" | trait_name == "soil_pH" | trait_name == "ideal_conditions" | trait_name == "frost_tolerance" | trait_name == "drought_tolerance" 
         | trait_name == "coastal_tolerance" | trait_name == "habit_canopy" | trait_name == "growth_rate" | trait_name == "foliage_colour" | trait_name == "risk" 
         | trait_name == "weed_status")

# check they are all there
trait_name_check <- all_entities_short %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency))

# create new columns to change the trait_name and value
all_entities_short$trait_name_new <- all_entities_short$trait_name
all_entities_short$value_new <- all_entities_short$value

# rearrange columns to make more sense
all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, trait_name_new, value,
         value_new, bird, insect, mammal_lizard, animal_pollinated, habitat, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)

# COMMON NAME
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "common_name", "common name", trait_name_new))

# replace the '-' with a space
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(trait_name == "common_name", gsub("-", " ", all_entities_short$value_new, perl=TRUE),
                             all_entities_short$value_new))

# some other subs
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(trait_name == "common_name", gsub("Sheoak", "She Oak", all_entities_short$value_new, perl=TRUE, ignore.case = TRUE),
                             all_entities_short$value_new))
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(trait_name == "common_name", gsub("Teatree", "Tea Tree", all_entities_short$value_new, perl=TRUE, ignore.case = TRUE),
                             all_entities_short$value_new))
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(trait_name == "common_name", gsub("Lillypilly", "Lilly Pilly", all_entities_short$value_new, perl=TRUE, ignore.case = TRUE),
                             all_entities_short$value_new))

# make common names capitalised
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(trait_name == "common_name", gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", all_entities_short$value_new, perl=TRUE),
                             all_entities_short$value_new))

# FLOWER COLOUR
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "flower_colour", "flower colour", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "gold", "yellow", value_new),
         value_new = if_else(value == "golden", "yellow", value_new),
         value_new = if_else(value == "grey", "black", value_new), 
         value_new = if_else(value == "insignificant", "inconspicuous flowers", value_new),
         value_new = if_else(value == "inconspicuous", "inconspicuous flowers", value_new),
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
         value_new = if_else(value == "powerlines", "under powerlines", value_new),
         value_new = if_else(value == "wet", "WSUD", value_new),
         value_new = if_else(value == "container", "garden", value_new),
         value_new = if_else(value == "indoor", "garden", value_new))

# USAGE
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "usage", "uses", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "cutflower", "cut flowers", value_new),
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
  mutate(trait_name_new = if_else(trait_name == "soil_type", "soil texture", trait_name_new),
         trait_name_new = if_else(trait_name == "soil_pH", "soil pH", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "acid", "acidic", value_new))

# PLANTING AND MAINTENANCE
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "ideal_conditions", "planting and maintenance", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "fertile", "fertile soil", value_new),
         value_new = if_else(value == "fertile", "fertile soil", value_new),
         value_new = if_else(value == "lateral_space", "lateral space", value_new),
         value_new = if_else(value == "protected", "sheltered", value_new),
         value_new = if_else(value == "poorly_drained", "poorly drained soil", value_new),
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
  mutate(value_new = if_else(value == "prostrate", "upright", value_new),
         value_new = if_else(value == "domed", "rounded", value_new),
         value_new = if_else(value == "narrow", "upright", value_new),
         value_new = if_else(value == "oval", "rounded", value_new))

# filter out rest
remove_habit_canopy <- all_entities_short %>%
  filter(trait_name == "habit_canopy" & value == "arborescent" | trait_name == "habit_canopy" & value == "branching" | 
           trait_name == "habit_canopy" & value == "clumping" | trait_name == "habit_canopy" & value == "compact" | 
           trait_name == "habit_canopy" & value == "conical" | trait_name == "habit_canopy" & value == "vase" |
           trait_name == "habit_canopy" & value == "bushy" | trait_name == "habit_canopy" & value == "dense" |
           trait_name == "habit_canopy" & value == "hedging_possible" | trait_name == "habit_canopy" & value == "open" |
           trait_name == "habit_canopy" & value == "variable" | trait_name == "habit_canopy" & value == "weeping") 

# remove
all_entities_short <- anti_join(all_entities_short, remove_habit_canopy)

# RISK
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "mildallergen", "allergen", value_new),
         value_new = if_else(value == "moderateallergen", "allergen", value_new),
         value_new = if_else(value == "poison", "poisonous or toxic", value_new),
         value_new = if_else(value == "severeallergen", "allergen", value_new),
         value_new = if_else(value == "spikey", "spikey or spiny", value_new))

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
  mutate(trait_name_new = if_else(trait_name == "weed_status", "weed status in Australia", trait_name_new))

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
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name_new, value_new,
         bird, insect, mammal_lizard, animal_pollinated, habitat, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)

# end need to distinct everything
all_entities_short <- all_entities_short %>%
  distinct(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name_new,
           value_new, bird, insect, mammal_lizard, animal_pollinated, habitat, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)

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
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, value,
         bird, insect, mammal_lizard, animal_pollinated, habitat, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)

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

# sdm <- read.csv("Master_database_input/LindaFarzin/diff_ST_march_and_min5traits_good_10_03_2021_Clean.csv", strip.white = TRUE)
# get rid of empty cells with white space
# https://stackoverflow.com/questions/2261079/how-can-i-trim-leading-and-trailing-white-space

# sdm_possible <- sdm %>%
  # select(SDM.is.possible) %>%
  # filter(SDM.is.possible != "")
# out of 1914 species, 1469 have data to do sdm, which means 445 are missing

# colnames(sdm_possible) <- "speciesName"

# species with missing sdm
# sdm_missing <- sdm %>%
#   filter(SDM.is.possible == "") %>%
#   select(species_list_ST_1Mar2021.csv)
# 
# colnames(sdm_missing) <- "speciesName"

# load ale's niche data

# niche <- read.csv("Master_database_input/Ale/niche_summaries_5414_species.csv")

# niche <- niche %>%
  # distinct(speciesName)

# diff between missing sdm and niche list
# diff_missing_sdm_niche <- setdiff(sdm_missing, niche)
# 77 species do not have a sdm or niche

# diff between models that do not have sdm or niche data and models that do not have sdm data
# diff_niches <- setdiff(sdm_missing, diff_missing_sdm_niche)
# 368 species have niche data but not sdm

# populate model type
# diff_niches$model_type <- "niche"
# diff_missing_sdm_niche$model_type <- "NA"
# sdm_possible$model_type <- "sdm"

# join together
# model_type <- bind_rows(diff_niches, diff_missing_sdm_niche, sdm_possible)

# join to database
# all_entities_short <- select(all_entities_short, -model_type)
# names(model_type)[names(model_type) == 'speciesName'] <- 'plant_name'
# 
# all_entities_short <- left_join(all_entities_short, model_type, by = "plant_name")
# all_entities_short$model_type[is.na(all_entities_short$model_type)] <- "NA"
# all_entities_short$scientificNameStd[is.na(all_entities_short$scientificNameStd)] <- "NA"
# all_entities_short$species[is.na(all_entities_short$species)] <- "NA"
# all_entities_short$canopy_cover[is.na(all_entities_short$canopy_cover)] <- "NA"
# all_entities_short$shade_value[is.na(all_entities_short$shade_value)] <- "NA"
# all_entities_short$carbon_value[is.na(all_entities_short$carbon_value)] <- "NA"

# rearrange columns

all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, value,
         bird, insect, mammal_lizard, animal_pollinated, habitat, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index, dehydration_tolerance, heat_tolerance)

# check that cultivar synonyms are all accounted for
syn_check <- all_entities_short %>%
  filter(scientificNameStd != plant_name)

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(synonym = if_else(plant_name == "Magnolia champaca", "Michelia champaca", synonym),
         synonym = if_else(plant_name == "Chenopodium candolleanum", "Rhagodia candolleana", synonym),
         synonym = if_else(plant_name == "Cassia artemisioides", "Senna artemisioides", synonym))

# add Ale's shade and carbon index categories (NEED TO REDO)

# extract info for Ale to recalculate
# co_benefit <- all_entities_short %>%
#   filter(tree == 1) %>%
#   distinct(scientificNameStd, plant_name, .keep_all = TRUE) %>%
#   select(scientificNameStd, plant_name, tree, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)
# 
# write.csv(co_benefit,"Master_database_output/Ale/co_benefit_analysis_ST_21.6.2021.csv",row.names = FALSE)

categories <- read.csv("Master_database_input/Ale/co_benefit_analysis_ST_21.6.2021_AO.csv")

# join to main database
all_entities_short <- select(all_entities_short, -shade_index, -carbon_index)

all_entities_short <- left_join(all_entities_short, categories, by = "plant_name")

glimpse(all_entities_short)

all_entities_short$shade_index <- as.character(all_entities_short$shade_index)
all_entities_short$carbon_index <- as.character(all_entities_short$carbon_index)

all_entities_short$shade_index[is.na(all_entities_short$shade_index)] <- "NA"
all_entities_short$carbon_index[is.na(all_entities_short$carbon_index)] <- "NA"

# rearrange columns
all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, value,
         bird, insect, mammal_lizard, animal_pollinated, habitat, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index, dehydration_tolerance, heat_tolerance)

# attach the synonyms found through Taxonstand

taxonomy <- read.csv("Master_database_output/taxonomy_checks/taxonstandcheck_ST_1.3.2021.csv")

taxonomy <- taxonomy %>% 
  select(Taxon, Taxonomic.status, New.Genus, New.Species) %>%
  filter(Taxonomic.status == "Synonym") %>%
  mutate(synonym = paste0(New.Genus, " ", New.Species)) %>%
  select(Taxon, synonym)

taxonomy <- taxonomy %>% 
  select(Taxon, Taxonomic.status, New.Genus, New.Species, New.Infraspecific.rank, New.Infraspecific) %>%
  filter(Taxonomic.status == "Synonym") %>%
  mutate(synonym = if_else(New.Infraspecific.rank != "", paste0(New.Genus, " ", New.Species, " ", New.Infraspecific.rank, " ",
                                                                New.Infraspecific), paste0(New.Genus, " ", New.Species))) %>%
  select(Taxon, synonym)

# remove the dots
taxonomy[] <- lapply(taxonomy, gsub, pattern = "var.", replacement = "var")
taxonomy[] <- lapply(taxonomy, gsub, pattern = "subsp.", replacement = "subsp")

# make sure the synonyms are not actually species

plant_name_database <- all_entities_short %>%
  filter(category == "SP") %>%
  distinct(plant_name)

synonym_names <- select(taxonomy, synonym)

same <- inner_join(plant_name_database, synonym_names, by = c("plant_name" = "synonym"))

# add the synonyms (if plants already don't have them)
no_syn <- all_entities_short %>%
  filter(synonym == "NA")

# extract the species from 'taxonomy'
names(taxonomy)[names(taxonomy) == 'Taxon'] <- 'plant_name'

no_syn_taxonomy <- inner_join(taxonomy, no_syn, by = "plant_name")

no_syn_taxonomy$synonym.y <- no_syn_taxonomy$synonym.x

no_syn_taxonomy <- select(no_syn_taxonomy, -synonym.x)
names(no_syn_taxonomy)[names(no_syn_taxonomy) == 'synonym.y'] <- 'synonym'

# remove these species from the database
all_entities_short <- anti_join(all_entities_short, no_syn_taxonomy, by = "plant_name")

# add back together
all_entities_short <- bind_rows(all_entities_short, no_syn_taxonomy)
all_entities_short <- arrange(all_entities_short, scientificNameStd, plant_name, trait_name, value)

#### feedback from the advisory board + remove erroneous columns
## powerlines
# remove 'under powerlines' as a value from the database
all_entities_short <- all_entities_short %>%
  filter(value != "under powerlines")

# change 'herb' into 'herbaceous'
names(all_entities_short)[names(all_entities_short) == 'herb'] <- 'herbaceous'

# remove 'gravel' as a 'soil texture'
all_entities_short <- all_entities_short %>%
  filter(value != "gravel")

# fix the gh species drought tolerance
all_entities_short[] <- lapply(all_entities_short, gsub, pattern = "dehydration tolerator", replacement = "tolerator")
all_entities_short[] <- lapply(all_entities_short, gsub, pattern = "dehydration avoider", replacement = "avoider")

names(all_entities_short)[names(all_entities_short) == 'dehydration_tolerance'] <- 'drought_strategy'

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(drought_strategy = if_else(drought_strategy == "intermediate", "tolerator/avoider", drought_strategy))

# try synonyms again
# https://github.com/ropensci/rgbif/issues/289

# library(rgbif)
# 
# plant_names <- all_entities_short %>%
#   filter(category == "SP") %>%
#   distinct(plant_name)
# 
# colnames(plant_names) <- "scientificName"
# 
# write.csv(plant_names,"Master_database_output/plant_names.csv",row.names = FALSE)

# run through this to get key
# https://www.gbif.org/tools/species-lookup
# save output

# load output
# gbif_species <- read.csv("Master_database_input/synonyms/gbif_species.csv")
# 
# syns <- lapply(gbif_species$key[1:1914], name_usage, data = "synonyms")
# 
# x_unlist <- unlist(syns, recursive = FALSE) # https://stackoverflow.com/questions/43591029/convert-nested-list-elements-into-data-frame-and-bind-the-result-into-one-data-f/43592335
# 
# # only want he 'data' elements
# y <- x_unlist[grep("data", names(x_unlist))] # https://stackoverflow.com/questions/39983986/filter-or-subset-list-by-partial-object-name-in-r  
# 
# z <- rbindlist(y, fill = TRUE) # bind the lists and fill in the missing columns
# 
# synonyms <- z %>%
#   select(species, canonicalName, rank, taxonomicStatus) %>%
#   filter(taxonomicStatus == "SYNONYM") %>%
#   filter(rank == "SPECIES") %>%
#   select(species, canonicalName)
# 
# write.csv(synonyms,"Master_database_input/synonyms/gbif_synonyms.csv",row.names = FALSE)

# synonyms

gbif_synonyms <- read.csv("Master_database_input/synonyms/gbif_synonyms.csv")

# add the species names from gbif that are already apparently synonyms
gbif_species <- read.csv("Master_database_input/synonyms/gbif_species.csv")

gbif_species <- gbif_species %>%
  filter(status == "SYNONYM") %>%
  select(verbatimScientificName, species)

names(gbif_species)[names(gbif_species) == 'species'] <- 'canonicalName'
names(gbif_species)[names(gbif_species) == 'verbatimScientificName'] <- 'species'

# join
gbif_synonyms <- bind_rows(gbif_synonyms, gbif_species)

# check the synonyms I already have in the database

syn_already_have <- all_entities_short %>%
  filter(synonym != "NA") %>%
  distinct(plant_name, synonym)

# fix up Myoporum tenuifolium (Myoporum acuminatum, Myoporum montanum) and Abelia uniflora (Abelia engleriana, Abelia schumannii)
syn_already_have <- syn_already_have %>%
  filter(plant_name != "Abelia uniflora" & plant_name != "Myoporum tenuifolium")

syn_already_have <- syn_already_have %>%
  add_row(plant_name = "Abelia uniflora", synonym = "Abelia engleriana")
syn_already_have <- syn_already_have %>%
  add_row(plant_name = "Abelia uniflora", synonym = "Abelia schumannii")
syn_already_have <- syn_already_have %>%
  add_row(plant_name = "Myoporum tenuifolium", synonym = "Myoporum acuminatum")
syn_already_have <- syn_already_have %>%
  add_row(plant_name = "Myoporum tenuifolium", synonym = "Myoporum montanum")

names(syn_already_have)[names(syn_already_have) == 'plant_name'] <- 'species'
names(syn_already_have)[names(syn_already_have) == 'synonym'] <- 'canonicalName'

# add to what I have
gbif_synonyms <- bind_rows(gbif_synonyms, syn_already_have)
gbif_synonyms <- distinct(gbif_synonyms, species, canonicalName)

# filter and fix
gbif_synonyms$species <- as.character(gbif_synonyms$species)
gbif_synonyms$canonicalName <- as.character(gbif_synonyms$canonicalName)

gbif_synonyms <- filter(gbif_synonyms, species != canonicalName)

# make sure the synonyms are not actually species

plant_name_database <- all_entities_short %>%
  filter(category == "SP") %>%
  distinct(plant_name)

synonym_names <- select(gbif_synonyms, canonicalName)
synonym_names <- distinct(synonym_names) # two different species can have the same synonym, why are they not syns of each other?

same <- inner_join(plant_name_database, synonym_names, by = c("plant_name" = "canonicalName"))  
# 16 differ

# filter these out
names(same)[names(same) == 'plant_name'] <- 'canonicalName'

# remove
gbif_synonyms <- anti_join(gbif_synonyms, same)

# check
synonym_names <- select(gbif_synonyms, canonicalName)
synonym_names <- distinct(synonym_names) # two different species can have the same synonym, why are they not syns of each other?

same <- inner_join(plant_name_database, synonym_names, by = c("plant_name" = "canonicalName"))
# all removed

# two species with the same synonym
gbif_synonyms_check <- gbif_synonyms %>%
  group_by(canonicalName) %>%
  summarise(frequency = n())
# I would remove these, there are 39

gbif_synonyms_remove <- gbif_synonyms_check %>%
  filter(frequency > 1) %>%
  select(canonicalName)

# remove
gbif_synonyms <- anti_join(gbif_synonyms, gbif_synonyms_remove)

# there are weird canonicalNames with 'publ' and 'oppr' at the end, filter those out
# https://stackoverflow.com/questions/22850026/filter-rows-which-contain-a-certain-string

gbif_synonyms <- filter(gbif_synonyms, !grepl("publ", canonicalName))
gbif_synonyms <- filter(gbif_synonyms, !grepl("oppr", canonicalName))


# fix some spelling mistakes
gbif_synonyms[] <- lapply(gbif_synonyms, gsub, pattern = "Mentha xpiperita", replacement = "Mentha x piperita")
gbif_synonyms[] <- lapply(gbif_synonyms, gsub, pattern = "Mentha xrotundifolia", replacement = "Mentha x rotundifolia")

# remove the synonyms for malus that are confusing, and wrong synonyms according to Gwilym
syn_remove <- gbif_synonyms %>%
  filter(species == "Malus pumila" & canonicalName == "Malus domestica" | 
         species == "Malus sieboldii" & canonicalName == "Malus floribunda" | 
         species == "Acacia harpophylla" & canonicalName == "Acacia harpopylla" |
         species == "Syzygium tierneyanum" & canonicalName == "Waterhousea floribunda" |
         species == "Acer pseudoplatanus" & canonicalName == "Acer atropurpureum" |
         species == "Acer pseudoplatanus" & canonicalName == "Acer latifolium" | 
         species == "Acer rubrum" & canonicalName == "Acer stenocarpum" | 
         species == "Acer saccharinum" & canonicalName == "Acer album" | 
         species == "Acer saccharinum" & canonicalName == "Acer pallidum" | 
         species == "Acronychia oblongifolia" & canonicalName == "Eriostemon oblongifolium" | 
         species == "Acronychia oblongifolia" & canonicalName == "Eriostemon oblongifolius" | 
         species == "Adiantum hispidulum" | 
         species == "Aechmea caudata" & canonicalName == "Aechmea floribunda" | 
         species == "Aechmea nudicaulis" & canonicalName == "Tillandsia serrata" | 
         species == "Aechmea pineliana" & canonicalName == "Echinostachys rosea" | 
         species == "Aesculus californica" & canonicalName == "Pavia californica" | 
         species == "Aesculus glabra" & canonicalName == "Aesculus carnea" |
         species == "Aesculus glabra" & canonicalName == "Pavia carnea" | 
         species == "Agave geminiflora" & canonicalName == "Tillandsia juncea" | 
         species == "Agave geminiflora" & canonicalName == "Dracaena filamentosa" | 
         species == "Ajuga reptans" & canonicalName == "Ajuga alpina" |
         species == "Albizia julibrissin" & canonicalName == "Albizzia julibrissin" |
         species == "Alnus glutinosa" & canonicalName == "Alnus imperialis" | 
         species == "Alnus glutinosa" & canonicalName == "Alnus aurea" | 
         species == "Aloe succotrina" & canonicalName == "Aloe soccotorina" | 
         species == "Aloe succotrina" & canonicalName == "Aloe soccotrina" | 
         species == "Aloe vera" & canonicalName == "Aloe humilis" |
         species == "Alonsoa meridionalis" | 
         species == "Aloysia citrodora" & canonicalName == "Aloysia citridora" | 
         species == "Aloysia citrodora" & canonicalName == "Verbena fragrans" | 
         species == "Alpinia zerumbet" & canonicalName == "Amomum nutans" | 
         species == "Alpinia zerumbet" & canonicalName == "Languas schumanniana" | 
         species == "Alpinia zerumbet" & canonicalName == "Languas speciosa" | 
         species == "Alpinia zerumbet" & canonicalName == "Renealmia nutans" | 
         species == "Alpinia zerumbet" & canonicalName == "Renealmia spectabilis" | 
         species == "Alternanthera ficoidea" & canonicalName == "Achyranthes ficoides" | 
         species == "Alternanthera ficoidea" & canonicalName == "Alternanthera polygonoides" |
         species == "Alternanthera ficoidea" & canonicalName == "Alternanthera tenella" | 
         species == "Alternanthera ficoidea" & canonicalName == "Bucholzia brachiata" |
         species == "Alternanthera ficoidea" & canonicalName == "Illecebrum tenellum" |  
         species == "Alternanthera ficoidea" & canonicalName == "Paronychia tenella" | 
         species == "Amaranthus tricolor" & canonicalName == "Amaranthus lancifolius" | 
         species == "Amaranthus tricolor" & canonicalName == "Amaranthus lancefolius" |
         species == "Amaranthus tricolor" & canonicalName == "Amaranthus polygamus" | 
         species == "Amaranthus tricolor" & canonicalName == "Amaranthus roxburgianus" | 
         species == "Amelanchier canadensis" | 
         species == "Amsonia tabernaemontana" & canonicalName == "Amsonia amsonia" | 
         species == "Andromeda polifolia" & canonicalName == "Andomeda canescens" | 
         species == "Angophora costata" & canonicalName == "Metrosideros apocynifolia" | 
         species == "Angophora hispida" & canonicalName == "Metrosideros anomala" | 
         species == "Angophora hispida" & canonicalName == "Metrosideros cordifolia" | 
         species == "Angophora hispida" & canonicalName == "Metrosideros hirsuta" | 
         species == "Angophora subvelutina" |
         species == "Antennaria dioica" & canonicalName == "Antennaria dioeca" | 
         species == "Arbutus unedo" & canonicalName == "Arbutus idaea")

gbif_synonyms <- anti_join(gbif_synonyms, syn_remove)

# Add synonyms that Gwilym says

gbif_synonyms_add <- data.frame("Acacia buxifolia", "Racosperma buxifolium")
names(gbif_synonyms_add) <- c("species", "canonicalName")

gbif_synonyms_add <- gbif_synonyms_add %>%
  add_row(species = "Abelia grandiflora", canonicalName = "Linnaea × grandiflora") %>%
  add_row(species = "Abelia grandiflora", canonicalName = "Abelia × grandiflora") %>%
  add_row(species = "Acacia dangarensis", canonicalName = "Racosperma dangarense") %>%
  add_row(species = "Acacia excelsa", canonicalName = "Racosperma excelsum") %>%
  add_row(species = "Acacia gunnii", canonicalName = "Racosperma gunnii") %>%
  add_row(species = "Acacia hakeoides", canonicalName = "Racosperma hakeoides") %>%
  add_row(species = "Syzygium floribundum", canonicalName = "Waterhousea floribunda") %>%
  add_row(species = "Actinostrobus pyramidalis", canonicalName = "Callitris pyramidalis") %>%
  add_row(species = "Allium siculum", canonicalName = "Nectaroscordum siculum") %>%
  add_row(species = "Allocasuarina luehmannii", canonicalName = "Casuarina luehmannii") %>%
  add_row(species = "Alonsoa meridionalis", canonicalName = "Alonsoa warscewiczii") %>%
  add_row(species = "Amelanchier canadensis", canonicalName = "Amelanchier austromontana")
  
gbif_synonyms <- rbind(gbif_synonyms, gbif_synonyms_add)

# IT IS ALL CLEANED, NOW TO JOIN TOGETHER
# https://stackoverflow.com/questions/38514988/concatenate-strings-by-group-with-dplyr

gbif_synonyms_join <- gbif_synonyms %>%
  group_by(species) %>%
  mutate(synonyms = paste0(canonicalName, collapse = ", ")) %>%
  distinct(species, synonyms)
# we have synonyms for 1294/1914 species!!!!

# join to master database
names(gbif_synonyms_join)[names(gbif_synonyms_join) == 'species'] <- 'plant_name'
names(gbif_synonyms_join)[names(gbif_synonyms_join) == 'synonyms'] <- 'synonym'

all_entities_short <- select(all_entities_short, -synonym)

all_entities_short <- left_join(all_entities_short, gbif_synonyms_join, by = "plant_name")

all_entities_short$synonym[is.na(all_entities_short$synonym)] <- "NA"

# rearrange columns
all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, climber, cycad, fern, grass, herbaceous, palm, shrub, succulent, tree, origin, trait_name, value,
         bird, insect, mammal_lizard, animal_pollinated, habitat, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index, drought_strategy, heat_tolerance)

# internal beta testing feedback
# Michelle: change 'grass' to 'grass-like'
names(all_entities_short)[names(all_entities_short) == 'grass'] <- 'grass-like'

# for gh species, remove the horticultural drought tolerance trait

gh_drought <- all_entities_short %>%
  filter(exp_tested == "Y") %>%
  filter(trait_name == "drought tolerance")

# remove
all_entities_short <- anti_join(all_entities_short, gh_drought)

# remove 'model_type' column
all_entities_short <- select(all_entities_short, -model_type)

# remove 'habitat' as a 'use
all_entities_short <- all_entities_short %>%
  filter(value != "habitat")

# Gwilym changes
# fix family names
all_entities_short[] <- lapply(all_entities_short, gsub, pattern = "Leguminosae", replacement = "Fabaceae")
all_entities_short[] <- lapply(all_entities_short, gsub, pattern = "Compositae", replacement = "Asteraceae")

# remove 'canopy shape' trait from everything that isn't a tree or shrub
canopy_shape_remove <- all_entities_short %>%
  filter(tree == "0" & shrub == "0") %>%
  filter(trait_name == "canopy shape")

all_entities_short <- anti_join(all_entities_short, canopy_shape_remove)

write.csv(all_entities_short,"Master_database_output/FINAL/trait_database_ST_FINAL_18.6.2021_vers1.7.csv",row.names=FALSE)



















# check AI with hort classifications of drought (according to Ale, ai is actually PET (potential evapotranspiration))

drought_class <- all_entities_short %>%
  filter(exp_tested == "N") %>%
  filter(trait_name == "drought tolerance") %>%
  select(plant_name, value)

names(drought_class)[names(drought_class) == 'value'] <- 'drought_tolerance'

# load Ale data

Ale <- read.csv("Master_database_input/Ale/niche_summaries_5414_species.csv")

# extract PET values
Ale_ai <- Ale %>%
  filter(var == "ai") %>%
  select(speciesName, p5, mean, p95)

Ale_ai <- Ale_ai %>%
  mutate(p5_adj = p5*365,
         mean_adj = mean*365,
         p95_adj = p95*365) %>%
  select(speciesName, p5_adj, mean_adj, p95_adj)

names(Ale_ai)[names(Ale_ai) == 'speciesName'] <- 'plant_name'
names(Ale_ai)[names(Ale_ai) == 'p5_adj'] <- 'PET_min'
names(Ale_ai)[names(Ale_ai) == 'mean_adj'] <- 'PET_mean'
names(Ale_ai)[names(Ale_ai) == 'p95_adj'] <- 'PET_max'

# extract precip values
Ale_precip <- Ale %>%
  filter(var == "Annual_precip") %>%
  select(speciesName, p5, mean, p95)

names(Ale_precip)[names(Ale_precip) == 'speciesName'] <- 'plant_name'
names(Ale_precip)[names(Ale_precip) == 'p5'] <- 'precip_min'
names(Ale_precip)[names(Ale_precip) == 'mean'] <- 'precip_mean'
names(Ale_precip)[names(Ale_precip) == 'p95'] <- 'precip_max'

# join together
Ale_ai_precip <- left_join(Ale_ai, Ale_precip, by = "plant_name")

# calculate precip-PET
Ale_ai_precip <- Ale_ai_precip %>%
  mutate(diff_min = precip_min - PET_min,
         diff_mean = precip_mean - PET_mean,
         diff_max = precip_max - PET_max) %>%
  select(plant_name, diff_min, diff_mean, diff_max)

# make into long format
# http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
Ale_ai_precip_long <- gather(Ale_ai_precip, diff_type, diff, diff_min:diff_max)

# join together

ai_drought_class <- inner_join(drought_class, Ale_ai_precip_long, by = "plant_name")

# graph

library(ggplot2)

plot <- ggplot(ai_drought_class, aes(x = drought_tolerance, y = diff, fill = diff_type)) +
  geom_boxplot()
plot

# calculate AI

# load Ale data

Ale <- read.csv("Master_database_input/Ale/niche_summaries_5414_species.csv")

# extract PET values
Ale_PET <- Ale %>%
  filter(var == "ai") %>%
  select(speciesName, p5, mean, p95)

Ale_PET <- Ale_PET %>%
  mutate(p5_adj = p5*365,
         mean_adj = mean*365,
         p95_adj = p95*365) %>%
  select(speciesName, p5_adj, mean_adj, p95_adj)

names(Ale_PET)[names(Ale_PET) == 'speciesName'] <- 'plant_name'
names(Ale_PET)[names(Ale_PET) == 'p5_adj'] <- 'PET_min'
names(Ale_PET)[names(Ale_PET) == 'mean_adj'] <- 'PET_mean'
names(Ale_PET)[names(Ale_PET) == 'p95_adj'] <- 'PET_max'

# extract precip values
Ale_precip <- Ale %>%
  filter(var == "Annual_precip") %>%
  select(speciesName, p5, mean, p95)

names(Ale_precip)[names(Ale_precip) == 'speciesName'] <- 'plant_name'
names(Ale_precip)[names(Ale_precip) == 'p5'] <- 'precip_min'
names(Ale_precip)[names(Ale_precip) == 'mean'] <- 'precip_mean'
names(Ale_precip)[names(Ale_precip) == 'p95'] <- 'precip_max'

# join together
Ale_PET_precip <- left_join(Ale_PET, Ale_precip, by = "plant_name")

# calculate AI
Ale_PET_precip <- Ale_PET_precip %>%
  mutate(AI_min = precip_min/PET_min,
         AI_mean = precip_mean/PET_mean,
         AI_max = precip_max/PET_max) %>%
  select(plant_name, AI_min, AI_mean, AI_max)

# make into long format
# http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
Ale_PET_precip_long <- gather(Ale_PET_precip, AI_type, AI, AI_min:AI_max)

# join together

ai_drought_class <- inner_join(drought_class, Ale_PET_precip_long, by = "plant_name")

# graph

library(ggplot2)

plot <- ggplot(ai_drought_class, aes(x = drought_tolerance, y = AI, fill = AI_type)) +
  geom_boxplot()
plot

# anova to see if things are statistically different
# http://www.sthda.com/english/wiki/one-way-anova-test-in-r
# extract just the PET_max data
# PET_max <- ai_drought_class %>%
#   filter(PET_type == "PET_max") %>%
#   select(-PET_type, -plant_name)
# names(PET_max)[names(PET_max) == 'PET'] <- 'PET_max'
# 
# glimpse(PET_max)
# PET_max$drought_tolerance <- as.factor(PET_max$drought_tolerance)
# levels(PET_max$drought_tolerance)
# PET_max$drought_tolerance <- ordered(PET_max$drought_tolerance, levels = c("putatively no", "putatively moderate", "putatively high"))
# 
# res.aov <- aov(PET_max ~ drought_tolerance, data = PET_max)
# summary(res.aov) # there is a significant difference

# TukeyHSD(res.aov) # all are significantly different

# check assumptions
# plot(res.aov, 1)
# library(car)
# leveneTest(PET_max ~ drought_tolerance, data = PET_max) # no evidence that variance across groups is significantly different





#### generate species list for Linda and Farzin

species_list <- all_entities_short %>%
  filter(category == "SP") %>%
  distinct(scientificNameStd)

write.csv(species_list,"Master_database_output/Farzin/species_list_ST_21May2021.csv", row.names=FALSE)

# check for Farzin which specie shave been removed

old_list <- read.csv("Master_database_output/Farzin/species_list_ST_5May2021.csv")

old_list <- select(old_list, scientificNameStd)

diff <- setdiff(old_list, species_list)
diff2 <- setdiff(species_list, old_list)

write.csv(diff,"Master_database_output/Farzin/removed_species_21May2021.csv", row.names=FALSE)

#####################
#### add more weeds

weeds_QL <- read.csv("Master_database_input/weeds/weeds_Ql.csv")

# check species in database
species <- all_entities_short %>%
  distinct(plant_name) #2603 plants now

# compare lists
same <- inner_join(species, weeds_QL, by = "plant_name")

# extract the weeds for Michelle

weed_list <- all_entities_short %>%
  filter(trait_name == "weed status in Australia") %>%
  distinct(plant_name, trait_name, value) %>%
  group_by(plant_name, trait_name) %>%
  summarise(frequency = n()) %>%
  select(-trait_name) %>%
  arrange(desc(frequency))
  
# add the list of states
states <- all_entities_short %>%
  filter(trait_name == "weed status in Australia") %>%
  select(plant_name, value) %>%
  group_by(plant_name) %>%
  mutate(states = paste0(value, collapse = ", ")) %>%
  distinct(plant_name, states)
  
# join together
weed_list_states <- left_join(weed_list, states, by = "plant_name")

write.csv(weed_list_states,"Master_database_output/weeds/weed_list_WPW.csv", row.names=FALSE)

#### nsw weeds

weedwise <- read.csv("Master_database_input/weeds/NSWWeedWise.csv")

weedwise$plant_name <- as.character(weedwise$plant_name)
weedwise <- arrange(weedwise, plant_name)

# extract species list from db
species <- all_entities_short %>%
  distinct(plant_name)

# compare this list with the weedwise list

same <- inner_join(weedwise, species, by = "plant_name") # NOT WORKING!!!!!

#### brisbane weeds

brisbane <- read.csv("Master_database_input/weeds/brisbane_weeds.csv")

# extract the non qld weeds from db
qld <- all_entities_short %>%
  filter(trait_name == "weed status in Australia") %>%
  filter(value != "Queensland") %>%
  distinct(plant_name)

# compare the list

same <- inner_join(qld, brisbane, by = "plant_name") # 32 plants



##############################################################
##### check the species names in the shared spreadsheet match with db

species <- all_entities_short %>%
  distinct(plant_name) # 2603 plants

species_sharedrive <- read.csv("Master_database_input/photos/species_list_photos_20May2021.csv")

species_sharedrive <- species_sharedrive %>%
  distinct(plant_name) # 2603 plants

diff <- setdiff(species, species_sharedrive)
diff2 <- setdiff(species_sharedrive, species)
# no differences

#### check that the file names match with the metadata

disuses_filenames <- read.csv("Master_database_input/photos/disused_file_photo_list_20May2021.csv")

metadata <- read.csv("Master_database_input/photos/photo_check_20May2021.csv")

metadata <- select(metadata, New.file.name)

diff <- setdiff(disuses_filenames, metadata)
diff2 <- setdiff(metadata, disuses_filenames)
# no differences

write.csv(diff,"Master_database_output/photos/extensions_to_change.csv", row.names=FALSE)

###### check that file names for metadata match for beta version images

beta_version <- read.csv("Master_database_input/photos/beta_version_photo_list_17Jun2021.csv")

metadata <- read.csv("Master_database_input/photos/photo_check_17Jun2021.csv")

metadata <- select(metadata, New.file.name)

names(metadata)[names(metadata) == 'New.file.name'] <- 'Photo_names'

diff <- setdiff(beta_version, metadata)
diff2 <- setdiff(metadata, beta_version)

write.csv(diff,"Master_database_output/photos/beta_version_diff_17Jun2021.csv", row.names=FALSE)
write.csv(diff2,"Master_database_output/photos/metadata_diff_17Jun2021.csv", row.names=FALSE)

#################################################
##### For Gwilym

# load species list (generated in vers 1.6)

gwilym <- read.csv("Master_database_output/Gwilym/WPW_plant_list.csv")

# plants with photos to check

metadata <- read.csv("Master_database_input/photos/photo_check_20May2021.csv")

metadata <- distinct(metadata, plant_name)
metadata$photos_to_check <- "yes"

# join together
gwilym <- left_join(gwilym, metadata, by = "plant_name")

write.csv(gwilym,"Master_database_output/Gwilym/WPW_plant_list_20May2021.csv", row.names=FALSE)

###################################################################################################
###################################################################################################
###################################################################################################

########## Version 1.8
# Things that have changed:
### Gwilym's corrections
### 'Weeds' are only declared species in each state
### GC, CULVAR, and SSP fixed, spp. removed from GCs
### 'silvery' changed to 'silver'
### decimal numbers for heights and widths > 2 instead of > 1
### changed WSUD to Water Sensitive Urban Design

library(tidyverse)

everything <- read.csv("Master_database_input/EVERYTHING_traits_2Sept2021.csv")

everything_gh <- read.csv("Master_database_input/EVERYTHING_gh_2Sept2021.csv")

all_entities <- bind_rows(everything, everything_gh)

all_entities_short <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value) %>%
  distinct(scientificNameStd, species, category, exp_tested, trait_name, value)

check <- distinct(all_entities_short, species) # 2605 entities

# remove the old height and width dimensions

all_entities_short <- all_entities_short %>%
  filter(trait_name != "max_height", trait_name != "height", trait_name != "min_height", 
         trait_name != "max_width", trait_name != "width", trait_name != "min_width")

check <- distinct(all_entities_short, species) # 2605 entities, haven't lost anything

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

# change min, max and average values into integars
height_width <- height_width %>%
  mutate_if(is.factor, as.character) %>%
  mutate(max_new = if_else(max > 2, round(max), max),
         min_new = if_else(min > 2, round(min), min),
         average_new = if_else(average > 2, round(average), average))

height_width <- select(height_width, -max, -min, -average)

names(height_width)[names(height_width) == 'max_new'] <- 'max'
names(height_width)[names(height_width) == 'min_new'] <- 'min'
names(height_width)[names(height_width) == 'average_new'] <- 'average'

glimpse(height_width)
height_width$average <- as.numeric(as.character(height_width$average))

filter(height_width, range < 0) # no mistakes

names <- distinct(height_width, species) # 2605, am not missing anything!

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

check <- distinct(all_entities_short, species) # 2605 entities, haven't lost anything

### do drought classifications

drought <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  filter(trait_name == "drought_tolerance") %>%
  select(scientificNameStd, species, category, exp_tested, trait_name, value)

# make all the values upper case
drought$value <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", drought$value, perl=TRUE)

drought_summary <- drought %>%
  group_by(scientificNameStd, species) %>%
  summarise(number_records = n())
# 1799 records

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
# 1875 records

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
# 945 records

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
  distinct(species, plantType, origin) # 2605 entities

# join to main dataset
all_entities_short <- left_join(all_entities_short, plant_type_origin, by = "species")

all_entities_short <- all_entities_short %>%
  select(scientificNameStd, species, plantType, origin, category, exp_tested, trait_name, value) %>%
  filter(trait_name != "native_exotic")

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

# rearrange all the columns
all_entities_short <- select(all_entities_short, scientificNameStd, family, genus, species, entity, synonym, model_type, plantType, climber, 
                             cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, category, exp_tested, trait_name, value,
                             canopy_cover, shade_value, shade_index, carbon_value, carbon_index)

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
                             height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)

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

## add parents of GC, H, HC

parents <- read.csv("Master_database_input/hybrids_genus_cultivars_parents_new.csv")

# change some names
parents[] <- lapply(parents, gsub, pattern = "Anigozanthos Red Cross", replacement = "Anigozanthos spp. Red Cross")
parents[] <- lapply(parents, gsub, pattern = "Ceanothus Blue Pacific", replacement = "Ceanothus spp. Blue Pacific")
parents[] <- lapply(parents, gsub, pattern = "Citrus Sunrise Lime", replacement = "Citrus spp. Sunrise Lime")
parents[] <- lapply(parents, gsub, pattern = "Grevillea Canterbury Gold", replacement = "Grevillea spp. Canterbury Gold")
parents[] <- lapply(parents, gsub, pattern = "Grevillea Coastal Sunset", replacement = "Grevillea spp. Coastal Sunset")
parents[] <- lapply(parents, gsub, pattern = "Grevillea Crimson YulLo", replacement = "Grevillea spp. Crimson Yul-Lo")
parents[] <- lapply(parents, gsub, pattern = "Grevillea Ivanhoe", replacement = "Grevillea spp. Ivanhoe")
parents[] <- lapply(parents, gsub, pattern = "Grevillea Long John", replacement = "Grevillea spp. Long John")
parents[] <- lapply(parents, gsub, pattern = "Grevillea Parakeet Pink", replacement = "Grevillea spp. Parakeet Pink")
parents[] <- lapply(parents, gsub, pattern = "Grevillea Silvereye Cream", replacement = "Grevillea spp. Silvereye Cream")
parents[] <- lapply(parents, gsub, pattern = "Grevillea Wattlebird Yellow", replacement = "Grevillea spp. Wattlebird Yellow")
parents[] <- lapply(parents, gsub, pattern = "Myoporum Monaro Marvel", replacement = "Myoporum spp. Monaro Marvel")
parents[] <- lapply(parents, gsub, pattern = "Philotheca Flower Girl", replacement = "Philotheca spp. Flower Girl")
parents[] <- lapply(parents, gsub, pattern = "Prostanthera Poorinda Ballerina", replacement = "Prostanthera spp. Poorinda Ballerina")
parents[] <- lapply(parents, gsub, pattern = "Prunus Elvins", replacement = "Prunus spp. Elvins")
parents[] <- lapply(parents, gsub, pattern = "Telopea Braidwood Brilliant", replacement = "Telopea spp. Braidwood Brilliant")
parents[] <- lapply(parents, gsub, pattern = "Telopea Shady Lady Red", replacement = "Telopea spp. Shady Lady Red")
parents[] <- lapply(parents, gsub, pattern = "Ulmus Sapporo Autumn Gold", replacement = "Ulmus spp. Sapporo Autumn Gold")
parents[] <- lapply(parents, gsub, pattern = "Xerochrysum Dargan Hill Monarch", replacement = "Xerochrysum spp. Dargan Hill Monarch")
parents[] <- lapply(parents, gsub, pattern = "Myoporum tenuifolium", replacement = "Myoporum montanum")
parents[] <- lapply(parents, gsub, pattern = "Hebe x franciscana Variegated", replacement = "Hebe x franciscana Variegata")
parents[] <- lapply(parents, gsub, pattern = "Tilia x europea", replacement = "Tilia x europaea")

parents <- select(parents, -plantType)

names(parents)[names(parents) == 'Species'] <- 'entity'

# join
found <- left_join(found, parents, by = "entity")

# rearrange the columns

found <- found %>%
  select(-frequency) %>%
  select(scientificNameStd, family, genus, species, entity, synonym, Parent_1, Parent_2,Parent_3, Parent_4, model_type, plantType, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, category, exp_tested, trait_name, value,
         height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)

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

# remove H, HC, GC from database

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
         height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)

# join back the H, HC, GCs

all_entities_short <- bind_rows(all_entities_short, found)

all_entities_short <- arrange(all_entities_short, entity, trait_name, value)

# some synonyms with missing family
syn <- all_entities_short %>%
  filter(family == "") %>%
  select(entity) %>%
  distinct(entity) # 17

# find and fix (also includes Gwilym corrections)

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
         family = if_else(entity == "Regelia velutina", "Myrtaceae", family),
         family = if_else(entity == "Citrus maxima", "Rutaceae", family),
         family = if_else(genus == "Syzygium", "Myrtaceae", family),
         family = if_else(genus == "Aloe", "Asphodelaceae", family),
         family = if_else(genus == "Atriplex", "Chenopodiaceae", family),
         family = if_else(genus == "Bulbine", "Asphodelaceae", family),
         family = if_else(genus == "Johnsonia", "Asphodelaceae", family),
         family = if_else(genus == "Phormium", "Asphodelaceae", family),
         family = if_else(genus == "Stypandra", "Asphodelaceae", family),
         family = if_else(genus == "Thelionema", "Asphodelaceae", family),
         family = if_else(genus == "Dianella", "Asphodelaceae", family),
         family = if_else(genus == "Camellia", "Theaceae", family),
         family = if_else(entity == "Carya illinoinensis", "Juglandaceae", family),
         family = if_else(genus == "Chamelaucium", "Myrtaceae", family),
         family = if_else(genus == "Diplarrena", "Iridaceae", family),
         family = if_else(entity == "Eremophila macdonnellii", "Scrophulariaceae", family),
         family = if_else(genus == "Hibiscus", "Malvaceae", family),
         family = if_else(entity == "Mimulus aurantiacus", "Phrymaceae", family),
         family = if_else(genus == "Myoporum", "Scrophulariaceae", family), 
         family = if_else(genus == "Poa", "Poaceae", family))

# check

syn_check <- filter(all_entities_short, family == "") # all fixed

# filter out cultivars
# check if sciname = entity
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
# Species with multiple synonyms that have 5 min traits: Abelia uniflora

# Species and synonyms that have 5 min traits: Bauhinia variegata, Coronidium scorpioides,
# Cupressus arizonica, Ficinia nodosa, Melaleuca fulgens,
# Syringa vulgaris, Syzygium tierneyanum, Virgilia oroboides, Pittosporum phillyraeoides

# summary of what we have so far

summary_original <- all_entities_short %>%
  select(entity, category) %>%
  distinct(entity, category) %>%
  group_by(category) %>%
  summarise(frequency = n())

# exclude the problematic ones

syn_good <- all_entities_short %>%
  filter(scientificNameStd != "Abelia uniflora" & scientificNameStd != "Pittosporum phillyraeoides" 
         & scientificNameStd != "Bauhinia variegata" & scientificNameStd != "Coronidium scorpioides" & scientificNameStd != "Cupressus arizonica" 
         & scientificNameStd != "Ficinia nodosa" & scientificNameStd != "Melaleuca fulgens" 
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
# Abelia uniflora
multi_syn <- all_entities_short %>%
  filter(scientificNameStd == "Abelia uniflora")

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
         | scientificNameStd == "Ficinia nodosa" | scientificNameStd == "Melaleuca fulgens" 
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
  distinct(entity) # 162 synonyms which is 164 from original minus the two species I combined

# change entity to plant name
names(all_entities_short)[names(all_entities_short) == 'entity'] <- 'plant_name'

##### remove medicinal and apiary from usage

# first check coverage of traits

species <- all_entities_short %>%
  distinct(plant_name)
# 2593 entities

traits <- all_entities_short %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2593)*100) # all relevant traits still 100%

# remove medicinal from usage

all_entities_short <- all_entities_short %>%
  filter(value != "medicinal")

# check coverage again

traits <- all_entities_short %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2593)*100) # usage still 100%

# check apiary

no_apiary <- all_entities_short %>%
  filter(value != "apiary")

# check coverage again

traits <- no_apiary %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percent_completeness = (frequency/2593)*100) # usage still 100%

# remove apiary from dataset as a usage
all_entities_short <- all_entities_short %>%
  filter(value != "apiary")

# rearrange columns to make more sense
all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, plantType, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, value,
         height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)

# need to calculate co-benefits
all_entities_short$width_max <- as.numeric(as.character(all_entities_short$width_max))
all_entities_short$width_min <- as.numeric(as.character(all_entities_short$width_min))
all_entities_short$height_max <- as.numeric(as.character(all_entities_short$height_max))
all_entities_short$height_min <- as.numeric(as.character(all_entities_short$height_min))
all_entities_short$tree <- as.numeric(as.character(all_entities_short$tree))
all_entities_short$shrub <- as.numeric(as.character(all_entities_short$shrub))

# shrubs with min height > 5m should be trees (Michelle decided)
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(tree = if_else(shrub == 1 & height_min >= 5, 1, tree))

# canopy cover
# all_entities_short$canopy_cover <- "NA"

all_entities_short$canopy_cover <- ifelse(all_entities_short$tree == 1, pi*(all_entities_short$width_max/2)^2,
                                          all_entities_short$canopy_cover)

# round to the nearest whole number
glimpse(all_entities_short)
all_entities_short$canopy_cover <- as.numeric(as.character(all_entities_short$canopy_cover))
all_entities_short$canopy_cover <- round(all_entities_short$canopy_cover)

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

all_entities_short$canopy_cover[is.na(all_entities_short$canopy_cover)] <- "NA"
all_entities_short$shade_value[is.na(all_entities_short$shade_value)] <- "NA"
all_entities_short$carbon_value[is.na(all_entities_short$carbon_value)] <- "NA"

# add the data that Paul and Sally sent

biodiversity <- read.csv("Master_database_input/biodiversity/Biodiversity_values_PR_Sp_17Jun2017.csv")

# fix up some name changes
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Acacia longifolia subsp Longifolia", replacement = "Acacia longifolia subsp longifolia")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Alyogne huegelii Delightfully Double", replacement = "Alyogyne huegelii Delightfully Double")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Lagerstroemia indica Commanchee", replacement = "Lagerstroemia indica Commanche")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Acacia alata biglandulosa", replacement = "Acacia alata subsp biglandulosa")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Andromeda polifolia compacta", replacement = "Andromeda polifolia Compacta")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Anemone hupehensis japonica", replacement = "Anemone hupehensis Japonica")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Dianella caerulea ssp assera Curly Tops", replacement = "Dianella caerulea subsp assera Curly Tops")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Eremophila bowmanii ssp latifolia", replacement = "Eremophila bowmanii subsp latifolia")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Escallonia rubra macrantha", replacement = "Escallonia rubra Macrantha")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Fagus sylvatica orientalis", replacement = "Fagus sylvatica Orientalis")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Grevillea preissii ssp preissii", replacement = "Grevillea preissii subsp preissii")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Lomandra confertifolia ssp pallida Golden Spray", replacement = "Lomandra confertifolia subsp pallida Golden Spray")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Anigozanthos Red Cross", replacement = "Anigozanthos spp. Red Cross")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Ceanothus Blue Pacific", replacement = "Ceanothus spp. Blue Pacific")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Citrus Sunrise Lime", replacement = "Citrus spp. Sunrise Lime")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Grevillea Canterbury Gold", replacement = "Grevillea spp. Canterbury Gold")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Grevillea Coastal Sunset", replacement = "Grevillea spp. Coastal Sunset")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Grevillea Crimson YulLo", replacement = "Grevillea spp. Crimson Yul-Lo")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Grevillea Ivanhoe", replacement = "Grevillea spp. Ivanhoe")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Grevillea Long John", replacement = "Grevillea spp. Long John")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Grevillea Parakeet Pink", replacement = "Grevillea spp. Parakeet Pink")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Grevillea Silvereye Cream", replacement = "Grevillea spp. Silvereye Cream")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Grevillea Wattlebird Yellow", replacement = "Grevillea spp. Wattlebird Yellow")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Myoporum Monaro Marvel", replacement = "Myoporum spp. Monaro Marvel")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Philotheca Flower Girl", replacement = "Philotheca spp. Flower Girl")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Prostanthera Poorinda Ballerina", replacement = "Prostanthera spp. Poorinda Ballerina")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Prunus Elvins", replacement = "Prunus spp. Elvins")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Telopea Braidwood Brilliant", replacement = "Telopea spp. Braidwood Brilliant")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Telopea Shady Lady Red", replacement = "Telopea spp. Shady Lady Red")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Ulmus Sapporo Autumn Gold", replacement = "Ulmus spp. Sapporo Autumn Gold")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Xerochrysum Dargan Hill Monarch", replacement = "Xerochrysum spp. Dargan Hill Monarch")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Camellia sasangua", replacement = "Camellia sasanqua")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Capparis spinosa var nummularia", replacement = "Capparis spinosa var. nummularia")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Acacia longifolia var sophorae", replacement = "Acacia longifolia subsp sophorae")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Amphipogon caricinus var caricinus", replacement = "Amphipogon caricinus var. caricinus")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Ceanothus papillosus var roweanus", replacement = "Ceanothus papillosus var. roweanus")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Correa alba var alba Starlight", replacement = "Correa alba Starlight")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Microlaena stipoides var stipoides", replacement = "Microlaena stipoides var. stipoides")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Eriogonum fasciculatum var foliolosum", replacement = "Eriogonum fasciculatum var. foliolosum")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Eucalyptus erythronema var marginata", replacement = "Eucalyptus erythronema var. marginata")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Gossypium sturtianum var sturtianum", replacement = "Gossypium sturtianum var. sturtianum")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Melia azedarach var australasica", replacement = "Melia azedarach var. australasica")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Osmanthus fragrans var aurantiacus", replacement = "Osmanthus fragrans var. aurantiacus")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Pittosporum phillyraeoides var microcarpa", replacement = "Pittosporum phillyraeoides var. microcarpa")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Platanus orientalis var insularis", replacement = "Platanus orientalis Insularis")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Platanus orientalis var insularis Autumn Glory", replacement = "Platanus orientalis Insularis Autumn Glory")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Carya illinoiensis", replacement = "Carya illinoinensis")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Chamaelaucium uncinatum", replacement = "Chamelaucium uncinatum")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Chamaelaucium uncinatum Burgundy Blush", replacement = "Chamelaucium uncinatum Burgundy Blush")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Chamaelaucium uncinatum Mullering Brook", replacement = "Chamelaucium uncinatum Mullering Brook")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Chamaelaucium uncinatum Murfit Rose", replacement = "Chamelaucium uncinatum Murfit Rose")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Cordyline australis Cabernett", replacement = "Cordyline australis Cabernet")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Diplarrhena moraea", replacement = "Diplarrena moraea")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Dodonaea viscosa Angustissima", replacement = "Dodonaea viscosa subsp angustissima")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Dodonaea viscosa Cuneata", replacement = "Dodonaea viscosa subsp cuneata")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Eremophila macdonellii", replacement = "Eremophila macdonnellii")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Eucalyptus camaldulensis Obtusa", replacement = "Eucalyptus camaldulensis subsp obtusa")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Banksia integrifolia Austraflora Roller Coaster", replacement = "Banksia integrifolia Roller Coaster")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Hibiscus hakeifolius", replacement = "Hibiscus hakeifolia")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Mimulus auriantiacus", replacement = "Mimulus aurantiacus")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Myoporum tenuifolium", replacement = "Myoporum montanum")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Hebe x franciscana Variegated", replacement = "Hebe x franciscana Variegata")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Tilia x europea", replacement = "Tilia x europaea")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Eriostemon myoporoides Profusion", replacement = "Philotheca myoporoides Profusion")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Eriostemon myoporoides Winter Rouge", replacement = "Philotheca myoporoides Winter Rouge")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Poa labillardierei", replacement = "Poa labillardieri")
biodiversity[] <- lapply(biodiversity, gsub, pattern = "Pittosporum phillyraeoides var. microcarpa", replacement = "Pittosporum angustifolium var. microcarpa")

biodiversity <- biodiversity %>%
  add_row(plant_name = "Syzygium australe", insect = "1", bird = "1", mammal_lizard = "0", animal_pollinated = "1",
          habitat = "1", biodiversity_value = "4") %>%
  add_row(plant_name = "Carissa macrocarpa Emerald Star", insect = "1", bird = "1", mammal_lizard = "0", animal_pollinated = "1",
          habitat = "1", biodiversity_value = "4")

# join to main database
all_entities_short <- left_join(all_entities_short, biodiversity, by = "plant_name")

# rearrange columns
all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, plantType, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, value,
         bird, insect, mammal_lizard, animal_pollinated, habitat, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)

############

# FINAL FORMAT OF THE DATABASE

# first check glasshouse species

gh <- all_entities_short %>%
  distinct(plant_name, exp_tested) %>%
  group_by(exp_tested) %>%
  summarise(frequency = n())
# 116 plants from glasshouse

# remove plantType column

all_entities_short <- select(all_entities_short, -plantType)

# select the traits that we want

all_entities_short <- all_entities_short %>%
  filter(trait_name == "common_name" | trait_name == "flower_colour" | trait_name == "flower_period" | trait_name == "leaf_loss" 
         | trait_name == "light_level" | trait_name == "placement" | trait_name == "usage" | trait_name == "height_max" 
         | trait_name == "height_min" | trait_name == "height_average" | trait_name == "width_max" | trait_name == "width_min" | trait_name == "width_average"
         | trait_name == "soil_type" | trait_name == "soil_pH" | trait_name == "ideal_conditions" | trait_name == "frost_tolerance" | trait_name == "drought_tolerance" 
         | trait_name == "coastal_tolerance" | trait_name == "habit_canopy" | trait_name == "growth_rate" | trait_name == "foliage_colour" | trait_name == "risk" 
         | trait_name == "weed_status" | trait_name == "states_found")

# check they are all there
trait_name_check <- all_entities_short %>%
  distinct(plant_name, trait_name) %>%
  group_by(trait_name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency))

# create new columns to change the trait_name and value
all_entities_short$trait_name_new <- all_entities_short$trait_name
all_entities_short$value_new <- all_entities_short$value

# rearrange columns to make more sense
all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, trait_name_new, value,
         value_new, bird, insect, mammal_lizard, animal_pollinated, habitat, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)

# states found
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "states_found", "states found", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "NewSouthWales", "NSW", value_new),
         value_new = if_else(value == "Victoria", "VIC", value_new),
         value_new = if_else(value == "Queensland", "QLD", value_new),
         value_new = if_else(value == "SouthAustralia", "SA", value_new),
         value_new = if_else(value == "WesternAustralia", "WA", value_new),
         value_new = if_else(value == "NorthernTerritory", "NT", value_new),
         value_new = if_else(value == "Tasmania", "TAS", value_new),
         value_new = if_else(value == "NorfolkIsland", "Norfolk Island", value_new))

# filter out the rest
remove_states <- all_entities_short %>%
  filter(trait_name == "states_found" & value == "introduced_NewSouthWales" | trait_name == "states_found" & value == "introduced_NorthernTerritory" | 
           trait_name == "states_found" & value == "introduced_Queensland" | trait_name == "states_found" & value == "introduced_SouthAustralia" | 
           trait_name == "states_found" & value == "introduced_Tasmania" | trait_name == "states_found" & value == "introduced_Tasmanis" | 
           trait_name == "states_found" & value == "introduced_Victoria" | trait_name == "states_found" & value == "introduced_WesternAustralia")

# remove
all_entities_short <- anti_join(all_entities_short, remove_states)

# COMMON NAME
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "common_name", "common name", trait_name_new))

# replace the '-' with a space
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(trait_name == "common_name", gsub("-", " ", all_entities_short$value_new, perl=TRUE),
                             all_entities_short$value_new))

# some other subs
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(trait_name == "common_name", gsub("Sheoak", "She Oak", all_entities_short$value_new, perl=TRUE, ignore.case = TRUE),
                             all_entities_short$value_new))
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(trait_name == "common_name", gsub("Teatree", "Tea Tree", all_entities_short$value_new, perl=TRUE, ignore.case = TRUE),
                             all_entities_short$value_new))
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(trait_name == "common_name", gsub("Lillypilly", "Lilly Pilly", all_entities_short$value_new, perl=TRUE, ignore.case = TRUE),
                             all_entities_short$value_new))

# make common names capitalised
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(trait_name == "common_name", gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", all_entities_short$value_new, perl=TRUE),
                             all_entities_short$value_new))

# FLOWER COLOUR
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "flower_colour", "flower colour", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "gold", "yellow", value_new),
         value_new = if_else(value == "golden", "yellow", value_new),
         value_new = if_else(value == "grey", "black", value_new), 
         value_new = if_else(value == "insignificant", "inconspicuous flowers", value_new),
         value_new = if_else(value == "inconspicuous", "inconspicuous flowers", value_new),
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
         value_new = if_else(value == "powerlines", "under powerlines", value_new),
         value_new = if_else(value == "wet", "Water Sensitive Urban Design", value_new),
         value_new = if_else(value == "container", "garden", value_new),
         value_new = if_else(value == "indoor", "garden", value_new))

# USAGE
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "usage", "uses", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "cutflower", "cut flowers", value_new),
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
  mutate(trait_name_new = if_else(trait_name == "soil_type", "soil texture", trait_name_new),
         trait_name_new = if_else(trait_name == "soil_pH", "soil pH", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "acid", "acidic", value_new))

# PLANTING AND MAINTENANCE
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(trait_name_new = if_else(trait_name == "ideal_conditions", "planting and maintenance", trait_name_new))

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "fertile", "fertile soil", value_new),
         value_new = if_else(value == "fertile", "fertile soil", value_new),
         value_new = if_else(value == "lateral_space", "lateral space", value_new),
         value_new = if_else(value == "protected", "sheltered", value_new),
         value_new = if_else(value == "poorly_drained", "poorly drained soil", value_new),
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
         value_new = if_else(value == "silver", "silver", value_new),
         value_new = if_else(value == "silver_foliage", "silver", value_new),
         value_new = if_else(value == "silvergreen", "silver", value_new),
         value_new = if_else(value == "silvergrey", "silver", value_new),
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
  mutate(value_new = if_else(value == "prostrate", "upright", value_new),
         value_new = if_else(value == "domed", "rounded", value_new),
         value_new = if_else(value == "narrow", "upright", value_new),
         value_new = if_else(value == "oval", "rounded", value_new))

# filter out rest
remove_habit_canopy <- all_entities_short %>%
  filter(trait_name == "habit_canopy" & value == "arborescent" | trait_name == "habit_canopy" & value == "branching" | 
           trait_name == "habit_canopy" & value == "clumping" | trait_name == "habit_canopy" & value == "compact" | 
           trait_name == "habit_canopy" & value == "conical" | trait_name == "habit_canopy" & value == "vase" |
           trait_name == "habit_canopy" & value == "bushy" | trait_name == "habit_canopy" & value == "dense" |
           trait_name == "habit_canopy" & value == "hedging_possible" | trait_name == "habit_canopy" & value == "open" |
           trait_name == "habit_canopy" & value == "variable" | trait_name == "habit_canopy" & value == "weeping") 

# remove
all_entities_short <- anti_join(all_entities_short, remove_habit_canopy)

# RISK
all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_new = if_else(value == "mildallergen", "allergen", value_new),
         value_new = if_else(value == "moderateallergen", "allergen", value_new),
         value_new = if_else(value == "poison", "poisonous or toxic", value_new),
         value_new = if_else(value == "severeallergen", "allergen", value_new),
         value_new = if_else(value == "spikey", "spikey or spiny", value_new))

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
  mutate(trait_name_new = if_else(trait_name == "weed_status", "weed status in Australia", trait_name_new))

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
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name_new, value_new,
         bird, insect, mammal_lizard, animal_pollinated, habitat, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)

# end need to distinct everything
all_entities_short <- all_entities_short %>%
  distinct(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name_new,
           value_new, bird, insect, mammal_lizard, animal_pollinated, habitat, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)

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
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, value,
         bird, insect, mammal_lizard, animal_pollinated, habitat, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)

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

# sdm <- read.csv("Master_database_input/LindaFarzin/diff_ST_march_and_min5traits_good_10_03_2021_Clean.csv", strip.white = TRUE)
# get rid of empty cells with white space
# https://stackoverflow.com/questions/2261079/how-can-i-trim-leading-and-trailing-white-space

# sdm_possible <- sdm %>%
# select(SDM.is.possible) %>%
# filter(SDM.is.possible != "")
# out of 1914 species, 1469 have data to do sdm, which means 445 are missing

# colnames(sdm_possible) <- "speciesName"

# species with missing sdm
# sdm_missing <- sdm %>%
#   filter(SDM.is.possible == "") %>%
#   select(species_list_ST_1Mar2021.csv)
# 
# colnames(sdm_missing) <- "speciesName"

# load ale's niche data

# niche <- read.csv("Master_database_input/Ale/niche_summaries_5414_species.csv")

# niche <- niche %>%
# distinct(speciesName)

# diff between missing sdm and niche list
# diff_missing_sdm_niche <- setdiff(sdm_missing, niche)
# 77 species do not have a sdm or niche

# diff between models that do not have sdm or niche data and models that do not have sdm data
# diff_niches <- setdiff(sdm_missing, diff_missing_sdm_niche)
# 368 species have niche data but not sdm

# populate model type
# diff_niches$model_type <- "niche"
# diff_missing_sdm_niche$model_type <- "NA"
# sdm_possible$model_type <- "sdm"

# join together
# model_type <- bind_rows(diff_niches, diff_missing_sdm_niche, sdm_possible)

# join to database
# all_entities_short <- select(all_entities_short, -model_type)
# names(model_type)[names(model_type) == 'speciesName'] <- 'plant_name'
# 
# all_entities_short <- left_join(all_entities_short, model_type, by = "plant_name")
# all_entities_short$model_type[is.na(all_entities_short$model_type)] <- "NA"
# all_entities_short$scientificNameStd[is.na(all_entities_short$scientificNameStd)] <- "NA"
# all_entities_short$species[is.na(all_entities_short$species)] <- "NA"
# all_entities_short$canopy_cover[is.na(all_entities_short$canopy_cover)] <- "NA"
# all_entities_short$shade_value[is.na(all_entities_short$shade_value)] <- "NA"
# all_entities_short$carbon_value[is.na(all_entities_short$carbon_value)] <- "NA"

# rearrange columns

all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, value,
         bird, insect, mammal_lizard, animal_pollinated, habitat, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index, dehydration_tolerance, heat_tolerance)

# check that cultivar synonyms are all accounted for
syn_check <- all_entities_short %>%
  filter(scientificNameStd != plant_name)

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(synonym = if_else(plant_name == "Magnolia champaca", "Michelia champaca", synonym),
         synonym = if_else(plant_name == "Chenopodium candolleanum", "Rhagodia candolleana", synonym),
         synonym = if_else(plant_name == "Cassia artemisioides", "Senna artemisioides", synonym))

# add Ale's shade and carbon index categories (NEED TO REDO)

# extract info for Ale to recalculate
# co_benefit <- all_entities_short %>%
#   filter(tree == 1) %>%
#   distinct(scientificNameStd, plant_name, .keep_all = TRUE) %>%
#   select(scientificNameStd, plant_name, tree, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index)
# 
# write.csv(co_benefit,"Master_database_output/Ale/co_benefit_analysis_ST_21.6.2021.csv",row.names = FALSE)

categories <- read.csv("Master_database_input/Ale/co_benefit_analysis_ST_21.6.2021_AO.csv")

# change some names, this will become redundant with a newer version of Ale's data
categories[] <- lapply(categories, gsub, pattern = "Fagus sylvatica orientalis", replacement = "Fagus sylvatica Orientalis")
categories[] <- lapply(categories, gsub, pattern = "Ceanothus Blue Pacific", replacement = "Ceanothus spp. Blue Pacific")
categories[] <- lapply(categories, gsub, pattern = "Citrus Sunrise Lime", replacement = "Citrus spp. Sunrise Lime")
categories[] <- lapply(categories, gsub, pattern = "Grevillea Ivanhoe", replacement = "Grevillea spp. Ivanhoe")
categories[] <- lapply(categories, gsub, pattern = "Prunus Elvins", replacement = "Prunus spp. Elvins")
categories[] <- lapply(categories, gsub, pattern = "Ulmus Sapporo Autumn Gold", replacement = "Ulmus spp. Sapporo Autumn Gold")
categories[] <- lapply(categories, gsub, pattern = "Camellia sasangua", replacement = "Camellia sasanqua")
categories[] <- lapply(categories, gsub, pattern = "Eucalyptus erythronema var marginata", replacement = "Eucalyptus erythronema var. marginata")
categories[] <- lapply(categories, gsub, pattern = "Pittosporum phillyraeoides var microcarpa", replacement = "Pittosporum phillyraeoides var. microcarpa")
categories[] <- lapply(categories, gsub, pattern = "Platanus orientalis var insularis", replacement = "Platanus orientalis Insularis")
categories[] <- lapply(categories, gsub, pattern = "Platanus orientalis var insularis Autumn Glory", replacement = "Platanus orientalis Insularis Autumn Glory")
categories[] <- lapply(categories, gsub, pattern = "Melia azedarach var australasica", replacement = "Melia azedarach var. australasica")
categories[] <- lapply(categories, gsub, pattern = "Carya illinoiensis", replacement = "Carya illinoinensis")

# join to main database
all_entities_short <- select(all_entities_short, -shade_index, -carbon_index)

all_entities_short <- left_join(all_entities_short, categories, by = "plant_name")

glimpse(all_entities_short)

all_entities_short$shade_index <- as.character(all_entities_short$shade_index)
all_entities_short$carbon_index <- as.character(all_entities_short$carbon_index)

all_entities_short$shade_index[is.na(all_entities_short$shade_index)] <- "NA"
all_entities_short$carbon_index[is.na(all_entities_short$carbon_index)] <- "NA"

# rearrange columns
all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, climber, cycad, fern, grass, herb, palm, shrub, succulent, tree, origin, trait_name, value,
         bird, insect, mammal_lizard, animal_pollinated, habitat, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index, dehydration_tolerance, heat_tolerance)

# attach the synonyms found through Taxonstand

taxonomy <- read.csv("Master_database_output/taxonomy_checks/taxonstandcheck_ST_1.3.2021.csv")

taxonomy <- taxonomy %>% 
  select(Taxon, Taxonomic.status, New.Genus, New.Species) %>%
  filter(Taxonomic.status == "Synonym") %>%
  mutate(synonym = paste0(New.Genus, " ", New.Species)) %>%
  select(Taxon, synonym)

taxonomy <- taxonomy %>% 
  select(Taxon, Taxonomic.status, New.Genus, New.Species, New.Infraspecific.rank, New.Infraspecific) %>%
  filter(Taxonomic.status == "Synonym") %>%
  mutate(synonym = if_else(New.Infraspecific.rank != "", paste0(New.Genus, " ", New.Species, " ", New.Infraspecific.rank, " ",
                                                                New.Infraspecific), paste0(New.Genus, " ", New.Species))) %>%
  select(Taxon, synonym)

# remove the dots
taxonomy[] <- lapply(taxonomy, gsub, pattern = "var.", replacement = "var")
taxonomy[] <- lapply(taxonomy, gsub, pattern = "subsp.", replacement = "subsp")

# make sure the synonyms are not actually species

plant_name_database <- all_entities_short %>%
  filter(category == "SP") %>%
  distinct(plant_name)

synonym_names <- select(taxonomy, synonym)

same <- inner_join(plant_name_database, synonym_names, by = c("plant_name" = "synonym"))

# add the synonyms (if plants already don't have them)
no_syn <- all_entities_short %>%
  filter(synonym == "NA")

# extract the species from 'taxonomy'
names(taxonomy)[names(taxonomy) == 'Taxon'] <- 'plant_name'

no_syn_taxonomy <- inner_join(taxonomy, no_syn, by = "plant_name")

no_syn_taxonomy$synonym.y <- no_syn_taxonomy$synonym.x

no_syn_taxonomy <- select(no_syn_taxonomy, -synonym.x)
names(no_syn_taxonomy)[names(no_syn_taxonomy) == 'synonym.y'] <- 'synonym'

# remove these species from the database
all_entities_short <- anti_join(all_entities_short, no_syn_taxonomy, by = "plant_name")

# add back together
all_entities_short <- bind_rows(all_entities_short, no_syn_taxonomy)
all_entities_short <- arrange(all_entities_short, scientificNameStd, plant_name, trait_name, value)

#### feedback from the advisory board + remove erroneous columns
## powerlines
# remove 'under powerlines' as a value from the database
all_entities_short <- all_entities_short %>%
  filter(value != "under powerlines")

# change 'herb' into 'herbaceous'
names(all_entities_short)[names(all_entities_short) == 'herb'] <- 'herbaceous'

# remove 'gravel' as a 'soil texture'
all_entities_short <- all_entities_short %>%
  filter(value != "gravel")

# fix the gh species drought tolerance
all_entities_short[] <- lapply(all_entities_short, gsub, pattern = "dehydration tolerator", replacement = "tolerator")
all_entities_short[] <- lapply(all_entities_short, gsub, pattern = "dehydration avoider", replacement = "avoider")

names(all_entities_short)[names(all_entities_short) == 'dehydration_tolerance'] <- 'drought_strategy'

all_entities_short <- all_entities_short %>%
  mutate_if(is.factor, as.character) %>%
  mutate(drought_strategy = if_else(drought_strategy == "intermediate", "tolerator/avoider", drought_strategy))

# try synonyms again
# https://github.com/ropensci/rgbif/issues/289

# library(rgbif)
# 
# plant_names <- all_entities_short %>%
#   filter(category == "SP") %>%
#   distinct(plant_name)
# 
# colnames(plant_names) <- "scientificName"
# 
# write.csv(plant_names,"Master_database_output/plant_names.csv",row.names = FALSE)

# run through this to get key
# https://www.gbif.org/tools/species-lookup
# save output

# load output
# gbif_species <- read.csv("Master_database_input/synonyms/gbif_species.csv")
# 
# syns <- lapply(gbif_species$key[1:1914], name_usage, data = "synonyms")
# 
# x_unlist <- unlist(syns, recursive = FALSE) # https://stackoverflow.com/questions/43591029/convert-nested-list-elements-into-data-frame-and-bind-the-result-into-one-data-f/43592335
# 
# # only want he 'data' elements
# y <- x_unlist[grep("data", names(x_unlist))] # https://stackoverflow.com/questions/39983986/filter-or-subset-list-by-partial-object-name-in-r  
# 
# z <- rbindlist(y, fill = TRUE) # bind the lists and fill in the missing columns
# 
# synonyms <- z %>%
#   select(species, canonicalName, rank, taxonomicStatus) %>%
#   filter(taxonomicStatus == "SYNONYM") %>%
#   filter(rank == "SPECIES") %>%
#   select(species, canonicalName)
# 
# write.csv(synonyms,"Master_database_input/synonyms/gbif_synonyms.csv",row.names = FALSE)

# synonyms

gbif_synonyms <- read.csv("Master_database_input/synonyms/gbif_synonyms.csv")

# add the species names from gbif that are already apparently synonyms
gbif_species <- read.csv("Master_database_input/synonyms/gbif_species.csv")

gbif_species <- gbif_species %>%
  filter(status == "SYNONYM") %>%
  select(verbatimScientificName, species)

names(gbif_species)[names(gbif_species) == 'species'] <- 'canonicalName'
names(gbif_species)[names(gbif_species) == 'verbatimScientificName'] <- 'species'

# join
gbif_synonyms <- bind_rows(gbif_synonyms, gbif_species)

# check the synonyms I already have in the database

syn_already_have <- all_entities_short %>%
  filter(synonym != "NA") %>%
  distinct(plant_name, synonym)

# fix up Abelia uniflora (Abelia engleriana, Abelia schumannii)
syn_already_have <- syn_already_have %>%
  filter(plant_name != "Abelia uniflora")

syn_already_have <- syn_already_have %>%
  add_row(plant_name = "Abelia uniflora", synonym = "Abelia engleriana")
syn_already_have <- syn_already_have %>%
  add_row(plant_name = "Abelia uniflora", synonym = "Abelia schumannii")

names(syn_already_have)[names(syn_already_have) == 'plant_name'] <- 'species'
names(syn_already_have)[names(syn_already_have) == 'synonym'] <- 'canonicalName'

# add to what I have
gbif_synonyms <- bind_rows(gbif_synonyms, syn_already_have)
gbif_synonyms <- distinct(gbif_synonyms, species, canonicalName)

# filter and fix
gbif_synonyms$species <- as.character(gbif_synonyms$species)
gbif_synonyms$canonicalName <- as.character(gbif_synonyms$canonicalName)

gbif_synonyms <- filter(gbif_synonyms, species != canonicalName)

# make sure the synonyms are not actually species

plant_name_database <- all_entities_short %>%
  filter(category == "SP") %>%
  distinct(plant_name)

synonym_names <- select(gbif_synonyms, canonicalName)
synonym_names <- distinct(synonym_names) # two different species can have the same synonym, why are they not syns of each other?

same <- inner_join(plant_name_database, synonym_names, by = c("plant_name" = "canonicalName"))  
# 16 differ

# filter these out
names(same)[names(same) == 'plant_name'] <- 'canonicalName'

# remove
gbif_synonyms <- anti_join(gbif_synonyms, same)

# check
synonym_names <- select(gbif_synonyms, canonicalName)
synonym_names <- distinct(synonym_names) # two different species can have the same synonym, why are they not syns of each other?

same <- inner_join(plant_name_database, synonym_names, by = c("plant_name" = "canonicalName"))
# all removed

# two species with the same synonym
gbif_synonyms_check <- gbif_synonyms %>%
  group_by(canonicalName) %>%
  summarise(frequency = n())
# I would remove these, there are 39

gbif_synonyms_remove <- gbif_synonyms_check %>%
  filter(frequency > 1) %>%
  select(canonicalName)

# remove
gbif_synonyms <- anti_join(gbif_synonyms, gbif_synonyms_remove)

# there are weird canonicalNames with 'publ' and 'oppr' at the end, filter those out
# https://stackoverflow.com/questions/22850026/filter-rows-which-contain-a-certain-string

gbif_synonyms <- filter(gbif_synonyms, !grepl("publ", canonicalName))
gbif_synonyms <- filter(gbif_synonyms, !grepl("oppr", canonicalName))

# fix some spelling mistakes
gbif_synonyms[] <- lapply(gbif_synonyms, gsub, pattern = "Mentha xpiperita", replacement = "Mentha x piperita")
gbif_synonyms[] <- lapply(gbif_synonyms, gsub, pattern = "Mentha xrotundifolia", replacement = "Mentha x rotundifolia")
gbif_synonyms[] <- lapply(gbif_synonyms, gsub, pattern = "Carya illinoiensis", replacement = "Carya illinoinensis")
gbif_synonyms[] <- lapply(gbif_synonyms, gsub, pattern = "Eremophila macdonellii", replacement = "Eremophila macdonnellii")
gbif_synonyms[] <- lapply(gbif_synonyms, gsub, pattern = "Hibiscus hakeifolius", replacement = "Hibiscus hakeifolia")
gbif_synonyms[] <- lapply(gbif_synonyms, gsub, pattern = "Poa labillardierei", replacement = "Poa labillardieri")

# remove the synonyms for malus that are confusing, and also wrong synonyms according to Gwilym
syn_remove <- gbif_synonyms %>%
  filter(species == "Malus pumila" & canonicalName == "Malus domestica" | 
           species == "Malus sieboldii" & canonicalName == "Malus floribunda" | 
           species == "Acacia harpophylla" & canonicalName == "Acacia harpopylla" |
           species == "Syzygium tierneyanum" & canonicalName == "Waterhousea floribunda" |
           species == "Acer pseudoplatanus" & canonicalName == "Acer atropurpureum" |
           species == "Acer pseudoplatanus" & canonicalName == "Acer latifolium" | 
           species == "Acer rubrum" & canonicalName == "Acer stenocarpum" | 
           species == "Acer saccharinum" & canonicalName == "Acer album" | 
           species == "Acer saccharinum" & canonicalName == "Acer pallidum" | 
           species == "Acronychia oblongifolia" & canonicalName == "Eriostemon oblongifolium" | 
           species == "Acronychia oblongifolia" & canonicalName == "Eriostemon oblongifolius" | 
           species == "Adiantum hispidulum" | 
           species == "Aechmea caudata" & canonicalName == "Aechmea floribunda" | 
           species == "Aechmea nudicaulis" & canonicalName == "Tillandsia serrata" | 
           species == "Aechmea pineliana" & canonicalName == "Echinostachys rosea" | 
           species == "Aesculus californica" & canonicalName == "Pavia californica" | 
           species == "Aesculus glabra" & canonicalName == "Aesculus carnea" |
           species == "Aesculus glabra" & canonicalName == "Pavia carnea" | 
           species == "Agave geminiflora" & canonicalName == "Tillandsia juncea" | 
           species == "Agave geminiflora" & canonicalName == "Dracaena filamentosa" | 
           species == "Ajuga reptans" & canonicalName == "Ajuga alpina" |
           species == "Albizia julibrissin" & canonicalName == "Albizzia julibrissin" |
           species == "Alnus glutinosa" & canonicalName == "Alnus imperialis" | 
           species == "Alnus glutinosa" & canonicalName == "Alnus aurea" | 
           species == "Aloe succotrina" & canonicalName == "Aloe soccotorina" | 
           species == "Aloe succotrina" & canonicalName == "Aloe soccotrina" | 
           species == "Aloe vera" & canonicalName == "Aloe humilis" |
           species == "Alonsoa meridionalis" | 
           species == "Aloysia citrodora" & canonicalName == "Aloysia citridora" | 
           species == "Aloysia citrodora" & canonicalName == "Verbena fragrans" | 
           species == "Alpinia zerumbet" & canonicalName == "Amomum nutans" | 
           species == "Alpinia zerumbet" & canonicalName == "Languas schumanniana" | 
           species == "Alpinia zerumbet" & canonicalName == "Languas speciosa" | 
           species == "Alpinia zerumbet" & canonicalName == "Renealmia nutans" | 
           species == "Alpinia zerumbet" & canonicalName == "Renealmia spectabilis" | 
           species == "Alternanthera ficoidea" & canonicalName == "Achyranthes ficoides" | 
           species == "Alternanthera ficoidea" & canonicalName == "Alternanthera polygonoides" |
           species == "Alternanthera ficoidea" & canonicalName == "Alternanthera tenella" | 
           species == "Alternanthera ficoidea" & canonicalName == "Bucholzia brachiata" |
           species == "Alternanthera ficoidea" & canonicalName == "Illecebrum tenellum" |  
           species == "Alternanthera ficoidea" & canonicalName == "Paronychia tenella" | 
           species == "Amaranthus tricolor" & canonicalName == "Amaranthus lancifolius" | 
           species == "Amaranthus tricolor" & canonicalName == "Amaranthus lancefolius" |
           species == "Amaranthus tricolor" & canonicalName == "Amaranthus polygamus" | 
           species == "Amaranthus tricolor" & canonicalName == "Amaranthus roxburgianus" | 
           species == "Amelanchier canadensis" | 
           species == "Amsonia tabernaemontana" & canonicalName == "Amsonia amsonia" | 
           species == "Andromeda polifolia" & canonicalName == "Andomeda canescens" | 
           species == "Angophora costata" & canonicalName == "Metrosideros apocynifolia" | 
           species == "Angophora hispida" & canonicalName == "Metrosideros anomala" | 
           species == "Angophora hispida" & canonicalName == "Metrosideros cordifolia" | 
           species == "Angophora hispida" & canonicalName == "Metrosideros hirsuta" | 
           species == "Angophora subvelutina" |
           species == "Antennaria dioica" & canonicalName == "Antennaria dioeca" | 
           species == "Arbutus unedo" & canonicalName == "Arbutus idaea" | 
           species == "Banksia marginata" & canonicalName == "Banksia australis" | 
           species == "Callitris columellaris" & canonicalName == "Callitris hugelii" | 
           species == "Calytrix tetragona" & canonicalName == "Calycothrix sullivani" | 
           species == "Camellia japonica" & canonicalName == "Kemelia japonica" | 
           species == "Camellia sasangua" | 
           species == "Carex gaudichaudiana" & canonicalName == "Carex vulpi-caudata" | 
           species == "Carya illinoinensis" & canonicalName == "Carya illinoensis" | 
           species == "Crataegus phaenopyrum" & canonicalName == "Cotoneaster cordata" | 
           species == "Cupressus torulosa" & canonicalName == "Juniperus gracilis" | 
           species == "Doryopteris raddiana" & canonicalName == "Adiantum raddianum" |
           species == "Melia azedarach" | 
           species == "Pandanus tectorius" |
           species == "Quercus robur" | 
           species == "Robinia pseudoacacia" | 
           species == "Thuja occidentalis" | 
           species == "Triadica sebifera" & canonicalName == "Croton macrocarpus" | 
           species == "Eucalyptus lesouefii" | 
           species == "Fraxinus pennsylvanica" | 
           species == "Syringa vulgaris" & canonicalName == "Ligustrum vulgare" | 
           species == "Mimulus aurantiacus" | 
           species == "Themeda triandra" | 
           species == "Calamagrostis arundinacea" | 
           species == "Calytrix tetragona" | 
           species == "Canna indica" | 
           species == "Coleus scutellarioides" | 
           species == "Phormium tenax" & canonicalName == "Phormium atropurpureum")

gbif_synonyms <- anti_join(gbif_synonyms, syn_remove)

# Add synonyms that Gwilym says

gbif_synonyms_add <- data.frame("Acacia buxifolia", "Racosperma buxifolium")
names(gbif_synonyms_add) <- c("species", "canonicalName")

gbif_synonyms_add <- gbif_synonyms_add %>%
  add_row(species = "Abelia grandiflora", canonicalName = "Linnaea × grandiflora") %>%
  add_row(species = "Abelia grandiflora", canonicalName = "Abelia × grandiflora") %>%
  add_row(species = "Acacia dangarensis", canonicalName = "Racosperma dangarense") %>%
  add_row(species = "Acacia excelsa", canonicalName = "Racosperma excelsum") %>%
  add_row(species = "Acacia gunnii", canonicalName = "Racosperma gunnii") %>%
  add_row(species = "Acacia hakeoides", canonicalName = "Racosperma hakeoides") %>%
  add_row(species = "Syzygium floribundum", canonicalName = "Waterhousea floribunda") %>%
  add_row(species = "Actinostrobus pyramidalis", canonicalName = "Callitris pyramidalis") %>%
  add_row(species = "Allium siculum", canonicalName = "Nectaroscordum siculum") %>%
  add_row(species = "Allocasuarina luehmannii", canonicalName = "Casuarina luehmannii") %>%
  add_row(species = "Alonsoa meridionalis", canonicalName = "Alonsoa warscewiczii") %>%
  add_row(species = "Amelanchier canadensis", canonicalName = "Amelanchier austromontana") %>%
  add_row(species = "Atractocarpus fitzalanii", canonicalName = "Randia fitzalanii") %>%
  add_row(species = "Atriplex paludosa", canonicalName = "Atriplex paludosa subsp cordata") %>%
  add_row(species = "Auranticarpa rhombifolia", canonicalName = "Pittosporum rhombifolium") %>%
  add_row(species = "Baloskion tetraphyllum", canonicalName = "Restio tetraphyllus") %>%
  add_row(species = "Bauhinia hookeri", canonicalName = "Lysiphyllum hookeri") %>%
  add_row(species = "Beaucarnea recurvata", canonicalName = "Nolina recurvata") %>%
  add_row(species = "Brugmansia sanguinea", canonicalName = "Datura sanguinea") %>%
  add_row(species = "Camellia sasanqua", canonicalName = "Camellia oleifera") %>%
  add_row(species = "Chamaecyparis lawsoniana", canonicalName = "Cupressus lawsoniana") %>%
  add_row(species = "Chrysocephalum apiculatum", canonicalName = "Helichrysum apiculatum") %>%
  add_row(species = "Chrysocephalum semipapposum", canonicalName = "Helichrysum semipapposum") %>%
  add_row(species = "Corymbia aparrerinja", canonicalName = "Eucalyptus papuana var. aparrerinja") %>%
  add_row(species = "Corymbia aparrerinja", canonicalName = "Eucalyptus aparrerinja") %>%
  add_row(species = "Corymbia citriodora", canonicalName = "Eucalyptus citriodora") %>%
  add_row(species = "Corymbia eximia", canonicalName = "Eucalyptus eximia") %>%
  add_row(species = "Corymbia ficifolia", canonicalName = "Eucalyptus ficifolia") %>%
  add_row(species = "Corymbia henryi", canonicalName = "Eucalyptus henryi") %>%
  add_row(species = "Corymbia maculata", canonicalName = "Eucalyptus maculata") %>%
  add_row(species = "Corymbia terminalis", canonicalName = "Eucalyptus terminalis") %>%
  add_row(species = "Corymbia tessellaris", canonicalName = "Eucalyptus tessellaris") %>%
  add_row(species = "Crinodendron hookerianum", canonicalName = "Tricuspidaria hookerianum") %>%
  add_row(species = "Dichopogon strictus", canonicalName = "Arthropodium fimbriatum") %>%
  add_row(species = "Hakea salicifolia", canonicalName = "Hakea saligna") %>%
  add_row(species = "Pandanus tectorius", canonicalName = "Pandanus tectorius var. australianus") %>%
  add_row(species = "Pandanus tectorius", canonicalName = "Pandanus pedunculatus") %>%
  add_row(species = "Pandanus tectorius", canonicalName = "Pandanus veitchii") %>%
  add_row(species = "Pandanus tectorius", canonicalName = "Pandanus baptistii") %>%
  add_row(species = "Pandanus tectorius", canonicalName = "Pandanus sanderi") %>%
  add_row(species = "Pandanus tectorius", canonicalName = "Pandanus stradbrokensis") %>%
  add_row(species = "Eucalyptus albopurpurea", canonicalName = "Eucalyptus lansdowneana subsp. albopurpurea") %>%
  add_row(species = "Euphorbia characias", canonicalName = "Euphorbia wulfenii") %>%
  add_row(species = "Euryomyrtus ramosissima", canonicalName = "Baeckea ramosissima") %>%
  add_row(species = "Hibiscus hakeifolia", canonicalName = "Alyogyne hakeifolia") %>%
  add_row(species = "Lysiphyllum hookeri", canonicalName = "Bauhinia hookeri") %>%
  add_row(species = "Themeda triandra", canonicalName = "Themeda australis") %>%
  add_row(species = "Pinus radiata", canonicalName = "Pinus insignis")
  

gbif_synonyms <- rbind(gbif_synonyms, gbif_synonyms_add)
gbif_synonyms <- arrange(gbif_synonyms, species, canonicalName)

# IT IS ALL CLEANED, NOW TO JOIN TOGETHER
# https://stackoverflow.com/questions/38514988/concatenate-strings-by-group-with-dplyr

gbif_synonyms_join <- gbif_synonyms %>%
  group_by(species) %>%
  mutate(synonyms = paste0(canonicalName, collapse = ", ")) %>%
  distinct(species, synonyms)
# we have synonyms for 1294/1914 species!!!!

# join to master database
names(gbif_synonyms_join)[names(gbif_synonyms_join) == 'species'] <- 'plant_name'
names(gbif_synonyms_join)[names(gbif_synonyms_join) == 'synonyms'] <- 'synonym'

all_entities_short <- select(all_entities_short, -synonym)

all_entities_short <- left_join(all_entities_short, gbif_synonyms_join, by = "plant_name")

all_entities_short$synonym[is.na(all_entities_short$synonym)] <- "NA"

# rearrange columns
all_entities_short <- all_entities_short %>%
  select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, climber, cycad, fern, grass, herbaceous, palm, shrub, succulent, tree, origin, trait_name, value,
         bird, insect, mammal_lizard, animal_pollinated, habitat, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index, drought_strategy, heat_tolerance)

########################## REMOVED BECAUSE TOO MANY ERRORS
# check coverage of the 'states found' trait
# do it again
# native_summary <- all_entities_short %>%
#   filter(origin == "Native") %>%
#   filter(category == "SP") %>%
#   distinct(plant_name) # 1144 species
# 
# states_found_summary <- all_entities_short %>%
#   filter(trait_name == "states found") %>%
#   distinct(plant_name) # 1144 species
# 
# diff <- setdiff(native_summary, states_found_summary) # 0 different
# diff2 <- setdiff(states_found_summary, native_summary) # 0 different

# pull out states found and origin
# states_found <- all_entities_short %>%
#   filter(trait_name == "states found") %>%
#   select(plant_name, origin, trait_name, value) %>%
#   arrange(plant_name, value)

# join the states together
# states_found <- states_found %>%
#   select(-trait_name) %>%
#   group_by(plant_name, origin) %>%
#   mutate(value = paste0(value, collapse = ", ")) %>%
#   distinct(plant_name, origin, value)

# join together
# states_found$value_new <- paste0("(", states_found$value , ")")
# 
# states_found$origin_new <- paste(states_found$origin, (states_found$value_new))
# 
# states_found <- select(states_found, plant_name, origin_new)

# get rid of origin
# states_found <- states_found[,2:3]
# names(states_found)[names(states_found) == 'origin_new'] <- 'origin'

# join to main dataset
# all_entities_short <- all_entities_short %>%
#   mutate_if(is.factor, as.character) %>%
#   mutate()

# extract the other species
# wpw_species <- all_entities_short %>%
#   distinct(plant_name)
# 
# other_species <- anti_join(wpw_species, states_found, by = "plant_name")

# join the origins for these other species
# origins <- all_entities_short %>%
#   distinct(plant_name, origin)
# 
# other_species <- left_join(other_species, origins, by = "plant_name")
# 
# states_found <- rbind(states_found, other_species)
# states_found <- arrange(states_found, plant_name)

# join to main dataset
# all_entities_short <- select(all_entities_short, -origin)
# 
# all_entities_short <- left_join(all_entities_short , states_found, by = "plant_name")

# rearrange columns
# all_entities_short <- all_entities_short %>%
#   select(scientificNameStd, family, genus, species, plant_name, synonym, category, exp_tested, Parent_1, Parent_2,Parent_3, Parent_4, model_type, climber, cycad, fern, grass, herbaceous, palm, shrub, succulent, tree, origin, trait_name, value,
#          bird, insect, mammal_lizard, animal_pollinated, habitat, biodiversity_value, height_min, height_max, width_min, width_max, canopy_cover, shade_value, shade_index, carbon_value, carbon_index, drought_strategy, heat_tolerance)

# remove the trait 'states found'
# all_entities_short <- all_entities_short %>%
#   filter(trait_name != "states found")
######################################################

# internal beta testing feedback
# Michelle: change 'grass' to 'grass-like'
names(all_entities_short)[names(all_entities_short) == 'grass'] <- 'grass-like'

# for gh species, remove the horticultural drought tolerance trait

# gh_drought <- all_entities_short %>%
#   filter(exp_tested == "Y") %>%
#   filter(trait_name == "drought tolerance")

# remove
# all_entities_short <- anti_join(all_entities_short, gh_drought)

# remove 'model_type' column
all_entities_short <- select(all_entities_short, -model_type)

# remove 'habitat' as a 'use'
all_entities_short <- all_entities_short %>%
  filter(value != "habitat")

# Gwilym changes
# fix family names
all_entities_short[] <- lapply(all_entities_short, gsub, pattern = "Leguminosae", replacement = "Fabaceae")
all_entities_short[] <- lapply(all_entities_short, gsub, pattern = "Compositae", replacement = "Asteraceae")

# remove 'canopy shape' trait from everything that isn't a tree
canopy_shape_remove <- all_entities_short %>%
  filter(tree == "0") %>%
  filter(trait_name == "canopy shape")

all_entities_short <- anti_join(all_entities_short, canopy_shape_remove)

# fix the subsp
all_entities_short[] <- lapply(all_entities_short, gsub, pattern = "subsp ", replacement = "subsp. ")

# fix the genus cultivars
all_entities_short[] <- lapply(all_entities_short, gsub, pattern = " spp. ", replacement = " ")

# check numbers
check <- all_entities_short %>%
  distinct(plant_name)

# write.csv(all_entities_short,"Master_database_output/FINAL/trait_database_ST_FINAL_30.8.2021_vers1.8TEST.csv",row.names=FALSE)






#################################################################
## extract data for Gwilym

Gwilym <- all_entities_short %>%
  filter(tree == "1") %>%
  filter(trait_name == "growth rate" | trait_name == "maximum height" | trait_name == "maximum width") %>%
  select(plant_name, trait_name, value) %>%
  group_by(plant_name, trait_name) %>%
  mutate(value = paste0(value, collapse = ", ")) %>%
  distinct(plant_name, trait_name, value)

Gwilym_wide <- Gwilym %>%
  spread(trait_name, value, fill = NA)

# write.csv(Gwilym_wide,"Master_database_output/Gwilym/Gwilym_canopy_projections.csv",row.names=FALSE)

#################################################################
### check coverage of 'states_found' trait

## first, check how many 'native' plants are in the database
native_summary <- all_entities_short %>%
  filter(origin == "Native") %>%
  distinct(plant_name, category) %>%
  group_by(category) %>%
  summarise(frequency = n())
# just species = 1145

## see how many plants with the 'states_found' trait I have
states_found <- all_entities %>%
  filter(Min_5_traits == "TRUE") %>%
  filter(Include_in_tool == "Yes") %>%
  filter(origin == "Native") %>%
  filter(trait_name == "states_found") %>%
  distinct(species, category) %>%
  group_by(category) %>%
  summarise(frequency = n())
# species + syn = 1147

################################################################
##### Andrew Turnbull
## extract species list for him

species_list <- all_entities_short %>%
  distinct(plant_name) %>%
  arrange(plant_name)

write.csv(species_list,"Master_database_output/Andrew_Turnbull/Andrew_species_list.csv",row.names=FALSE)

# missing traits
species_list <- as.vector(unlist(species_list$plant_name)) # change the dataframe into a list of vectors
class(species_list)                                        # https://stackoverflow.com/questions/7070173/convert-data-frame-column-to-a-vector

trait_list <- all_entities_short %>%                       # list of the traits
  distinct(trait_name)

# traits I don't want
trait_discard <- trait_list %>%
  filter(trait_name == "drought tolerance" | trait_name == "frost tolerance" | 
         trait_name == "coastal tolerance" | trait_name == "canopy shape" | 
           trait_name == "risk" | trait_name == "weed status in Australia")

# filter out
trait_list <- anti_join(trait_list, trait_discard)

trait_list <- as.vector(unlist(trait_list$trait_name)) # change the dataframe into a list of vectors
class(trait_list)                                        # https://stackoverflow.com/questions/7070173/convert-data-frame-column-to-a-vector

species_alltraits <- expand.grid(species_list, trait_list) # create a new dataframe of all combinations of two vectors
# https://stackoverflow.com/questions/11388359/unique-combination-of-all-elements-from-two-or-more-vectors

colnames(species_alltraits) <- c("plant_name", "trait_name") # assign column names

species_alltraits <- species_alltraits %>%
  arrange(plant_name) # arrange by species

# list of what we have
what_we_have <- all_entities_short %>%
  distinct(plant_name, trait_name) %>%
  arrange(plant_name)

traits_missing <- setdiff(species_alltraits, what_we_have) # the traits that we are missing

write.csv(traits_missing,"Master_database_output/Andrew_Turnbull/missing_traits.csv",row.names=FALSE)

################################################################
# cross reference the PlantSure list with what WPW was for weeds

plantsure_list <- read.csv("Master_database_input/weeds/plantsure_list.csv")

wpw_weeds <- read.csv("Master_database_input/weeds/weed_list_wpw_long.csv")

# check the syns

wpw_syn_weeds <- left_join(wpw_weeds, gbif_synonyms, by = c("plant_name" = "species"))
wpw_syn_weeds <- select(wpw_syn_weeds, plant_name, canonicalName)

# check against the plantsure list
weeds_same_syn <- semi_join(plantsure_list, wpw_syn_weeds, by = c("plant_name" = "canonicalName"))
# found 2

plantsure_weeds <- semi_join(wpw_weeds, plantsure_list, by = "plant_name")

# add the two species form before
weeds_add <- wpw_weeds %>%
  filter(plant_name == "Lagerstroemia indica" | plant_name == "Cenchrus setaceus")

plantsure_weeds <- rbind(plantsure_weeds, weeds_add)

# add the synonyms from the wpw database
wpw_species_list <- all_entities_short %>%
  distinct(plant_name, synonym)

plantsure_weeds <- left_join(plantsure_weeds, wpw_species_list, by = "plant_name")
plantsure_weeds <- select(plantsure_weeds, plant_name, synonym, value_David, value_Other, Source)

write.csv(plantsure_weeds,"Master_database_output/weeds/plantsure_weeds.csv",row.names=FALSE)

# check how many species in the plantsure list match with the wpw list
wpw_species_list <- all_entities_short %>%
  distinct(plant_name, synonym)

same_species <- semi_join(plantsure_list, wpw_species_list, by = "plant_name")

# check syns, can't do it this way as it need to be in 'long' format
# same_syn <- semi_join(plantsure_list, wpw_species_list, by = c("plant_name" = "synonym"))
# same_syn <- semi_join(wpw_species_list, plantsure_list, by = c("synonym" = "plant_name"))

same_syn <- semi_join(plantsure_list, gbif_synonyms, by = c("plant_name" = "canonicalName"))

# add these to first list
same_species <- rbind(same_species, same_syn)
same_species <- arrange(same_species)

# differences
diff_species <- anti_join(plantsure_list, same_species)

# make this into a dataframe
colnames(same_species) <- "name_match"
colnames(diff_species) <- "no_name_match"

write.csv(same_species,"Master_database_output/weeds/plantsure_WPW_same.csv",row.names=FALSE)
write.csv(diff_species,"Master_database_output/weeds/plantsure_WPW_diff.csv",row.names=FALSE)

############################################################################################
### do a photo check

database <- all_entities_short %>%
  distinct(plant_name) # 2581

photos <- read.csv("Master_database_input/photos/photo_check_13Jul2021.csv")

photos <- photos %>%
  distinct(plant_name) # 2588

diff <- setdiff(photos, database)
diff2 <- setdiff(database, photos)

############################################################################################
# Extract traits for Rony

rony_list <- read.csv("Master_database_input/Rony/suggested_789_spp.csv")

wpw_species <- all_entities_short %>%
  distinct(scientificNameStd)

same <- semi_join(wpw_species, rony_list, by = c("scientificNameStd" = "Rony_species"))
# 370/789 match

same$wpw_list <- same$scientificNameStd
names(same)[names(same) == 'scientificNameStd'] <- 'rony_list'

# now check the names that didn't match and see if they are synonyms

diffs <- anti_join(rony_list, wpw_species, by = c("Rony_species" = "scientificNameStd"))
# 419 species

# check the synonyms
synonyms_rony <- semi_join(diffs, gbif_synonyms, by = c("Rony_species" = "canonicalName"))
# only two species (but one of them is not a synonym)
# add that one to the master list

same <- same %>%
  add_row(rony_list = "Leucopogon parviflorus", wpw_list = "Leptecophylla juniperina")
# so in total 371/789 match

same <- arrange(same, wpw_list)

# attach the traits
wpw_traits <- all_entities_short %>%
  select(plant_name, trait_name, value)

same <- left_join(same, wpw_traits, by = c("wpw_list" = "plant_name"))

check <- same %>%
  distinct(wpw_list) # 371

write.csv(same,"Master_database_output/Rony/rony_789_spp_wpw_traits.csv",row.names=FALSE)

#####################################################################################################
# extract the drought tolerances for expt species for Gwilym

drought_Gwilym <- all_entities_short %>%
  filter(exp_tested == "Y") %>%
  filter(trait_name == "drought tolerance") %>%
  select(plant_name, trait_name, value)

drought_Gwilym[] <- lapply(drought_Gwilym, gsub, pattern = "putatively high", replacement = "high")
drought_Gwilym[] <- lapply(drought_Gwilym, gsub, pattern = "putatively moderate", replacement = "moderate")
drought_Gwilym[] <- lapply(drought_Gwilym, gsub, pattern = "putatively no", replacement = "no")

# add the missing species to the bottom

missing_species <- all_entities_short %>%
  filter(exp_tested == "Y") %>%
  distinct(plant_name)

diff <- anti_join(missing_species, drought_Gwilym, by = "plant_name")
diff$trait_name <- "drought tolerance"
diff$value <- "NA"

drought_Gwilym <- rbind(drought_Gwilym, diff)

drought_Gwilym <- arrange(drought_Gwilym, plant_name)

write.csv(drought_Gwilym,"Master_database_output/Gwilym/experimental_species_drought.csv", row.names=FALSE)

###############################################################################
# test the 'fuzzy matching' technique for Waverley species list
# https://stackoverflow.com/questions/26405895/how-can-i-match-fuzzy-match-strings-from-two-datasets

wpw_species <- all_entities_short %>%
  distinct(plant_name)

waverley <- read.csv("Master_database_input/Waverley/waverley_species_list.csv")

# remove white space
# https://stackoverflow.com/questions/20760547/removing-whitespace-from-a-whole-data-frame-in-r
glimpse(waverley)
# change to character (https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters)
waverley[] <- lapply(waverley, as.character)

waverley <- waverley %>%
  mutate_if(is.character, str_trim)

# install.packages("fuzzyjoin")
library(fuzzyjoin)

waverley_fuzzy <- stringdist_join(waverley, wpw_species, 
                        by = "plant_name",
                        mode = "left",
                        ignore_case = FALSE, 
                        method = "jw", 
                        max_dist = 99, 
                        distance_col = "dist") 
 
# 0.000 is a perfect match
waverley_fuzzy_filtered <- filter(waverley_fuzzy, dist < 0.06) # can adjust based on the amount of similarity you want, I think this number works best for the waverley list
waverley_fuzzy_filtered <- arrange(waverley_fuzzy_filtered, plant_name.x)

# extract the species that did not match and check them against synonyms

matchedspecies <- as.data.frame(waverley_fuzzy_filtered$plant_name.x)
names(matchedspecies)[names(matchedspecies) == 'waverley_fuzzy_filtered$plant_name.x'] <- 'plant_name'
mismatchedspecies <- anti_join(waverley, matchedspecies, by = "plant_name")

# fuzzy match the synonyms
waverley_syn_fuzzy <- stringdist_join(mismatchedspecies, gbif_synonyms, 
                                  by = c("plant_name" = "canonicalName"),
                                  mode = "left",
                                  ignore_case = FALSE, 
                                  method = "jw", 
                                  max_dist = 99, 
                                  distance_col = "dist") 

waverley_syn_fuzzy <- filter(waverley_syn_fuzzy, dist < 0.06)

# join these to the main list
waverley_fuzzy_filtered <- select(waverley_fuzzy_filtered, -dist)
names(waverley_fuzzy_filtered)[names(waverley_fuzzy_filtered) == 'plant_name.x'] <- 'waverley_plant_name'
names(waverley_fuzzy_filtered)[names(waverley_fuzzy_filtered) == 'plant_name.y'] <- 'wpw_plant_name'

waverley_syn_fuzzy <- select(waverley_syn_fuzzy, plant_name, species)
names(waverley_syn_fuzzy)[names(waverley_syn_fuzzy) == 'plant_name'] <- 'waverley_plant_name'
names(waverley_syn_fuzzy)[names(waverley_syn_fuzzy) == 'species'] <- 'wpw_plant_name'

waverley_fuzzy_filtered <- rbind(waverley_fuzzy_filtered, waverley_syn_fuzzy)

waverley_fuzzy_filtered <- arrange(waverley_fuzzy_filtered, waverley_plant_name)

# check there are not any duplicates
check1 <- waverley_fuzzy_filtered %>%
  group_by(waverley_plant_name) %>%
  summarise(frequency = n())

check2 <- waverley_fuzzy_filtered %>%
  group_by(wpw_plant_name) %>%
  summarise(frequency = n())

# remove mistakes
remove_waverley <- waverley_fuzzy_filtered %>%
  filter(waverley_plant_name == "Isolepis nodosa" & wpw_plant_name == "Ficinia nodosa")

waverley_fuzzy_filtered <- anti_join(waverley_fuzzy_filtered, remove_waverley)

# out of the 295 species in the Waverley list, 153 match with wpw

write.csv(waverley_fuzzy_filtered,"Master_database_output/waverley_council/waverley_species_list_match.csv", row.names=FALSE)


# randwick species

wpw_species <- all_entities_short %>%
  distinct(plant_name)

randwick <- read.csv("Master_database_input/Waverley/randwick_species_list.csv")

# remove white space
# https://stackoverflow.com/questions/20760547/removing-whitespace-from-a-whole-data-frame-in-r
glimpse(randwick)
# change to character (https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters)
randwick[] <- lapply(randwick, as.character)

randwick <- randwick %>%
  mutate_if(is.character, str_trim)

library(fuzzyjoin)

randwick_fuzzy <- stringdist_join(randwick, wpw_species, 
                                  by = "plant_name",
                                  mode = "left",
                                  ignore_case = FALSE, 
                                  method = "jw", 
                                  max_dist = 99, 
                                  distance_col = "dist") 

# 0.000 is a perfect match
randwick_fuzzy_filtered <- filter(randwick_fuzzy, dist < 0.06) # can adjust based on the amount of similarity you want, I think this number works best for the waverley list
randwick_fuzzy_filtered <- arrange(randwick_fuzzy_filtered, plant_name.x)

# extract the species that did not match and check them against synonyms

matchedspecies <- as.data.frame(randwick_fuzzy_filtered$plant_name.x)
names(matchedspecies)[names(matchedspecies) == 'randwick_fuzzy_filtered$plant_name.x'] <- 'plant_name'
mismatchedspecies <- anti_join(randwick, matchedspecies, by = "plant_name")

# fuzzy match the synonyms
randwick_syn_fuzzy <- stringdist_join(mismatchedspecies, gbif_synonyms, 
                                      by = c("plant_name" = "canonicalName"),
                                      mode = "left",
                                      ignore_case = FALSE, 
                                      method = "jw", 
                                      max_dist = 99, 
                                      distance_col = "dist") 

randwick_syn_fuzzy <- filter(randwick_syn_fuzzy, dist < 0.06)

# join these to the main list
randwick_fuzzy_filtered <- select(randwick_fuzzy_filtered, -dist)
names(randwick_fuzzy_filtered)[names(randwick_fuzzy_filtered) == 'plant_name.x'] <- 'randwick_plant_name'
names(randwick_fuzzy_filtered)[names(randwick_fuzzy_filtered) == 'plant_name.y'] <- 'wpw_plant_name'

randwick_syn_fuzzy <- select(randwick_syn_fuzzy, plant_name, species)
names(randwick_syn_fuzzy)[names(randwick_syn_fuzzy) == 'plant_name'] <- 'randwick_plant_name'
names(randwick_syn_fuzzy)[names(randwick_syn_fuzzy) == 'species'] <- 'wpw_plant_name'
randwick_syn_fuzzy <- distinct(randwick_syn_fuzzy, randwick_plant_name, wpw_plant_name)

# fix some things that I saw
randwick_fuzzy_filtered <- randwick_fuzzy_filtered %>%
  add_row(randwick_plant_name = "Philotheca 'Winter Rouge'", wpw_plant_name = "Eriostemon myoporoides Winter Rouge") %>%
  add_row(randwick_plant_name = "Acacia longifolia var. sophora", wpw_plant_name = "Acacia longifolia subsp sophorae")

remove <- randwick_fuzzy_filtered %>%
  filter(randwick_plant_name == "Pyrus calleryana" & wpw_plant_name == "Pyrus calleryana D6" | 
           randwick_plant_name == "Melaleuca ericifolia" & wpw_plant_name == "Melaleuca hypericifolia" | 
           randwick_plant_name == "Melaleuca hypericifolia" & wpw_plant_name == "Melaleuca ericifolia")

randwick_fuzzy_filtered <- anti_join(randwick_fuzzy_filtered, remove)

randwick_fuzzy_filtered <- rbind(randwick_fuzzy_filtered, randwick_syn_fuzzy)

randwick_fuzzy_filtered <- arrange(randwick_fuzzy_filtered, randwick_plant_name)

# check there are not any duplicates
check1 <- randwick_fuzzy_filtered %>%
  group_by(randwick_plant_name) %>%
  summarise(frequency = n())

check2 <- randwick_fuzzy_filtered %>%
  group_by(wpw_plant_name) %>%
  summarise(frequency = n())

# remove mistakes
remove2 <- randwick_fuzzy_filtered %>%
  filter(randwick_plant_name == "Restio tetraphyllus" & wpw_plant_name == "Baloskion tetraphyllum" | 
           randwick_plant_name == "Baurea rubioides" & wpw_plant_name == "Bauera rubioides" | 
           randwick_plant_name == "Isolepis nodosa" & wpw_plant_name == "Ficinia nodosa" | 
           randwick_plant_name == "Melalueca armillaris" & wpw_plant_name == "Melaleuca armillaris" |
           randwick_plant_name == "Philotheca myropoides" & wpw_plant_name == "Philotheca myoporoides" | 
           randwick_plant_name == "Themeda australis" & wpw_plant_name == "Themeda triandra")

randwick_fuzzy_filtered <- anti_join(randwick_fuzzy_filtered, remove2)

# out of the 435 species in the Waverley list, 243 match with wpw

write.csv(randwick_fuzzy_filtered,"Master_database_output/waverley_council/randwick_species_list_match.csv", row.names=FALSE)

# check the suitability data from Farzin to get rid of removed species

wpw_species <- all_entities_short %>%
  distinct(plant_name)

farzin <- read.csv("Master_database_input/Waverley/species_suitability_farzin.csv")

diff <- setdiff(farzin, wpw_species)

##################################################################################################################
##################################################################################################################
# check the weeds

weeds <- read.csv("Master_database_input/weeds/weeds_state_lists.csv")

wpw_species <- all_entities_short %>%
  distinct(plant_name)

library(fuzzyjoin)

weeds_fuzzy <- stringdist_join(weeds, wpw_species, 
                                  by = "plant_name",
                                  mode = "left",
                                  ignore_case = FALSE, 
                                  method = "jw", 
                                  max_dist = 99, 
                                  distance_col = "dist") 

# 0.000 is a perfect match
weeds_fuzzy_filtered <- filter(weeds_fuzzy, dist < 0.06) # can adjust based on the amount of similarity you want, I think this number works best for the waverley list
weeds_fuzzy_filtered <- arrange(weeds_fuzzy_filtered, plant_name.x)

# extract the species that did not match and check them against synonyms
matchedspecies <- as.data.frame(weeds_fuzzy_filtered$plant_name.x)
names(matchedspecies)[names(matchedspecies) == 'weeds_fuzzy_filtered$plant_name.x'] <- 'plant_name'
mismatchedspecies <- anti_join(weeds, matchedspecies, by = "plant_name")

# fuzzy match the synonyms
weeds_syn_fuzzy <- stringdist_join(mismatchedspecies, gbif_synonyms, 
                                      by = c("plant_name" = "canonicalName"),
                                      mode = "left",
                                      ignore_case = FALSE, 
                                      method = "jw", 
                                      max_dist = 99, 
                                      distance_col = "dist") 

weeds_syn_fuzzy <- filter(weeds_syn_fuzzy, dist < 0.06)

# join these to the main list
names(weeds_fuzzy_filtered)[names(weeds_fuzzy_filtered) == 'plant_name.x'] <- 'weed_plant_name'
names(weeds_fuzzy_filtered)[names(weeds_fuzzy_filtered) == 'plant_name.y'] <- 'wpw_plant_name'

names(weeds_syn_fuzzy)[names(weeds_syn_fuzzy) == 'plant_name'] <- 'weed_plant_name'
names(weeds_syn_fuzzy)[names(weeds_syn_fuzzy) == 'species'] <- 'wpw_plant_name'
weeds_syn_fuzzy <- select(weeds_syn_fuzzy, -canonicalName)

weeds_fuzzy_filtered <- rbind(weeds_fuzzy_filtered, weeds_syn_fuzzy)

write.csv(weeds_fuzzy_filtered,"Master_database_output/weeds/weeds_state_list_matched.csv", row.names=FALSE)

####################################################
# extract traits for Waverley Council

wanted_traits <- all_entities_short %>%
  select(plant_name, origin, climber, cycad, fern, "grass-like", herbaceous, palm, shrub, succulent, tree, 
         bird, insect, mammal_lizard, animal_pollinated, trait_name, value) %>%
  filter(trait_name == "common name" | trait_name == "shade tolerance" | trait_name == "leaf loss" | trait_name == "average height" | trait_name == "average width" | 
         trait_name == "soil texture" | trait_name == "soil pH" | trait_name == "urban context" | 
         trait_name == "uses" | trait_name == "coastal tolerance" | trait_name == "drought tolerance" | 
         trait_name == "weed status in Australia")

# rename the grass column so it's easier
names(wanted_traits)[names(wanted_traits) == 'grass-like'] <- 'grass'

### make form into long format
# select just the form columns

form <- wanted_traits %>%
  select(plant_name, climber, cycad, fern, grass, herbaceous, palm, shrub, succulent, tree)

form <- form %>%
  mutate_if(is.factor, as.character) %>%
  mutate(climber = if_else(climber == "1", "climber", ""),
         cycad = if_else(cycad == "1", "cycad", ""),
         fern = if_else(fern == "1", "fern", ""),
         grass = if_else(grass == "1", "grass", ""),
         herbaceous = if_else(herbaceous == "1", "herbaceous", ""),
         palm = if_else(palm == "1", "palm", ""), 
         shrub = if_else(shrub == "1", "shrub", ""), 
         succulent = if_else(succulent == "1", "succulent", ""),
         tree = if_else(tree == "1", "tree", ""))

# change the blank cells into NAs
# https://stackoverflow.com/questions/24172111/change-the-blank-cells-to-na
form <- form %>% 
  mutate_all(na_if,"")

# one line for each species
form <- form %>%
  distinct(plant_name, climber, cycad, fern, grass, herbaceous, palm, shrub, succulent, tree)

# join the forms
# https://stackoverflow.com/questions/13944078/concatenate-rows-of-a-data-frame
form <- form %>%
  unite(form_new, climber, cycad, fern, grass, herbaceous, palm, shrub, succulent, tree, sep = ", ", na.rm = TRUE)

names(form)[names(form) == 'form_new'] <- 'form'

# join to main dataset
wanted_traits <- wanted_traits %>%
  select(-climber, -cycad, -fern, -grass, -herbaceous, -palm, -shrub, -succulent, -tree)

wanted_traits <- left_join(wanted_traits, form, by = "plant_name")

### make biodiversity into long format
# select just the biodiversity columns

diversity <- wanted_traits %>%
  select(plant_name, bird, insect, mammal_lizard, animal_pollinated)

diversity <- diversity %>%
  mutate_if(is.factor, as.character) %>%
  mutate(bird = if_else(bird == "1", "bird", ""),
         insect = if_else(insect == "1", "insect", ""),
         mammal_lizard = if_else(mammal_lizard == "1", "mammal/lizard", ""),
         animal_pollinated = if_else(animal_pollinated == "1", "pollinator", ""))

# change the blank cells into NAs
diversity <- diversity %>% 
  mutate_all(na_if,"")

# one line for each species
diversity <- diversity %>%
  distinct(plant_name, bird, insect, mammal_lizard, animal_pollinated)

# join the forms
diversity <- diversity %>%
  unite(biodiversity, bird, insect, mammal_lizard, animal_pollinated, sep = ", ", na.rm = TRUE)

diversity <- diversity %>% 
  mutate_all(na_if,"")

# join to main dataset
wanted_traits <- wanted_traits %>%
  select(-bird, -insect, -mammal_lizard, -animal_pollinated)

wanted_traits <- left_join(wanted_traits, diversity, by = "plant_name")

### make the traits wide format
# extract the traits

council_traits <- wanted_traits %>%
  select(plant_name, trait_name, value)

council_traits[] <- lapply(council_traits, gsub, pattern = "putatively high", replacement = "high")
council_traits[] <- lapply(council_traits, gsub, pattern = "putatively moderate", replacement = "moderate")
council_traits[] <- lapply(council_traits, gsub, pattern = "putatively no", replacement = "no")

# join values for the same trait together
council_traits <- council_traits %>%
  group_by(plant_name, trait_name) %>%
  mutate(value_new = paste0(value, collapse = ", ")) %>%
  distinct(plant_name, trait_name, value_new)

# change to long format
council_traits_long <- spread(council_traits, trait_name, value_new, fill = NA)

# join to main dataset
wanted_traits <- wanted_traits %>%
  select(-trait_name, -value)

wanted_traits <- left_join(wanted_traits, council_traits_long, by = "plant_name")

wanted_traits <- distinct(wanted_traits, plant_name, .keep_all = TRUE)

# load the council lists
council_lists_combined <- read.csv("Master_database_input/Waverley/council_lists_combined.csv")

# RANDWICK
randwick <- council_lists_combined %>%
  filter(Council == "Randwick")

# add extra species 

randwick <- randwick %>%
  add_row(Council = "Randwick", List = "Extra", wpw_name = "Acacia decurrens") %>%
  add_row(Council = "Randwick", List = "Extra", wpw_name = "Acronychia imperforata") %>%
  add_row(Council = "Randwick", List = "Extra", wpw_name = "Alphitonia excelsa") %>%
  add_row(Council = "Randwick", List = "Extra", wpw_name = "Prostanthera ovalifolia") %>%
  add_row(Council = "Randwick", List = "Extra", wpw_name = "Melaleuca bracteata")
  
# attach suitabilities
randwick_suitability <- read.csv("Master_database_input/Waverley/randwick_suitability.csv")

randwick_suitability <- select(randwick_suitability, -Council)

# join to main dataset
randwick <- left_join(randwick, randwick_suitability, by = c("wpw_name" = "Species"))

# fill in the missing values for Callistemon citrinus
randwick[[7,5]] <- "SUITABLE"
randwick[[7,6]] <- "MARGINAL"
randwick[[7,7]] <- "MARGINAL"

# join on the traits
randwick <- left_join(randwick, wanted_traits, by = c("wpw_name" = "plant_name"))

randwick <- select(randwick, Council, List, council_name, wpw_name, "common name", X2030, X2050, X2070, origin, form, biodiversity, "leaf loss", "average height", "average width", "coastal tolerance", "drought tolerance", "shade tolerance", "soil texture", "soil pH", "urban context", uses)

names(randwick)[names(randwick) == 'X2030'] <- '2030'
names(randwick)[names(randwick) == 'X2050'] <- '2050'
names(randwick)[names(randwick) == 'X2070'] <- '2070'

randwick <- arrange(randwick, List, wpw_name)

# WAVERLEY
waverley <- council_lists_combined %>%
  filter(Council == "Waverley")

# add extra species 

waverley <- waverley %>%
  add_row(Council = "Waverley", List = "Extra", wpw_name = "Acacia maidenii") %>%
  add_row(Council = "Waverley", List = "Extra", wpw_name = "Alpinia caerulea") %>%
  add_row(Council = "Waverley", List = "Extra", wpw_name = "Banksia robur") %>%
  add_row(Council = "Waverley", List = "Extra", wpw_name = "Brachyscome multifida") %>%
  add_row(Council = "Waverley", List = "Extra", wpw_name = "Corymbia ficifolia")

# attach suitabilities
waverley_suitability <- read.csv("Master_database_input/Waverley/waverley_suitability.csv")

waverley_suitability <- select(waverley_suitability, -Council)

# join to main dataset
waverley <- left_join(waverley, waverley_suitability, by = c("wpw_name" = "Species"))

# fill in the missing values for Callistemon citrinus
waverley[[8,5]] <- "SUITABLE"
waverley[[8,6]] <- "MARGINAL"
waverley[[8,7]] <- "MARGINAL"

# join on the traits
waverley <- left_join(waverley, wanted_traits, by = c("wpw_name" = "plant_name"))

waverley <- select(waverley, Council, List, council_name, wpw_name, "common name", X2030, X2050, X2070, origin, form, biodiversity, "leaf loss", "average height", "average width", "coastal tolerance", "drought tolerance", "shade tolerance", "soil texture", "soil pH", "urban context", uses)

names(waverley)[names(waverley) == 'X2030'] <- '2030'
names(waverley)[names(waverley) == 'X2050'] <- '2050'
names(waverley)[names(waverley) == 'X2070'] <- '2070'

waverley <- arrange(waverley, List, wpw_name)

# WOOLLAHRA
woollahra <- council_lists_combined %>%
  filter(Council == "Woollahra")

# add extra species 

woollahra <- woollahra %>%
  add_row(Council = "Woollahra", List = "Extra", wpw_name = "Melicope elleryana") %>%
  add_row(Council = "Woollahra", List = "Extra", wpw_name = "Toona ciliata") %>%
  add_row(Council = "Woollahra", List = "Extra", wpw_name = "Melastoma malabathricum") %>%
  add_row(Council = "Woollahra", List = "Extra", wpw_name = "Flindersia xanthoxyla")

# attach suitabilities
woollahra_suitability <- read.csv("Master_database_input/Waverley/woollahra_suitability.csv")

woollahra_suitability <- select(woollahra_suitability, -Council)

# join to main dataset
woollahra <- left_join(woollahra, woollahra_suitability, by = c("wpw_name" = "Species"))

# fill in the missing values for Callistemon citrinus
woollahra[[8,5]] <- "SUITABLE"
woollahra[[8,6]] <- "MARGINAL"
woollahra[[8,7]] <- "MARGINAL"

# fill in the missing values for the extra species
woollahra[[48,5]] <- "SUITABLE"
woollahra[[48,6]] <- "SUITABLE"
woollahra[[48,7]] <- "SUITABLE"
woollahra[[49,5]] <- "SUITABLE"
woollahra[[49,6]] <- "SUITABLE"
woollahra[[49,7]] <- "SUITABLE"
woollahra[[50,5]] <- "SUITABLE"
woollahra[[50,6]] <- "SUITABLE"
woollahra[[50,7]] <- "SUITABLE"
woollahra[[51,5]] <- "SUITABLE"
woollahra[[51,6]] <- "SUITABLE"
woollahra[[51,7]] <- "SUITABLE"

# join on the traits
woollahra <- left_join(woollahra, wanted_traits, by = c("wpw_name" = "plant_name"))

woollahra <- select(woollahra, Council, List, council_name, wpw_name, "common name", X2030, X2050, X2070, origin, form, biodiversity, "leaf loss", "average height", "average width", "coastal tolerance", "drought tolerance", "shade tolerance", "soil texture", "soil pH", "urban context", uses)

names(woollahra)[names(woollahra) == 'X2030'] <- '2030'
names(woollahra)[names(woollahra) == 'X2050'] <- '2050'
names(woollahra)[names(woollahra) == 'X2070'] <- '2070'

woollahra <- arrange(woollahra, List, wpw_name)








  
  


