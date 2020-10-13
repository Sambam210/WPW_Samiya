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

