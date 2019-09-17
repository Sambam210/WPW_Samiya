library(dplyr)

Dave <- read.csv("Dave_data/UptodateDB.csv")

### I want to create a list of all the species in Dave's DB and figure out how many of the 49 traits they have values for

### STEP 1: select the columns I am interested in

DaveNew <- Dave %>%
  select(newspecies, source, trait_name, value)

head(DaveNew)

DaveNew <- distinct(DaveNew,newspecies,trait_name, .keep_all = TRUE) # for each species and trait I just want one row

                                                                     # figured out from ?distinct

head(DaveNew)

DaveNew2 <- DaveNew %>%
  count(newspecies, sort = TRUE, name = "total_traits/49")

head(DaveNew2)

write.csv(DaveNew2,"Dave_data_output/traitcompleteness.csv",row.names=FALSE)

hist(DaveNew2$`total_traits/49`) # graph the data

### let's full out the species with 10-20 traits

ten_twenty <- filter(DaveNew2, `total_traits/49` > 10 & `total_traits/49` < 20 | `total_traits/49` == 10 | `total_traits/49` == 20)

ten_twenty <- select(missing, newspecies)

ten_twenty_whatwehave <- left_join(species_missing, DaveNew, by = "newspecies") # from David's full dataset this is the traits we have for the ten_twenty species

ten_twenty_whatwehave <- select(ten_twenty_whatwehave, -source, -value)

write.csv(ten_twenty_whatwehave,"Dave_data_output/ten_twenty_whatwehave.csv", row.names = FALSE)

### create a full matrix of traits and species 

davetraits <- read.csv("Data/davidtraitlist.csv") # David's full trait list

davetraits$trait_name <- as.character(davetraits$trait_name) # change factors into characters

davetraits <- as.vector(unlist(davetraits$trait_name)) # change the dataframe into a list of vectors
class(davetraits)                                      # https://stackoverflow.com/questions/7070173/convert-data-frame-column-to-a-vector

ten_twenty <- as.vector(unlist(ten_twenty$newspecies)) # change the dataframe into a list of vectors
class(ten_twenty) 

ten_twenty_alltraits <- expand.grid(ten_twenty, davetraits) # create a new dataframe of all combinations of two vectors
# https://stackoverflow.com/questions/11388359/unique-combination-of-all-elements-from-two-or-more-vectors

colnames(ten_twenty_alltraits) <- c("newspecies", "trait_name") # assign column names

ten_twenty_alltraits <- ten_twenty_alltraits %>%
  arrange(newspecies) # arrange by species

ten_twenty_missing <- setdiff(ten_twenty_alltraits, ten_twenty_whatwehave) # the traits that we are missing

write.csv(ten_twenty_missing,"Dave_data_output/ten_twenty_missing.csv", row.names = FALSE)

ten_twenty_missing_freq <- ten_twenty_missing %>%
  count(trait_name, sort = TRUE, name = "missing_frequency")

write.csv(ten_twenty_missing_freq,"Dave_data_output/ten_twenty_missing_freq.csv", row.names = FALSE)

#######################################################################################################################################

# what about getting rid of the traits that are "risk factors"?

# DaveNew has found 4356 species, how many of those are actual species (genus and species name) vs varieties etc

library(dplyr)
install.packages("stringr")
library(stringr)

Dave <- read.csv("Dave_data/UptodateDB.csv")

species <- Dave %>%
  select(newspecies) # only select the species column

species <- as.vector(unlist(species$newspecies)) # change species into a vector

x <- str_count(species,"[[:upper:]]")==1 # for each species, says with true or false if they have only one capital letter
                                         # https://stringr.tidyverse.org/articles/regular-expressions.html

y <- data.frame(species,x) # merge the two dataframes together

onlyspecies <- y %>%
  filter(x=="TRUE") %>% # just want all the species with one capital letter
  select(species) %>% # only want the species column and not the true column
  distinct(species) # only one line per species

##### 3618/4356 are species and not varieties, etc

##################################################################################################################################################################

# try and extract all the data that we have for each of the glasshouse species (can I just match species lists or do I have to run the code again?)

library(dplyr)

Dave <- read.csv("Dave_data/UptodateDB.csv")

DaveNew <- Dave %>%
  select(newspecies)

Renee<-read.csv("Data/ReneeSpecies.csv") # read in Renee's species list

Renee <- Renee %>%
  rename(newspecies=Species) # changing Renee's column name to match David's

missing<-setdiff(Renee,DaveNew) # there are 8 species that don't match but Dave said that 2 of those species had different names in his database
                                # let's change those names

# Renee[[12,1]] <- "Arthropodium cirrhatum" # this doesn't work!

glimpse(Renee) # species names seem to be factors and not characters!

Renee$newspecies <- as.character(Renee$newspecies) # change factors into characters
                                                   # https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters

glimpse(Renee) # worked!

Renee[[12,1]] <- "Arthropodium cirrhatum"
Renee[[45,1]] <- "Ficus microcarpa var hillii"

missing<-setdiff(Renee,DaveNew) # there are now 6 species that don't exist in Dave's db after those changes

DaveNew <- Dave %>%
  select(newspecies, source, trait_name, value) # let's subset David's data with the variables I am interested in

glspeciesdata<-left_join(Renee, DaveNew, by = "newspecies") # merge the two datasets

write.csv(glspeciesdata,"Dave_data_output/glspeciestraits.csv", row.names = FALSE)

##### let's find out what we don't know for each species out of the 49 traits

glspeciestraits <- read.csv("Dave_data_output/glspeciestraits.csv")

glspeciestraits <- glspeciestraits %>%
  select(newspecies, trait_name) %>% # let's subset for the variables we are interested in
  distinct(newspecies, trait_name, .keep_all = TRUE) # let's only have one trait per species combo

davetraits <- read.csv("Data/davidtraitlist.csv") # David's full trait list

davetraits$trait_name <- as.character(davetraits$trait_name) # change factors into characters

davetraits <- as.vector(unlist(davetraits$trait_name)) # change the dataframe into a list of vectors
class(davetraits)                                      # https://stackoverflow.com/questions/7070173/convert-data-frame-column-to-a-vector

Renee <- as.vector(unlist(Renee$newspecies)) # change the dataframe into a list of vectors
class(Renee) 

alltraitsallspecies <- expand.grid(Renee, davetraits) # create a new dataframe of all combinations of two vectors
                                                      # https://stackoverflow.com/questions/11388359/unique-combination-of-all-elements-from-two-or-more-vectors

colnames(alltraitsallspecies) <- c("newspecies", "trait_name") # assign column names

alltraitsallspecies <- alltraitsallspecies %>%
  arrange(newspecies) # arrange by species

whatweneed <- setdiff(alltraitsallspecies, glspeciestraits) # the traits that we are missing

whatweneed <- whatweneed %>%
  add_count(newspecies, sort = TRUE, name = "missing_trait_count") # count the number of traits we need for each species

whatweneed <- whatweneed %>%
  mutate(trait_count = (49 - missing_trait_count)) # how many traits we actually have

write.csv(whatweneed, "Dave_data_output/whatweneed.csv", row.names = FALSE)

whatweneed <- whatweneed %>%
  distinct(newspecies, .keep_all = TRUE) # let's only have one trait per species combo

hist(whatweneed$trait_count) # histogram of the traits we have

#####################################################################################################################################################

# let's extract info on the drought tolerance of the glasshouse species

library(tidyverse)

glspeciestraits <- read.csv("Dave_data_output/glspeciestraits.csv")

drought <- glspeciestraits %>%
  filter(trait_name == "drought_tolerance") # filter just the drought tolerance trait

str(drought)

drought_summary <- count(drought, value)
  
drought_summary <- drought %>%
  group_by(newspecies) %>%
  count(value) # count the number of yes and no for each species

# convert into long format
# https://stackoverflow.com/questions/34684925/how-to-use-the-spread-function-properly-in-tidyr

drought_summary_long <- spread(drought_summary, key = value, value = n, fill = 0) #90/113 species have drought tolerance info

# add a total records column
drought_summary_long <- mutate(drought_summary_long, total = (No + Yes))

# classify the strategy
# https://stackoverflow.com/questions/15016723/how-to-add-column-into-a-dataframe-based-on-condition 

drought_summary_long <- drought_summary_long %>%
  mutate(drought_tolerant = case_when(No > Yes ~ 'No',
                                      Yes > No ~ 'Yes',
                                      TRUE ~ 'Mixed'))

write.csv(drought_summary_long, "Dave_data_output/gh_drought_summary.csv", row.names = FALSE)

#####################################################################################################################################

# filter species according to minimum selection criteria identified by advisory board
# they want info on:
# drought_tolerance
# frost_tolerance
# coastal_tolerance
# light_level
# form
# height
# width

# First, let's see the frequency of these traits within the database

# let's load the data

Dave <- read.csv("Dave_data/UptodateDB.csv")

library(tidyverse)

# select the columns I am interested in

DaveNew <- Dave %>%
  select(newspecies, source, trait_name, value)

head(DaveNew)

DaveNew <- distinct(DaveNew,newspecies,trait_name, .keep_all = TRUE) # for each species and trait I just want one row

                                                                     # figured out from ?distinct

# subset only the traits I am interested in

DaveNew <- DaveNew %>%
  filter(trait_name == "drought_tolerance" | trait_name == "frost_tolerance" | trait_name == "coastal_tolerance" |
         trait_name == "light_level" | trait_name == "form" | trait_name == "max_height" | trait_name == "height" |
         trait_name == "min_height" | trait_name == "max_width" | trait_name == "width" | trait_name == "min_width")

# frequency of each trait

selection_trait_frequency <- DaveNew %>%
  add_count(trait_name, sort = TRUE, name = "frequency") %>%
  select(-newspecies, -source, -value) %>%
  distinct(trait_name, .keep_all = TRUE) %>%
  mutate(completeness = (frequency / 4356)) # completeness of each trait, there are 4356 unique 'species' in the database

write.csv(selection_trait_frequency, "Dave_data_output/advisory_board_min_traits/selection_trait_frequency.csv", row.names = FALSE)



