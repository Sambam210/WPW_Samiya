# check difference between Hugh and Renee's species lists

library(dplyr)

Renee<-read.csv("Data/ReneeSpecies.csv")
Hugh<-read.csv("Data/HughSpecies.csv")

# arrange species by alphabetical order

Renee<-arrange(Renee,Species)
Hugh<-arrange(Hugh,Species)

# are the two datasets the same?
# https://stackoverflow.com/questions/3171426/compare-two-data-frames-to-find-the-rows-in-data-frame-1-that-are-not-present-in

setdiff(Renee,Hugh) # returns the species that are different

semi_join(Renee,Hugh) # returns all the species that are the same

anti_join(Renee,Hugh) # returns the species that are different


###################################################################################################################################
                                    # COMBINING HUGH'S CLIMATE, RENEE'S PHYSIOLOGY AND MY EVERGREEN DATA #
###################################################################################################################################

#################################################### adding manually collected traits #############################################

Samiya<-read.csv("Data/Leaf drop_Samiya.csv") # uploading my deciduous/evergreen data


Reneeful<-read.csv("Data/RankingTraits_Control_2019_Renee.csv") # uploading Renee's full dataset


Samiya <- Samiya %>%
  rename(Species=searchTaxon,
         Deciduous_or_evergreen=Deciduous.or.evergreen,
         Leaf_senescence_in_response_to_drought=Leaf.senescence.in.response.to.drought,
         Water_storage=Water.storage,
         Hairs=Trichomes.hairs) # changing the column name of my dataset to match Renee's & getting rid of spaces

names(Samiya)

Samiyanew <- Samiya %>%
  select(Species, 
         Deciduous_or_evergreen, 
         Leaf_senescence_in_response_to_drought,
         Water_storage,
         Hairs) # subsetting my data

names(Samiyanew)

newdata<-left_join(Samiyanew, Reneeful, by = "Species") # merging Renee's info on habit and physiology with my info on evergreen/deciduous

SamiyaandRenee <- newdata %>%
  select(Species, 
         Species_Code,
         Origin,
         Growth_Form,
         Woody,
         Deciduous_or_evergreen,
         Leaf_senescence_in_response_to_drought,
         Water_storage,
         Hairs) # reordering the columns so they are tidyer, getting rid of osmotic potential data becasue it's old

################################################################################################################################################

######################################################### adding on the glasshouse data ########################################################

# Osmotic potential

OsmPot <- read.csv("GH_data/WPW_GH_OSMPOT.csv") # Renee said it was clean therefore there is no 'clean' file

# looking for outliers
OsmPot <- OsmPot %>%
  filter(Treatment == 'C') %>% # filter for only the controls
  select(Species_Code, Batch, OsmPot_MPa) %>% # select relevant columns
  group_by(Species_Code, Batch) %>% # group by species code and batch
  summarise(mean_OsmPot = mean(OsmPot_MPa, na.rm = TRUE)) # removes the NAs so the means work

library(ggplot2)

ggplot(OsmPot, aes(x = Batch, y = mean_OsmPot)) +
  geom_boxplot() +
  theme_bw()

# adding the data to the master spreadsheet

OsmPot <- read.csv("GH_data/WPW_GH_OSMPOT.csv") # Renee said it was clean therefore there is no 'clean' file

OsmPot <- OsmPot %>%
  filter(Treatment == 'C') %>% # filter for only the controls
  select(Species_Code, OsmPot_MPa) %>% # select relevant columns
  group_by(Species_Code) %>% # group by species code
  summarise(mean_OsmPot = mean(OsmPot_MPa, na.rm = TRUE)) # removes the NAs so the means work

SamiyaandRenee <- left_join(SamiyaandRenee, OsmPot, by = "Species_Code") # joining the osmotic potential data

# LMA, thickness, succulence

LMA <- read.csv("GH_data/WPW_GH_LMA_clean.csv") # Renee uploaded an updated version of the LMA data with errors fixed

# looking for outliers

LMA <- LMA %>%
  filter(Treatment == 'C') %>% # filter for only the controls
  select(Species_Code, Batch, Fresh_Weight_g, Dry_Weight_g, Thickness_mm_Avg, LMA_gm2) %>% # select relevant columns
  mutate(Weight_diff_g = Fresh_Weight_g - Dry_Weight_g) %>% # calculate index of succulence
  group_by(Species_Code, Batch) %>% # group by species code and batch
  summarise(mean_LMA_gm2 = mean(LMA_gm2, na.rm = TRUE),
            Thickness_mm_Avg = mean(Thickness_mm_Avg, na.rm = TRUE),
            mean_succulance_g = mean(Weight_diff_g, na.rm = TRUE)) # removes the NAs so the means work

ggplot(LMA, aes(x = Batch, y = mean_LMA_gm2)) +
  geom_boxplot() +
  theme_bw()

ggplot(LMA, aes(x = Batch, y = Thickness_mm_Avg)) +
  geom_boxplot() +
  theme_bw()

ggplot(LMA, aes(x = Batch, y = mean_succulance_g)) +
  geom_boxplot() +
  theme_bw()

# adding the data to the master spreadsheet

LMA <- read.csv("GH_data/WPW_GH_LMA_clean.csv") # Renee uploaded an updated version of the LMA data with errors fixed

# remove ACSM plants that have wrong leaf areas and therefore LMA
LMA <- LMA %>%
  filter(Sample_ID != "Acsm6C", Sample_ID != "Acsm8C", Sample_ID != "Acsm9C", 
         Sample_ID != "Acsm10C")

LMA <- LMA %>%
  filter(Treatment == 'C') %>% # filter for only the controls
  select(Species_Code, Fresh_Weight_g, Dry_Weight_g, Thickness_mm_Avg, LMA_gm2) %>% # select relevant columns
  mutate(Weight_diff_g = Fresh_Weight_g - Dry_Weight_g) %>% # calculate index of succulence
  group_by(Species_Code) %>% # group by species code
  summarise(mean_LMA_gm2 = mean(LMA_gm2, na.rm = TRUE),
            Thickness_mm_Avg = mean(Thickness_mm_Avg, na.rm = TRUE),
            mean_succulance_g = mean(Weight_diff_g, na.rm = TRUE)) # removes the NAs so the means work

SamiyaandRenee <- left_join(SamiyaandRenee, LMA, by = "Species_Code") # joining the LMA data

# leaf area and lobosity (I calculated these)

leaf_area <- read.csv("Data/leaf_lobosity_area.csv")

leaf_area <- leaf_area %>%
  filter(Treatment == 'C') %>% # filter for only the controls
  select(Species_Code, Batch, Area_cm, Lobosity_adjusted) %>% # select relevant columns
  group_by(Species_Code, Batch) %>% # group by species code and batch
  summarise(mean_leaf_area_cm2 = mean(Area_cm, na.rm = TRUE),
            mean_Lobosity = mean(Lobosity_adjusted, na.rm = TRUE))

ggplot(leaf_area, aes(x = Batch, y = mean_leaf_area_cm2)) +
  geom_boxplot() +
  theme_bw()

filter(leaf_area, mean_leaf_area_cm2 > 75) # no errors, just bog leaves

ggplot(leaf_area, aes(x = Batch, y = mean_Lobosity)) +
  geom_boxplot() +
  theme_bw()

# adding to the master spreadsheet

leaf_area <- read.csv("Data/leaf_lobosity_area.csv")

leaf_area <- leaf_area %>%
  filter(Treatment == 'C') %>% # filter for only the controls
  select(Species_Code, Area_cm, Lobosity_adjusted) %>% # select relevant columns
  group_by(Species_Code) %>% # group by species code and batch
  summarise(mean_leaf_area_cm2 = mean(Area_cm, na.rm = TRUE),
            mean_Lobosity = mean(Lobosity_adjusted, na.rm = TRUE))

SamiyaandRenee <- left_join(SamiyaandRenee, leaf_area, by = "Species_Code") # joining the data

################################################################################################################################################

######################################################## adding climate data ###################################################################

Hughfull<-read.csv("Data/niche.data.HB.csv") # uploading Hugh's full dataset

all(Hughfull$searchTaxon == Hughfull$searchTaxon.20) # there are 2 'searchTaxon' columns, checking if they are the same, they are

Hughfull <- Hughfull %>%
  rename(Species=searchTaxon) # changing Hugh's column name to match Renee's and mine

HughAI <- Hughfull %>%
  select(Species,
         AI_min:AI_q98_q02) # subsetting Hugh's data so it only contains the aridity index variables

SamiyaandReneeandHugh <- left_join(SamiyaandRenee, HughAI, by = "Species") # combining all the data

SamiyaandReneeandHugh[rowSums(is.na(SamiyaandReneeandHugh)) > 0,] # find NAs in dataset, there are 2 species
                                                                  # https://stackoverflow.com/questions/7980622/subset-of-rows-containing-na-missing-values-in-a-chosen-column-of-a-data-frame

# R has somehow lost the info for 'Kennedia beckxiana' during the merge of Samiya and Renee data

# Replacing the 'Kennedia beckxiana' missing values
SamiyaandReneeandHugh[[60,2]] <- "Kebe"
SamiyaandReneeandHugh[[60,3]] <- "Native"
SamiyaandReneeandHugh[[60,4]] <- "Climber"
SamiyaandReneeandHugh[[60,5]] <- "yes"
SamiyaandReneeandHugh[[60,10]] <- "-0.882" # osmotic potential value
SamiyaandReneeandHugh[[60,11]] <- "43.272" # LMA value
SamiyaandReneeandHugh[[60,12]] <- "0.179" # thickness value
SamiyaandReneeandHugh[[60,13]] <- "0.53442" # succulence value
SamiyaandReneeandHugh[[60,14]] <- "12.9651354" # leaf area
SamiyaandReneeandHugh[[60,15]] <- "1.0000000" # lobosity

write.csv(SamiyaandReneeandHugh,"Data_output/SamiyaandReneeandHugh.csv",row.names=FALSE) # save as csv into "Data" folder

#####################################################################################################################################

# Corroborating my glasshouse list with what Nikki gave me

library(dplyr)

pots <- read.csv("Data/List_Of_Pots.csv")
mylist <- read.csv("Data_output/SamiyaandReneeandHugh.csv")

mylist <- mylist %>%
  select(Species)

pots <- pots %>%
  select(Species) %>%
  distinct(Species)

diff <- setdiff(pots,mylist)

# there are 13 differences
 # 1. Correa alba - is actually Correa pulchella
 # 2. Cupaniopsis anacardiodes - is Cupaniopsis anacardioides - spelling mistake
 # 3. Magnolia grandiflora has a space at end
 # 4. Brachychiton populneus has a space at end
 # 5. Eremophila bignoniiflora has a space at end
 # 6. Acmena smithii - is actually Syzygium smithii (note that both species appear in Nikki's list)
 # 7. Betula utilis 'Jacquemontii' - is just Betula utilis
 # 8. Cynodon dactylon x C. transvaalensis - is just Cynodon dactylon
 # 9. Lagerstroemia indica x fauriei - is just Lagerstroemia indica
 # 10. Liquidamber styraciflua - is Liquidambar styraciflua - spelling mistake
 # 11. Platanus x acerifolia - is Platanus acerifolia
 # 12. Prunus cerasifera has a space at end
 # 13. Eucalyptus populnea has a space at end

# let's match by species code

mylistcode <- mylist %>%
  select(Species_Code)

potscode <- pots %>%
  select(Species_Code) %>%
  distinct(Species_Code)

diff <- setdiff(potscode,mylistcode)

# NO DIFFERENCES!

                              # LESSON - match by species_code and not species name when joining to HIEv data!!!! #

######################################################################################################################################

