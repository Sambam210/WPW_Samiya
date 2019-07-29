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


Samiya<-read.csv("Data/Leaf drop_Samiya.csv") # uploading my deciduous/evergreen data


Reneeful<-read.csv("Data/RankingTraits_Control_2019_Renee.csv") # uploading Renee's full dataset


Samiya <- Samiya %>%
  rename(Species=searchTaxon) # changing the column name of my dataset to match Renee's

names(Samiya)

Samiyanew <- Samiya %>%
  select(Species, 
         Deciduous.or.evergreen, 
         Leaf.senescence.in.response.to.drought) # subsetting my data so it only contains the 'species' and evergreen/deciduous' info

names(Samiyanew)

newdata<-left_join(Samiyanew, Reneeful, by = "Species") # merging Renee's info on habit and physiology with my info on evergreen/deciduous

SamiyaandRenee <- newdata %>%
  select(Species, 
         Species_Code,
         Origin,
         Growth_Form,
         Native_Woody,
         Deciduous.or.evergreen,
         Leaf.senescence.in.response.to.drought,
         OsmPot_MPa,
         TLP_MPa) # reordering the columns so they are tidyer

Hughfull<-read.csv("Data/niche.data.HB.csv") # uploading Hugh's full dataset

all(Hughfull$searchTaxon == Hughfull$searchTaxon.20) # there are 2 'searchTaxon' columns, checking if they are the same, they are

Hughfull <- Hughfull %>%
  rename(Species=searchTaxon) # changing Hugh's column name to match Renee's and mine

HughAI <- Hughfull %>%
  select(Species,
         AI_min:AI_q98_q02) # subsetting Hugh's data so it only contains the aridity index variables

SamiyaandReneeandHugh <- left_join(SamiyaandRenee, HughAI, by = "Species") # combining all the data

write.csv(SamiyaandReneeandHugh,"Data/SamiyaandReneeandHugh.csv",row.names=FALSE) # save as csv into "Data" folder


SamiyaandReneeandHugh[rowSums(is.na(SamiyaandReneeandHugh)) > 0,] # find NAs in dataset, there are 2 species
                                                                  # https://stackoverflow.com/questions/7980622/subset-of-rows-containing-na-missing-values-in-a-chosen-column-of-a-data-frame

# R has somehow lost the infor for 'Kennedia beckxiana' during the merge of Samiya and Renee data

# Replacing the 'Kennedia beckxiana' missing values
SamiyaandReneeandHugh[[60,2]] <- "Kebe"
SamiyaandReneeandHugh[[60,3]] <- "Native"
SamiyaandReneeandHugh[[60,4]] <- "Climber/Groundcover"
SamiyaandReneeandHugh[[60,5]] <- "" # replacing the NA with a blank
SamiyaandReneeandHugh[[60,8]] <- "-0.88"
SamiyaandReneeandHugh[[60,9]] <- "-1.05"

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

