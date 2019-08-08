###################################################### HIEv Glasshouse Data ##########################################################
######################################################################################################################################
######################################################################################################################################

############################################################ Pot details #############################################################

library("dplyr")

PotDets <- read.csv("GH_data/WPW_GH_POTDETAILS.csv")

# check if the species code matches the first 4 letters of sample ID (after I fixed Poel)

PotDets$Sample_ID_short<-c(substr(PotDets$Sample_ID,1,4)) # extract the first 4 letters of Sample ID
head(PotDets)
Species_Code <- PotDets %>%
  select(Species_Code) %>%
  rename(Check=Species_Code)

Sample_ID_short <- PotDets %>%
  select(Sample_ID_short) %>%
  rename(Check=Sample_ID_short)

str(Sample_ID_short)
str(Species_Code) # need to change to character
Species_Code$Check <- as.character(Species_Code$Check)
str(Species_Code)

diff <- as.data.frame(Sample_ID_short == Species_Code) # TRUE of FALSE for whether they are different

cbind_check <- cbind(Species_Code, diff) # bind the two data frames together so I can have a species code
colnames(cbind_check) <- c("Species", "Check") # rename col names as both were called 'check'

filter(cbind_check, Check == FALSE) # Poel doesn't match!


# check that the treatment code matches the last letter of the species code

PotDets <- read.csv("GH_data/WPW_GH_POTDETAILS.csv")

# start with baseline
PotDets_baseline <- filter(PotDets, Treatment == "C" | Treatment == "D")

PotDets_baseline$Treatment_short<-c(substr(PotDets_baseline$Sample_ID,6,6)) # extract the last letter of Sample ID
head(PotDets_baseline)
Treatment_Code <- PotDets_baseline %>%
  select(Treatment) %>%
  rename(Check=Treatment)

Treatment_short <- PotDets_baseline %>%
  select(Treatment_short) %>%
  rename(Check=Treatment_short)

str(Treatment_short)
str(Treatment_Code) # need to change to character
Treatment_Code$Check <- as.character(Treatment_Code$Check)
str(Treatment_Code)

diff <- as.data.frame(Treatment_short == Treatment_Code) # TRUE of FALSE for whether they are different

cbind_check <- cbind(PotDets_baseline$Sample_ID, diff) # bind the two data frames together so I can have a species code

filter(cbind_check, Check == FALSE) # lots doesn't match!

# now heatwave
PotDets_heatwave <- filter(PotDets, Treatment == "C_HW" | Treatment == "D_HW")

PotDets_heatwave$Treatment_short<-c(substr(PotDets_heatwave$Sample_ID,6,6)) # extract the last letter of Sample ID
PotDets_heatwave$Treatment_new<-c(substr(PotDets_heatwave$Treatment,1,1)) # extract the first letter of treatment

head(PotDets_heatwave)
Treatment_Code <- PotDets_heatwave %>%
  select(Treatment_new) %>%
  rename(Check=Treatment_new)

Treatment_short <- PotDets_heatwave %>%
  select(Treatment_short) %>%
  rename(Check=Treatment_short)

str(Treatment_short)
str(Treatment_Code) # need to change to character
Treatment_Code$Check <- as.character(Treatment_Code$Check)
str(Treatment_Code)

diff <- as.data.frame(Treatment_short == Treatment_Code) # TRUE of FALSE for whether they are different

cbind_check <- cbind(PotDets_heatwave$Sample_ID, diff) # bind the two data frames together so I can have a species code

filter(cbind_check, Check == FALSE) # lots of differences!

# Check to see that the sample_id matches perfectly for baseline and heatwave plants

rm(list=ls()) # clear the environment

PotDets <- read.csv("GH_data/WPW_GH_POTDETAILS.csv")

# subset into baseline and heatwave
baseline <- filter(PotDets, Treatment == "C" | Treatment == "D")
heatwave <- filter(PotDets, Treatment == "C_HW" | Treatment == "D_HW")

# subset each dataset again
baseline <- baseline %>%
  select(Species_Code, Treatment, Sample_ID)

heatwave <- heatwave %>%
  select(Species_Code, Treatment, Sample_ID) %>%
  rename(Species_Code2=Species_Code, Treatment2=Treatment, Sample_ID2=Sample_ID) # need to rename the columns so they are different to baseline

# merge the datasets
baseline_heatwave <- cbind(baseline,heatwave)

baseline_heatwave$diff_Sample_ID <- (baseline_heatwave$Sample_ID == baseline_heatwave$Sample_ID2) # create a new column TRUE or FALSE if baseline equals heatwave sample id

filter(baseline_heatwave, diff_Sample_ID == FALSE) # both the same

baseline_heatwave$diff_Species_Code <- (baseline_heatwave$Species_Code == baseline_heatwave$Species_Code2) # create a new column TRUE or FALSE if baseline equals heatwave species code

filter(baseline_heatwave, diff_Species_Code == FALSE) # both the same!

########################################################## LMA #######################################################################

### QC/QA

# The formula they used for LMA is (dry weight/fresh weight)*10000. This is because area is in cm^2 and mass is in grams. LMA was calculated as g/m^2

library("dplyr")

LMA<-read.csv("GH_data/WPW_GH_LMA.csv")

str(LMA)

# subset for only the control and drought pots and columns I am interested in

LMA <- LMA %>%
  filter(Treatment == "C" | Treatment == "D") %>%
  select(Glasshouse, Experiment, Batch, Species, Species_Code, Treatment, 
         Sample_ID, Area, Fresh_Weight, Dry_Weight, Thickness_average, LMA)

# looks for outliers in the LMA data

plot(LMA~Thickness_average, LMA)

LMA_outliers <- filter(LMA, LMA > 300) # 3 entries

# look to see if any dry and fresh weights have been swapped around

weight_check <- LMA %>%
  mutate(diff = Fresh_Weight - Dry_Weight) %>%
  filter(diff < 0) # there are 39 errors (only 4 that are not in batch 3)

# let's swap the fresh and dry weights and recalculate LMA

weight_check_edit <- weight_check %>%
  rename(Dry_Weight=Fresh_Weight, Fresh_Weight=Dry_Weight) %>%
  mutate(LMA = (Dry_Weight/Fresh_Weight)*1000)

## replacing the values in the master dataset with the edited values

# first need to filter out the rows in which the difference was not negative OR there is an NA in the diff column, these are the rows that are correct
weight_check2 <- LMA %>%
  mutate(diff = Fresh_Weight - Dry_Weight) %>%
  filter(!diff < 0 | is.na(diff)) 

# new dataset with the fixed values
LMA_weight_fix <- bind_rows(weight_check_edit,weight_check2)

# arrange by sample ID and remove difference column
LMA_weight_fix <- arrange(LMA_weight_fix, Species_Code, Treatment)
LMA_weight_fix <- select(LMA_weight_fix, -diff)

# check if it worked, it did!
weight_check3 <- LMA_weight_fix %>%
  mutate(diff = Fresh_Weight - Dry_Weight) %>%
  filter(diff < 0)

# graph the amended LMA
plot(LMA~Thickness_average, LMA_weight_fix)

filter(LMA_weight_fix, LMA > 500) # Goov3C and Plac4C look like outliers even though their weights were never swapped

filter(LMA_weight_fix, Species_Code == "Goov" | Species_Code == "Plac")

# Goov3C definitely is an outlier (areas is v. small), Plac4C dry weight is also v. high

filter(LMA_weight_fix, LMA > 250)

filter(LMA_weight_fix, Species_Code == "Lolo" | Species_Code == "Poel" | Species_Code == "Stse")

# more potential outliers:
### Lolo2D weights look low
### Stse2D weights are too high

# check the spread of the data
LMA_summary <- LMA_weight_fix %>%
  group_by(Species_Code) %>%
  summarise(meanLMA = mean(LMA, na.rm = TRUE), sdLMA = sd(LMA, na.rm = TRUE), 
            rangeLMA = (max(LMA, na.rm = TRUE) - (min(LMA, na.rm=TRUE))))


# plot fresh and dry weight
plot(Fresh_Weight~Dry_Weight, LMA_weight_fix)

filter(LMA_weight_fix, Fresh_Weight > 30)

library(ggplot2)

ggplot(LMA_weight_fix, aes(x = Dry_Weight, y = Fresh_Weight, colour = Batch)) +
  geom_point() +
  theme_bw()

ggplot(LMA_weight_fix, aes(x = Batch, y = Fresh_Weight)) +
  geom_boxplot() +
  theme_bw()

# ALL OF BATCH 3 ARE OUTLIERS!!! Fresh weight and areas don't match with original datasheet
# ALSO Magr5D fresh weight too high

LMA_weight_fix_rmBt3 <- filter(LMA_weight_fix, Batch == "Bt0" | Batch == "Bt1" | Batch == "Bt2" | Batch == "Bt3.5" | Batch == "Bt4" | Batch == "Bt5" | Batch == "Bt6")

plot(Fresh_Weight~Dry_Weight, LMA_weight_fix_rmBt3)
# only Magr5D is an outlier

plot(Fresh_Weight~LMA, LMA_weight_fix_rmBt3)
filter(LMA_weight_fix_rmBt3, LMA > 300)
# same outliers are coming out (Goov3C, Lolo2D, Plac4C and Magr5D)

plot(Dry_Weight~LMA, LMA_weight_fix_rmBt3)
filter(LMA_weight_fix_rmBt3, LMA > 300)
filter(LMA_weight_fix_rmBt3, Dry_Weight > 3)

plot(Dry_Weight~Area, LMA_weight_fix_rmBt3)
filter(LMA_weight_fix_rmBt3, Area > 400)
# Placs have high areas
filter(LMA_weight_fix_rmBt3, Dry_Weight > 3 & Area > 200)

write.csv(LMA_weight_fix,"GH_data/LMA_weight_fix.csv",row.names=FALSE)
write.csv(LMA_weight_fix_rmBt3,"GH_data/LMA_weight_fix_rmBt3.csv",row.names=FALSE)

###### Replacing all the Batch 3 values with the values I have + fixed the weights

LMA_weight_fix_Bt3_fixed <- read.csv("GH_data/WPW_GH_LMA_weight_fix_Bt3_fixed.csv")

library("dplyr")

str(LMA_weight_fix_Bt3_fixed)

LMA_weight_fix_Bt3_fixed <- LMA_weight_fix_Bt3_fixed %>%
  filter(Treatment == "C" | Treatment == "D") %>%
  select(Glasshouse, Experiment, Batch, Species, Species_Code, Treatment, 
         Sample_ID, Area, Fresh_Weight, Dry_Weight, Thickness_average, LMA)

#let's calculate LMA and see if they did it right

# let's check which ones differ

LMA_weight_fix_Bt3_fixed <- LMA_weight_fix_Bt3_fixed %>%
  mutate(LMA_new = (Dry_Weight/Area)*10000) %>%
  mutate(diff = LMA_new - LMA)

filter(LMA_weight_fix_Bt3_fixed, diff > 3) # 4 differ significantly Mypa1C, 2C, 4C and Nado3C

LMA_weight_fix_Bt3_fixed <- select(LMA_weight_fix_Bt3_fixed, -diff) # remove columns

########### I will use my manually calculated LMA (LMA_new) values from now

# let's calculate the difference in fresh and dry weight
LMA_weight_fix_Bt3_fixed<- LMA_weight_fix_Bt3_fixed %>%
  mutate(weight_diff = Fresh_Weight - Dry_Weight)

# let's see it there are any mistakes
filter(LMA_weight_fix_Bt3_fixed, weight_diff < 0) # there are none

### exploratory plots

plot(LMA_new~Thickness_average, LMA_weight_fix_Bt3_fixed)

filter(LMA_weight_fix_Bt3_fixed, LMA_new > 250) # 3 entries Goov3C, Poel1D, Acbr1C

plot(Fresh_Weight~Dry_Weight, LMA_weight_fix_Bt3_fixed)
filter(LMA_weight_fix_Bt3_fixed, Fresh_Weight > 50) # Magr5D

plot(Fresh_Weight~LMA_new, LMA_weight_fix_Bt3_fixed)

plot(Dry_Weight~LMA_new, LMA_weight_fix_Bt3_fixed)

plot(Dry_Weight~Area, LMA_weight_fix_Bt3_fixed)

plot(LMA_new~Thickness_average, LMA_weight_fix_Bt3_fixed)
filter(LMA_weight_fix_Bt3_fixed, Thickness_average > 0.6) # 8 entries, all of which were Tuvi and Arci (thick leaves)

# check the spread of the data
LMA_summary_Bt3_fixed <- LMA_weight_fix_Bt3_fixed %>%
  group_by(Species_Code) %>%
  summarise(meanLMA = mean(LMA_new, na.rm = TRUE), sdLMA = sd(LMA_new, na.rm = TRUE), 
            rangeLMA = (max(LMA_new, na.rm = TRUE) - (min(LMA_new, na.rm=TRUE))))

filter(LMA_summary_Bt3_fixed, rangeLMA > meanLMA) # 16 species

# check the range of weight_diff
range(LMA_weight_fix_Bt3_fixed$weight_diff, na.rm = TRUE)

hist(LMA_weight_fix_Bt3_fixed$weight_diff)

filter(LMA_weight_fix_Bt3_fixed, weight_diff > 15) # Magr5D

write.csv(LMA_weight_fix_Bt3_fixed,"GH_data/LMA_weight_fix_Bt3_fixed.csv",row.names=FALSE)

################################################## Checking the new LMA data on HIEv (added 30th July) ###################################


library("dplyr")

LMA<-read.csv("GH_data/WPW_GH_LMA_clean.csv")

str(LMA)

# check if the species code matches the first 4 letters of sample ID

LMA$Sample_ID_short<-c(substr(LMA$Sample_ID,1,4)) # extract the first 4 letters of Sample ID
head(LMA)
Species_Code <- LMA %>%
  select(Species_Code) %>%
  rename(Check=Species_Code)

Sample_ID_short <- LMA %>%
  select(Sample_ID_short) %>%
  rename(Check=Sample_ID_short)

setdiff(Species_Code,Sample_ID_short) # no differences

# subset for only the control and drought pots and columns I am interested in

LMA <- LMA %>%
  filter(Treatment == "C" | Treatment == "D") %>%
  select(Glasshouse, Experiment, Batch, Species, Species_Code, Treatment, 
         Sample_ID, Area_cm2, Fresh_Weight_g, Dry_Weight_g, Thickness_mm_Avg, LMA_gm2)

# looks for outliers in the LMA data

plot(LMA_gm2~Thickness_mm_Avg, LMA) # looks good!

# look to see if any dry and fresh weights have been swapped around

weight_check <- LMA %>%
  mutate(diff = Fresh_Weight_g - Dry_Weight_g) %>%
  filter(diff < 0) # 0 errors!

# plot fresh and dry weight
plot(Fresh_Weight_g~Dry_Weight_g, LMA) # looks good

library(ggplot2)

ggplot(LMA, aes(x = Dry_Weight_g, y = Fresh_Weight_g, colour = Batch)) +
  geom_point() +
  theme_bw() # looks good

ggplot(LMA, aes(x = Batch, y = Fresh_Weight_g)) +
  geom_boxplot() +
  theme_bw() # looks good

plot(Fresh_Weight_g~LMA_gm2, LMA) # looks good

plot(Dry_Weight_g~LMA_gm2, LMA) # looks good

plot(Dry_Weight_g~Area_cm2, LMA) # looks good

#let's calculate LMA and see if they did it right

# let's check which ones differ
LMA <- LMA %>%
  mutate(LMA_new = (Dry_Weight_g/Area_cm2)*10000) %>%
  mutate(diff = LMA_new - LMA_gm2)

filter(LMA, diff > 3) # none differed!

LMA <- select(LMA, -diff, -LMA_new) # remove columns

########################################################## OSM POT #######################################################################

library(dplyr)
library(ggplot2)

Osm_Pot <- read.csv("GH_data/WPW_GH_OSMPOT.csv")

# select the columns I want

Osm_Pot <- Osm_Pot %>%
  select(Glasshouse, Experiment, Batch, Species, Species_Code, Treatment, Sample_ID, OsmPot_MPa)

ggplot(Osm_Pot, aes(x = Batch, y = OsmPot_MPa)) +
  geom_boxplot() +
  theme_bw()

# For each pot I want to look at the difference in osmotic potential between baseline and heatwave

# let's split the dataset into baseline and heatwave

baseline <- Osm_Pot %>%
  filter(Treatment == "C" | Treatment == "D")

heatwave <- Osm_Pot %>%
  filter(Treatment == "C_HW" | Treatment == "D_HW")

Osm_Pot_HW <- as.vector(unlist(heatwave$OsmPot_MPa)) # change osmotic potential into a vector

# create a new column in the baseline dataset with the difference between baseline and heatwave osmpotic potentials

baseline <- baseline %>%
  mutate(Osm_diff = OsmPot_MPa - Osm_Pot_HW)

hist(baseline$Osm_diff)

filter(baseline, Osm_diff > 1) # Dyfr5C has a big difference

filter(baseline, Osm_diff < -1)# Mupa4D also looks like it had a big difference

########################################################## T LEAF #######################################################################

library(dplyr)
library(ggplot2)

TLeaf <- read.csv("GH_data/WPW_GH_TLEAF.csv")

# select the columns I want

TLeaf <- TLeaf %>%
  select(Glasshouse, Experiment, Batch, Species, Species_Code, Treatment, Sample_ID, Tleaf_C_Avg, Tleaf_C_Max)

ggplot(TLeaf, aes(x = Batch, y = Tleaf_C_Avg)) +
  geom_boxplot() +
  theme_bw()

ggplot(TLeaf, aes(x = Batch, y = Tleaf_C_Max)) +
  geom_boxplot() +
  theme_bw() # there is an entry of 422 for Elre5D, I assume this is supposed to be 42.2?

plot(Tleaf_C_Max ~ Tleaf_C_Avg, ylim = c(0,100), TLeaf) # by removing Elre5D the distribution looks good

filter(TLeaf, Tleaf_C_Max > 45 & Tleaf_C_Avg < 40)
