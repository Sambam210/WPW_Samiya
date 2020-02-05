# This prelim analysis is based on ground assessmnet of tree health conducted by Anthony and I on 29,30,31 Jan and 4 Feb 2020 around Penrith following
# a recent spate of heatwaves. Trees were visiually assessed with the following comments:

# healthy
# lightly scorched
# heavily scorched
# defolitated

# Trees that were removed haved also been documented

library(tidyverse)
library(ggplot2)

# load up the asessment scores

assessment <- read.csv("Western_Sydney_Heatwave_data/Assessment.csv")

# change the numeric scores to character strings and filter out the trees that were removed

assessment <- assessment %>%
  mutate(Score = case_when(Score == "2" ~ "lightly scorched",
                                Score == "3" ~ "heavily scorched",
                                Score == "4" ~ "defoliated")) %>%
  filter(Notes != "Removed")

# load up the inventory info from Ale
# not that this is not the original file, I did added a column called "Assessed" to document the trees we missed

inventory <- read.csv("Western_Sydney_Heatwave_data/Penrith_inventory_summary.csv")

# removed the trees that were not assessed

inventory <- filter(inventory, Assessed == "Y")

# merge the two datasets together according to ID

all <- left_join(inventory, assessment, by = "id")

# replace the NA in the Score column with healthy

all$Score[is.na(all$Score)] <- "healthy"

# filter out the NAs in Notes column to get species with wrong id

all$Notes[all$Notes == ""] <- NA # need to fill blank cells with NA first
wrong_id <- filter(all, !is.na(Notes))

# change the notes and species columns

wrong_id$Species <- wrong_id$Notes

# in the master datashett filter out the species with missing id

all <- filter(all, is.na(Notes))

# combine the two datasets

all <- bind_rows(all, wrong_id)

# arrange by id

all <- arrange(all, id)




