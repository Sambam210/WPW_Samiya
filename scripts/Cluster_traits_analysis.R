## Now that we have hort and traits classifications....
# let's graph the traits for each group in the traits classification
# let's graph the traits of the species that change between classifications

library(tidyverse)

# load the classification data
# let's use the 75% concensus classification

classification <- read.csv("Ladderplot_data_input/hortandtraits_LDMC_75%concensus.csv")

# load the traits data

traits <- read.csv("PCA_Cluster_data/PCA_data.csv")

# filter the traits data

traits <- select(traits, -Species, -Deciduous_or_evergreen, -Leaf_senescence_in_response_to_drought, -Water_storage_organs,
                 -Leaf_hairs, -mean_Lobosity)


# load the LMA data
LMA <- read.csv("GH_data/WPW_GH_LMA_clean.csv")

# transform fresh and dry weights into LDMC
LDMC <- LMA %>%
  filter(Treatment == "C") %>%
  select(Species, Species_Code, Fresh_Weight_g, Dry_Weight_g) %>%
  mutate(LDMC = Dry_Weight_g/Fresh_Weight_g) %>%
  group_by(Species_Code) %>%
  summarise(mean_LDMC = mean(LDMC, na.rm = TRUE))

# join to master spreadsheet
alltraits <- left_join(traits, LDMC, by = "Species_Code")

# join everything together

everything <- left_join(classification,alltraits, by = "Species_Code") # doesn't work!!!
