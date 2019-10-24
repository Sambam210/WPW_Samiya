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

# transform fresh and dry weights into LDMC and aslo add density (LMA/thickness (gm^-3)) see Bartlett et al 2012
LDMCandDensity <- LMA %>%
  filter(Treatment == "C") %>%
  select(Species, Species_Code, Fresh_Weight_g, Dry_Weight_g, LMA_gm2, Thickness_mm_Avg) %>%
  mutate(LDMC = (Dry_Weight_g/Fresh_Weight_g),
         Thickness_m_Avg = (Thickness_mm_Avg*0.001), # change units
         LMA_kgm2 = (LMA_gm2*0.001), # change units
         Density = (LMA_kgm2/Thickness_m_Avg)) %>%
  group_by(Species_Code) %>%
  summarise(mean_LDMC = mean(LDMC, na.rm = TRUE),
            mean_Density_kgm3 = mean(Density, na.rm = TRUE))

# join to master spreadsheet
alltraits <- left_join(traits, LDMCandDensity, by = "Species_Code")

# join everything together
everything <- full_join(classification,alltraits, by = "Species_Code")

# arrange in alphabetical order by species code
everything <- arrange(everything, Species_Code)

# save output
write.csv(everything,"Cluster_traits_analysis_data/traits_and_cluster.csv",row.names=FALSE)

#######################################################################################################################################

#####                                                     CREATING BOXPLOTS

#######################################################################################################################################

library(tidyverse)
library(ggplot2)

# load data

everything <- read.csv("Cluster_traits_analysis_data/traits_and_cluster.csv")

# filter out all the species with NAs
# https://stackoverflow.com/questions/26665319/removing-na-in-dplyr-pipe

everything <- drop_na(everything) # ended up with 90 species

# need to change classification into long format
everything <- gather(everything, key="method", value="classification", -Species_Code, -Origin, -Growth_Form, -Growth_structure,
                     -mean_OsmPot, -mean_LMA_gm2, -Thickness_mm_Avg, -mean_succulance_g, -mean_leaf_area_cm2, -mean_LDMC,
                     -mean_Density_kgm3)

# also change 

##### Let's compare the traits hort vs traits for each classification

## 1. drought tolerant
drought_tolerant <- filter(everything, classification == "drought tolerant")

# OsmPot

OsmPot <- ggplot(drought_tolerant, aes(x = method, y = mean_OsmPot)) +
  geom_boxplot(alpha=0.5, outlier.shape=NA)+
  labs(title="",x="Method", y = "Osmotic potential (MPa)")+
  geom_point(position=position_jitterdodge(),aes(color=method),alpha=0.6) + 
  scale_color_manual(values = c("#00AFBB", "#FC4E07")) +
  guides(color = FALSE) + # remove legend
  theme_bw()

OsmPot

# LMA

LMA <- ggplot(drought_tolerant, aes(x = method, y = mean_LMA_gm2)) +
  geom_boxplot(alpha=0.5, outlier.shape=NA)+
  labs(title="",x="Method", y = expression("LMA" ~ (g/m^{2}))) +
  geom_point(position=position_jitterdodge(),aes(color=method),alpha=0.6) + 
  scale_color_manual(values = c("#00AFBB", "#FC4E07")) +
  guides(color = FALSE) + # remove legend
  theme_bw()

LMA

# Thickness

Thickness <- ggplot(drought_tolerant, aes(x = method, y = Thickness_mm_Avg)) +
  geom_boxplot(alpha=0.5, outlier.shape=NA)+
  labs(title="",x="Method", y = "Thickness (mm)") +
  geom_point(position=position_jitterdodge(),aes(color=method),alpha=0.6) + 
  scale_color_manual(values = c("#00AFBB", "#FC4E07")) +
  guides(color = FALSE) + # remove legend
  theme_bw()

Thickness

# Leaf area
Leaf_area <- ggplot(drought_tolerant, aes(x = method, y = mean_leaf_area_cm2)) +
  geom_boxplot(alpha=0.5, outlier.shape=NA)+
  labs(title="",x="Method", y = expression("Leaf area" ~ (cm^{2}))) +
  geom_point(position=position_jitterdodge(),aes(color=method),alpha=0.6) + 
  scale_color_manual(values = c("#00AFBB", "#FC4E07")) +
  guides(color = FALSE) + # remove legend
  theme_bw()

Leaf_area

# LDMC

LDMC <- ggplot(drought_tolerant, aes(x = method, y = mean_LDMC)) +
  geom_boxplot(alpha=0.5, outlier.shape=NA)+
  labs(title="",x="Method", y = "LDMC") +
  geom_point(position=position_jitterdodge(),aes(color=method),alpha=0.6) + 
  scale_color_manual(values = c("#00AFBB", "#FC4E07")) +
  guides(color = FALSE) + # remove legend
  theme_bw()

LDMC

# join plots together

library(gridExtra)

grid.arrange(OsmPot, LMA, Thickness, Leaf_area, LDMC, nrow = 3, ncol = 2)
# saved plot manually, for some reason code wasn't working

## 2. Drought intolerant
drought_intolerant <- filter(everything, classification == "drought intolerant")

# OsmPot

OsmPot <- ggplot(drought_intolerant, aes(x = method, y = mean_OsmPot)) +
  geom_boxplot(alpha=0.5, outlier.shape=NA)+
  labs(title="",x="Method", y = "Osmotic potential (MPa)")+
  geom_point(position=position_jitterdodge(),aes(color=method),alpha=0.6) + 
  scale_color_manual(values = c("#00AFBB", "#FC4E07")) +
  guides(color = FALSE) + # remove legend
  theme_bw()

OsmPot

# LMA

LMA <- ggplot(drought_intolerant, aes(x = method, y = mean_LMA_gm2)) +
  geom_boxplot(alpha=0.5, outlier.shape=NA)+
  labs(title="",x="Method", y = expression("LMA" ~ (g/m^{2}))) +
  geom_point(position=position_jitterdodge(),aes(color=method),alpha=0.6) + 
  scale_color_manual(values = c("#00AFBB", "#FC4E07")) +
  guides(color = FALSE) + # remove legend
  theme_bw()

LMA

# Thickness

Thickness <- ggplot(drought_intolerant, aes(x = method, y = Thickness_mm_Avg)) +
  geom_boxplot(alpha=0.5, outlier.shape=NA)+
  labs(title="",x="Method", y = "Thickness (mm)") +
  geom_point(position=position_jitterdodge(),aes(color=method),alpha=0.6) + 
  scale_color_manual(values = c("#00AFBB", "#FC4E07")) +
  guides(color = FALSE) + # remove legend
  theme_bw()

Thickness

# Leaf area
Leaf_area <- ggplot(drought_intolerant, aes(x = method, y = mean_leaf_area_cm2)) +
  geom_boxplot(alpha=0.5, outlier.shape=NA)+
  labs(title="",x="Method", y = expression("Leaf area" ~ (cm^{2}))) +
  geom_point(position=position_jitterdodge(),aes(color=method),alpha=0.6) + 
  scale_color_manual(values = c("#00AFBB", "#FC4E07")) +
  guides(color = FALSE) + # remove legend
  theme_bw()

Leaf_area

# LDMC

LDMC <- ggplot(drought_intolerant, aes(x = method, y = mean_LDMC)) +
  geom_boxplot(alpha=0.5, outlier.shape=NA)+
  labs(title="",x="Method", y = "LDMC") +
  geom_point(position=position_jitterdodge(),aes(color=method),alpha=0.6) + 
  scale_color_manual(values = c("#00AFBB", "#FC4E07")) +
  guides(color = FALSE) + # remove legend
  theme_bw()

LDMC

# join plots together

library(gridExtra)

grid.arrange(OsmPot, LMA, Thickness, Leaf_area, LDMC, nrow = 3, ncol = 2)
# saved plot manually, for some reason code wasn't working

# PLOTTING EVERYTHING TOGETHER

OsmPot <- ggplot(everything, aes(x = method, y = mean_OsmPot, fill = classification)) +
  geom_boxplot(aes(fill=classification),alpha=0.5, outlier.shape=NA) +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07", "#E7B800")) +
  labs(title="",x="Method", y = "Osmotic potential (MPa)") +
  theme_bw() +
  theme(legend.position = "none") # remove legend

OsmPot

LMA <- ggplot(everything, aes(x = method, y = mean_LMA_gm2, fill = classification)) +
  geom_boxplot(aes(fill=classification),alpha=0.5, outlier.shape=NA) +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07", "#E7B800")) +
  labs(title="",x="Method", y = expression("LMA" ~ (g/m^{2}))) +
  theme_bw() +
  theme(legend.position = "none") # remove legend

LMA

Thickness <- ggplot(everything, aes(x = method, y = Thickness_mm_Avg, fill = classification)) +
  geom_boxplot(aes(fill=classification),alpha=0.5, outlier.shape=NA) +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07", "#E7B800")) +
  labs(title="",x="Method", y = "Thickness (mm)") +
  theme_bw() +
  theme(legend.position = "none") # remove legend

Thickness

Leaf_area <- ggplot(everything, aes(x = method, y = mean_leaf_area_cm2, fill = classification)) +
  geom_boxplot(aes(fill=classification),alpha=0.5, outlier.shape=NA) +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07", "#E7B800")) +
  labs(title="",x="Method", y = expression("Leaf area" ~ (cm^{2}))) +
  theme_bw() +
  theme(legend.position = "none") # remove legend

Leaf_area

LDMC <- ggplot(everything, aes(x = method, y = mean_LDMC, fill = classification)) +
  geom_boxplot(aes(fill=classification),alpha=0.5, outlier.shape=NA) +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07", "#E7B800")) +
  labs(title="",x="Method", y = "LDMC") +
  theme_bw() +
  theme(legend.position = c(1.4,0.2)) # legend for all the plots

LDMC

grid.arrange(OsmPot, LMA, Thickness, Leaf_area, LDMC, nrow = 3, ncol = 2)
# saved plot manually, for some reason code wasn't working


