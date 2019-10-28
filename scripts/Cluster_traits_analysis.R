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

# transform fresh and dry weights into LDMC and also add density (LMA/thickness (gm^-3)) see Bartlett et al 2012
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

# load the osmpot data
osmpot <- read.csv("GH_data/WPW_GH_OSMPOT.csv")

# transform osmpot into tlp using Batlett et al 2012 equation
osmpot <- osmpot %>%
  filter(Treatment == "C") %>%
  select(Species_Code, OsmPot_MPa) %>%
  mutate(TLP = (0.832 * OsmPot_MPa) - 0.631) %>%
  group_by(Species_Code) %>%
  summarise(mean_TLP = mean(TLP, na.rm = TRUE))

# join to master spreadsheet
alltraits <- left_join(alltraits, osmpot, by = "Species_Code")

# join everything together
everything <- full_join(classification,alltraits, by = "Species_Code")

# arrange in alphabetical order by species code
everything <- arrange(everything, Species_Code)

# save output
write.csv(everything,"Cluster_traits_analysis_data/traits_and_cluster.csv",row.names=FALSE)

#######################################################################################################################################

#####                                                     CREATING GRAPHS

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
                     -mean_Density_kgm3, -mean_TLP)

##### Let's compare the traits hort vs traits for each classification

## 1. drought tolerant
drought_tolerant <- filter(everything, classification == "drought tolerant")

# TLP

TLP <- ggplot(drought_tolerant, aes(x = method, y = mean_TLP)) +
  geom_boxplot(alpha=0.5, outlier.shape=NA)+
  labs(title="",x="Method", y = "TLP (MPa)")+
  geom_point(position=position_jitterdodge(),aes(color=method),alpha=0.6) + 
  scale_color_manual(values = c("#00AFBB", "#FC4E07")) +
  guides(color = FALSE) + # remove legend
  theme_bw()

TLP

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

grid.arrange(TLP, LMA, Thickness, Leaf_area, LDMC, nrow = 3, ncol = 2)
# saved plot manually, for some reason code wasn't working

## 2. Drought intolerant
drought_intolerant <- filter(everything, classification == "drought intolerant")

# TLP

TLP <- ggplot(drought_intolerant, aes(x = method, y = mean_TLP)) +
  geom_boxplot(alpha=0.5, outlier.shape=NA)+
  labs(title="",x="Method", y = "TLP (MPa)")+
  geom_point(position=position_jitterdodge(),aes(color=method),alpha=0.6) + 
  scale_color_manual(values = c("#00AFBB", "#FC4E07")) +
  guides(color = FALSE) + # remove legend
  theme_bw()

TLP

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

grid.arrange(TLP, LMA, Thickness, Leaf_area, LDMC, nrow = 3, ncol = 2)
# saved plot manually, for some reason code wasn't working

# PLOTTING EVERYTHING TOGETHER

TLP <- ggplot(everything, aes(x = method, y = mean_TLP, fill = classification)) +
  geom_boxplot(aes(fill=classification),alpha=0.5, outlier.shape=NA) +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07", "#E7B800")) +
  labs(title="",x="Method", y = "TLP (MPa)") +
  theme_bw() +
  theme(legend.position = "none") # remove legend

TLP

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

grid.arrange(TLP, LMA, Thickness, Leaf_area, LDMC, nrow = 3, ncol = 2)
# saved plot manually, for some reason code wasn't working

#### now let's look at how the traits are changing with changing hort classification

# load data

everything <- read.csv("Cluster_traits_analysis_data/traits_and_cluster.csv")

# filter out all the species with NAs
# https://stackoverflow.com/questions/26665319/removing-na-in-dplyr-pipe

everything <- drop_na(everything) # ended up with 90 species

### filter just for the drought tolerant hort classification
drought_tolerant <- filter(everything, hort_classification == "drought tolerant")

# need to create a new column for the changes in classification if there are any

drought_tolerant <- drought_tolerant %>%
  mutate(Changes = case_when(hort_classification == "drought tolerant" & traits_classification == "drought tolerant" ~ "DT to DT",
                             hort_classification == "drought tolerant" & traits_classification == "drought intolerant" ~ "DT to DI",
                             hort_classification == "drought tolerant" & traits_classification == "mixture" ~ "DT to mixture"))

# OsmPot

TLP <- ggplot(drought_tolerant, aes(x = Changes, y = mean_TLP, fill = Changes)) +
  geom_boxplot(alpha=0.5, outlier.shape=NA) +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07", "#E7B800")) +
  labs(title="",x="", y = "TLP (MPa)") +
  theme_bw() +
  theme(legend.position = "none") # remove legend

TLP

# LMA

LMA <- ggplot(drought_tolerant, aes(x = Changes, y = mean_LMA_gm2, fill = Changes)) +
  geom_boxplot(alpha=0.5, outlier.shape=NA) +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07", "#E7B800")) +
  labs(title="",x="", y = expression("LMA" ~ (g/m^{2}))) +
  theme_bw() +
  theme(legend.position = "none") # remove legend

LMA

# Thickness

Thickness <- ggplot(drought_tolerant, aes(x = Changes, y = Thickness_mm_Avg, fill = Changes)) +
  geom_boxplot(alpha=0.5, outlier.shape=NA)+
  scale_fill_manual(values = c("#00AFBB", "#FC4E07", "#E7B800")) +
  labs(title="",x="", y = "Thickness (mm)") +
  theme_bw() +
  theme(legend.position = "none") # remove legend

Thickness

# Leaf area
Leaf_area <- ggplot(drought_tolerant, aes(x = Changes, y = mean_leaf_area_cm2, fill = Changes)) +
  geom_boxplot(alpha=0.5, outlier.shape=NA)+
  scale_fill_manual(values = c("#00AFBB", "#FC4E07", "#E7B800")) +
  labs(title="",x="", y = expression("Leaf area" ~ (cm^{2}))) +
  theme_bw() +
  theme(legend.position = "none") # remove legend

Leaf_area

# LDMC

LDMC <- ggplot(drought_tolerant, aes(x = Changes, y = mean_LDMC, fill = Changes)) +
  geom_boxplot(alpha=0.5, outlier.shape=NA)+
  scale_fill_manual(values = c("#00AFBB", "#FC4E07", "#E7B800")) +
  labs(title="",x="", y = "LDMC") +
  theme_bw() +
  theme(legend.position = "none") # remove legend

LDMC

# join plots together

library(gridExtra)

grid.arrange(TLP, LMA, Thickness, Leaf_area, LDMC, nrow = 3, ncol = 2)
# saved plot manually, for some reason code wasn't working

### filter just for the mixture hort classification (there are only 7 hort drought intolerant species so didn't do)
mixture <- filter(everything, hort_classification == "mixture")

# need to create a new column for the changes in classification if there are any

mixture <- mixture %>%
  mutate(Changes = case_when(hort_classification == "mixture" & traits_classification == "drought tolerant" ~ "mixture to DT",
                             hort_classification == "mixture" & traits_classification == "drought intolerant" ~ "mixture to DI",
                             hort_classification == "mixture" & traits_classification == "mixture" ~ "mixture to mixture"))

# TLP

TLP <- ggplot(mixture, aes(x = Changes, y = mean_TLP, fill = Changes)) +
  geom_boxplot(alpha=0.5, outlier.shape=NA) +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07", "#E7B800")) +
  labs(title="",x="", y = "TLP (MPa)") +
  theme_bw() +
  theme(legend.position = "none") # remove legend

TLP

# LMA

LMA <- ggplot(mixture, aes(x = Changes, y = mean_LMA_gm2, fill = Changes)) +
  geom_boxplot(alpha=0.5, outlier.shape=NA) +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07", "#E7B800")) +
  labs(title="",x="", y = expression("LMA" ~ (g/m^{2}))) +
  theme_bw() +
  theme(legend.position = "none") # remove legend

LMA

# Thickness

Thickness <- ggplot(mixture, aes(x = Changes, y = Thickness_mm_Avg, fill = Changes)) +
  geom_boxplot(alpha=0.5, outlier.shape=NA)+
  scale_fill_manual(values = c("#00AFBB", "#FC4E07", "#E7B800")) +
  labs(title="",x="", y = "Thickness (mm)") +
  theme_bw() +
  theme(legend.position = "none") # remove legend

Thickness

# Leaf area

Leaf_area <- ggplot(mixture, aes(x = Changes, y = mean_leaf_area_cm2, fill = Changes)) +
  geom_boxplot(alpha=0.5, outlier.shape=NA)+
  scale_fill_manual(values = c("#00AFBB", "#FC4E07", "#E7B800")) +
  labs(title="",x="", y = expression("Leaf area" ~ (cm^{2}))) +
  theme_bw() +
  theme(legend.position = "none") # remove legend

Leaf_area

# LDMC

LDMC <- ggplot(mixture, aes(x = Changes, y = mean_LDMC, fill = Changes)) +
  geom_boxplot(alpha=0.5, outlier.shape=NA)+
  scale_fill_manual(values = c("#00AFBB", "#FC4E07", "#E7B800")) +
  labs(title="",x="", y = "LDMC") +
  theme_bw() +
  theme(legend.position = "none") # remove legend

LDMC

# join plots together

library(gridExtra)

grid.arrange(TLP, LMA, Thickness, Leaf_area, LDMC, nrow = 3, ncol = 2)
# saved plot manually, for some reason code wasn't working

### Let's look at changes in life forms

# load data

everything <- read.csv("Cluster_traits_analysis_data/traits_and_cluster.csv")

# filter out all the species with NAs
# https://stackoverflow.com/questions/26665319/removing-na-in-dplyr-pipe

everything <- drop_na(everything) # ended up with 90 species

# need to change classification into long format
everything <- gather(everything, key="method", value="classification", -Species_Code, -Origin, -Growth_Form, -Growth_structure,
                     -mean_OsmPot, -mean_LMA_gm2, -Thickness_mm_Avg, -mean_succulance_g, -mean_leaf_area_cm2, -mean_LDMC,
                     -mean_Density_kgm3)

stacked_all <- everything %>%
  group_by(method, classification, Growth_Form) %>%
  summarise(frequency = n())

# stacked barplot of life forms for each classification

graph <- ggplot(stacked_all, aes(x = classification, y = frequency, fill = Growth_Form)) +
                geom_bar(position = "stack", stat = "identity") +
                facet_wrap(~ method)

graph

## look at just the drought tolerant species

# load data

everything <- read.csv("Cluster_traits_analysis_data/traits_and_cluster.csv")

# filter out all the species with NAs
# https://stackoverflow.com/questions/26665319/removing-na-in-dplyr-pipe

everything <- drop_na(everything) # ended up with 90 species

### filter just for the drought tolerant hort classification
drought_tolerant <- filter(everything, hort_classification == "drought tolerant")

# need to create a new column for the changes in classification if there are any

drought_tolerant <- drought_tolerant %>%
  mutate(Changes = case_when(hort_classification == "drought tolerant" & traits_classification == "drought tolerant" ~ "DT to DT",
                             hort_classification == "drought tolerant" & traits_classification == "drought intolerant" ~ "DT to DI",
                             hort_classification == "drought tolerant" & traits_classification == "mixture" ~ "DT to mixture"))

drought_summary <- drought_tolerant %>%
  group_by(Growth_Form, Changes) %>%
  summarise(frequency = n())

graph2 <- ggplot(drought_summary, aes(x = Changes, y = frequency, fill = Growth_Form)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Horticultural drougth tolerant species")

graph2

### filter just for the mixture hort classification (there are only 7 hort drought intolerant species so didn't do)
mixture <- filter(everything, hort_classification == "mixture")

# need to create a new column for the changes in classification if there are any

mixture <- mixture %>%
  mutate(Changes = case_when(hort_classification == "mixture" & traits_classification == "drought tolerant" ~ "mixture to DT",
                             hort_classification == "mixture" & traits_classification == "drought intolerant" ~ "mixture to DI",
                             hort_classification == "mixture" & traits_classification == "mixture" ~ "mixture to mixture"))

mixture_summary <- mixture %>%
  group_by(Growth_Form, Changes) %>%
  summarise(frequency = n())

graph3 <- ggplot(mixture_summary, aes(x = Changes, y = frequency, fill = Growth_Form)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Horticultural mixture species")

graph3

# join plots together

library(gridExtra)

grid.arrange(graph, graph2, graph3, nrow = 3, ncol = 1)
# saved plot manually, for some reason code wasn't working

############################################################################################################################################

#                                                 Relationship between TLP, LDMC and density

############################################################################################################################################

library(tidyverse)
library(ggplot2)

# load data

everything <- read.csv("Cluster_traits_analysis_data/traits_and_cluster.csv")

# let's look at LDMC
# plot all the points
all_together <- ggplot(everything, aes(x = mean_TLP, y = mean_LDMC)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = "TLP (MPa)", y = "LDMC (g/g)") +
  annotate("text", x = -0.8, y = 0.6, label = "r2 = 0.38", size = 5) +
  theme_bw()

all_together

# group points by woody/non woody

growth_structure <- ggplot(everything, aes(x = mean_TLP, y = mean_LDMC, shape = Growth_structure, color = Growth_structure)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = "TLP (MPa)", y = "LDMC (g/g)") +
  theme_bw()

growth_structure

# let's subset for just the trees and shrubs

treesshrubs <- filter(everything, Growth_Form == "Tree" | Growth_Form == "Shrub")

treeshrubplot <- ggplot(treesshrubs, aes(x = mean_TLP, y = mean_LDMC, shape = Growth_Form, color = Growth_Form)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = "TLP (MPa)", y = "LDMC (g/g)") +
  theme_bw()

treeshrubplot

# let's look at density
# plot all the points

density <- ggplot(everything, aes(x = mean_TLP, y = mean_Density_kgm3)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = "TLP (MPa)", y = "Density (LMA/thickness) (kg/m3)") +
  annotate("text", x = -0.8, y = 0.6, label = "r2 = 0.31", size = 5) +
  theme_bw()

density

# group points by woody/non woody

growth_structure <- ggplot(everything, aes(x = mean_TLP, y = mean_Density_kgm3, shape = Growth_structure, color = Growth_structure)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = "TLP (MPa)", y = "Density (LMA/thickness) (kg/m3)") +
  theme_bw()

growth_structure

# just trees and shrubs

treesshrubs <- filter(everything, Growth_Form == "Tree" | Growth_Form == "Shrub")

treeshrubplot <- ggplot(treesshrubs, aes(x = mean_TLP, y = mean_Density_kgm3, shape = Growth_Form, color = Growth_Form)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = "TLP (MPa)", y = "Density (LMA/thickness) (kg/m3)") +
  theme_bw()

treeshrubplot

# plot density and LDMC

densityandthickness <- ggplot(everything, aes(x = mean_Density_kgm3, y = mean_LDMC)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = "Density (LMA/thickness) (kg/m3)", y = "LDMC (g/g)") +
  annotate("text", x = 100, y = 0.6, label = "r2 = 0.54", size = 5) +
  theme_bw()

densityandthickness
