# This prelim analysis is based on ground assessmnet of tree health conducted by Anthony and I on 29,30,31 Jan and 4 Feb 2020 around Penrith following
# a recent spate of heatwaves. Trees were visiually assessed with the following comments:

# healthy
# lightly scorched
# heavily scorched
# defolitated

# Trees that were removed haved also been documented

library(tidyverse)

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

# save output

write.csv(all, "Western_Sydney_Heatwave_output/cleaned_data.csv", row.names = FALSE)

# NOTE: I have manually cleaned up some errors in taxonomy

############################################################################################################################################


#########################################################################################################################################

#                                                       GRAPHS

##########################################################################################################################################

library(tidyverse)
library(ggplot2)

data <- read.csv("Western_Sydney_Heatwave_output/cleaned_data.csv")

# let's pull out the species which we have >= 20 records for

records <- data %>%
  group_by(Species) %>%
  summarise(frequency = n()) %>%
  filter(frequency > 20 | frequency == 20) # 40 species with >= 20 records

# let's pull these species out of the master database

records_data <- left_join(records, data, by = "Species")

# let's look at how many plants were in each score category from those 40 species

damaged <- records_data %>%
  group_by(Species, Score, frequency) %>%
  summarise(partial_frequency = n()) %>%
  mutate(percent = (partial_frequency/frequency)*100)

# change the names for score categories as per Michelle and Ale's suggestions

damaged[] <-lapply(damaged, gsub, pattern = "healthy", replacement = "no damage")
damaged[] <-lapply(damaged, gsub, pattern = "lightly scorched", replacement = "lightly damaged")
damaged[] <-lapply(damaged, gsub, pattern = "heavily scorched", replacement = "heavily damaged")

damaged$Score <- factor(damaged$Score, levels = c("no damage", "lightly damaged", "heavily damaged", "defoliated"))

# for some reason numbers have been changed to characters, change back

damaged$frequency <- as.numeric(as.character(damaged$frequency))
damaged$partial_frequency <- as.numeric(as.character(damaged$partial_frequency))
damaged$percent <- as.numeric(as.character(damaged$percent))

glimpse(damaged)

order <- damaged %>%
  filter(Score == "no damage") %>%
  arrange(percent) %>%
  rowid_to_column(var='order') %>%
  select(order, Species)

damaged_new <- left_join(damaged, order, by = "Species")

damaged_new <- select(damaged_new, -Score.y)

damaged_new <- rename(damaged_new, Score = Score.x)

# combine species and frequency into new column
# https://stackoverflow.com/questions/39098406/add-brackets-to-string-in-data-frame
# https://stackoverflow.com/questions/18115550/combine-two-or-more-columns-in-a-dataframe-into-a-new-column-with-a-new-name

damaged_new$frequency_new <- paste0("(", damaged_new$frequency , ")")

damaged_new$Species_new <- paste(damaged_new$Species, (damaged_new$frequency_new))

# put an asterisk in from of the exotic species
# https://stackoverflow.com/questions/39098406/add-brackets-to-string-in-data-frame

origin <- read.csv("Western_Sydney_Heatwave_output/40_species.csv")

origin <- select(origin, Species, Origin)

damaged_new <- left_join(damaged_new, origin, by = "Species")

damaged_new$Species_new_new <- with(damaged_new, ifelse(Origin == "Exotic", paste0("* ", Species_new),
                                                        paste0(Species_new)))

# grey colours
# https://ggplot2.tidyverse.org/reference/scale_grey.html

graph <- ggplot(damaged_new, aes(x = reorder(Species_new_new, desc(order)), y = percent, fill = Score)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_grey(start = 0.8, end = 0.2) +
  labs(x = "Species", y = "Percentage") +
  coord_flip() +
  geom_hline(yintercept = 25, linetype="solid", color = "white", size=0.5) +
  geom_hline(yintercept = 50, linetype="solid", color = "white", size=0.5) +
  geom_hline(yintercept = 75, linetype="solid", color = "white", size=0.5) +
  theme_bw()

graph

# in colour
graph <- ggplot(damaged_new, aes(x = reorder(Species_new_new, desc(order)), y = percent, fill = Score)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("#66CC00", "#FFFF00", "#FF6600", "#000000")) +
  labs(x = "Species", y = "Percentage") +
  coord_flip() +
  geom_hline(yintercept = 25, linetype="solid", color = "white", size=0.5) +
  geom_hline(yintercept = 50, linetype="solid", color = "white", size=0.5) +
  geom_hline(yintercept = 75, linetype="solid", color = "white", size=0.5) +
  theme_bw()

graph

# https://community.rstudio.com/t/r-ggplot2-reorder-stacked-plot/23912/2

# exoitc vs native species damage

origin <- read.csv("Western_Sydney_Heatwave_output/40_species.csv")

# join origin to damage datasheet

damage_origin <- left_join(damaged, origin, by = "Species")

# graph

damage_origin$Score <- factor(damage_origin$Score, levels = c("healthy", "lightly scorched", "heavily scorched", "defoliated"))

# native species

native <- filter(damage_origin, Origin == "Native")

order <- native %>%
  filter(Score == "healthy") %>%
  arrange(percent) %>%
  rowid_to_column(var='order') %>%
  select(order, Species)

native_new <- left_join(native, order, by = "Species")

native_new <- select(native_new, -Score.y)

native_new <- rename(native_new, Score = Score.x)

native_origin <- ggplot(native_new, aes(x = reorder(Species, desc(order)), y = percent, fill = Score)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_grey() +
  labs(title = "Native species", x = "Species", y = "Percentage") +
  coord_flip() +
  geom_hline(yintercept = 25, linetype="solid", color = "white", size=0.5) +
  geom_hline(yintercept = 50, linetype="solid", color = "white", size=0.5) +
  geom_hline(yintercept = 75, linetype="solid", color = "white", size=0.5) +
  theme_bw()

native_origin

# exotic species

exotic <- filter(damage_origin, Origin == "Exotic")

order <- exotic %>%
  filter(Score == "healthy") %>%
  arrange(percent) %>%
  rowid_to_column(var='order') %>%
  select(order, Species)

exotic_new <- left_join(exotic, order, by = "Species")

exotic_new <- select(exotic_new, -Score.y)

exotic_new <- rename(exotic_new, Score = Score.x)

exotic_origin <- ggplot(exotic_new, aes(x = reorder(Species, desc(order)), y = percent, fill = Score)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_grey() +
  labs(title = "Exotic species", x = "Species", y = "Percentage") +
  coord_flip() +
  geom_hline(yintercept = 25, linetype="solid", color = "white", size=0.5) +
  geom_hline(yintercept = 50, linetype="solid", color = "white", size=0.5) +
  geom_hline(yintercept = 75, linetype="solid", color = "white", size=0.5) +
  theme_bw()

exotic_origin

# deciduous vs evergreen species damage

leafloss <- read.csv("Western_Sydney_Heatwave_output/40_species.csv")

# join origin to damage datasheet

damage_leafloss <- left_join(damaged, origin, by = "Species")

# graph

damage_leafloss$Score <- factor(damage_leafloss$Score, levels = c("healthy", "lightly scorched", "heavily scorched", "defoliated"))

# evergreen species

evergreen <- filter(damage_leafloss, Leaf_loss == "Evergreen")

order <- evergreen %>%
  filter(Score == "healthy") %>%
  arrange(percent) %>%
  rowid_to_column(var='order') %>%
  select(order, Species)

evergreen_new <- left_join(evergreen, order, by = "Species")

evergreen_new <- select(evergreen_new, -Score.y)

evergreen_new <- rename(evergreen_new, Score = Score.x)

evergreen_leafloss <- ggplot(evergreen_new, aes(x = reorder(Species, desc(order)), y = percent, fill = Score)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_grey() +
  labs(title = "Evergreen species", x = "Species", y = "Percentage") +
  coord_flip() +
  geom_hline(yintercept = 25, linetype="solid", color = "white", size=0.5) +
  geom_hline(yintercept = 50, linetype="solid", color = "white", size=0.5) +
  geom_hline(yintercept = 75, linetype="solid", color = "white", size=0.5) +
  theme_bw()

evergreen_leafloss

# deciduous species

deciduous <- filter(damage_leafloss, Leaf_loss == "Deciduous")

order <- deciduous %>%
  filter(Score == "healthy") %>%
  arrange(percent) %>%
  rowid_to_column(var='order') %>%
  select(order, Species)

deciduous_new <- left_join(deciduous, order, by = "Species")

deciduous_new <- select(deciduous_new, -Score.y)

deciduous_new <- rename(deciduous_new, Score = Score.x)

deciduous_leafloss <- ggplot(deciduous_new, aes(x = reorder(Species, desc(order)), y = percent, fill = Score)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_grey() +
  labs(title = "Deciduous species", x = "Species", y = "Percentage") +
  coord_flip() +
  geom_hline(yintercept = 25, linetype="solid", color = "white", size=0.5) +
  geom_hline(yintercept = 50, linetype="solid", color = "white", size=0.5) +
  geom_hline(yintercept = 75, linetype="solid", color = "white", size=0.5) +
  theme_bw()

deciduous_leafloss

############################ let's look at tree dimensions

## HEIGHT

# healthy

healthy_height <- data %>%
  add_count(Height, sort = FALSE, name = "height_total") %>%
  filter(Score == "healthy") %>%
  group_by(Height, height_total) %>%
  summarise(frequency = n()) %>%
  mutate(percent = (frequency/height_total)*100) %>%
  filter(Height != "0")

healthy_height <- ggplot(healthy_height, aes(x = Height, y = percent)) +
  geom_bar(stat = "identity") +
  labs(title = "Healthy", x = "Height (m)", y = "Percentage")

healthy_height

# lightly scorched

lightly_scorched_height <- data %>%
  add_count(Height, sort = FALSE, name = "height_total") %>%
  filter(Score == "lightly scorched") %>%
  group_by(Height, height_total) %>%
  summarise(frequency = n()) %>%
  mutate(percent = (frequency/height_total)*100)

lightly_scorched_height <- ggplot(lightly_scorched_height, aes(x = Height, y = percent)) +
  geom_bar(stat = "identity") +
  labs(title = "Lightly scorched", x = "Height (m)", y = "Percentage")

lightly_scorched_height

# heavily scorched

heavily_scorched_height <- data %>%
  add_count(Height, sort = FALSE, name = "height_total") %>%
  filter(Score == "heavily scorched") %>%
  group_by(Height, height_total) %>%
  summarise(frequency = n()) %>%
  mutate(percent = (frequency/height_total)*100)

heavily_scorched_height <- ggplot(heavily_scorched_height, aes(x = Height, y = percent)) +
  geom_bar(stat = "identity") +
  labs(title = "Heavily scorched", x = "Height (m)", y = "Percentage")

heavily_scorched_height

# defoliated

defoliated_height <- data %>%
  add_count(Height, sort = FALSE, name = "height_total") %>%
  filter(Score == "defoliated") %>%
  group_by(Height, height_total) %>%
  summarise(frequency = n()) %>%
  mutate(percent = (frequency/height_total)*100)

defoliated_height <- ggplot(defoliated_height, aes(x = Height, y = percent)) +
  geom_bar(stat = "identity") +
  labs(title = "Defoliated", x = "Height (m)", y = "Percentage")

defoliated_height

# join plots together

library(gridExtra)

grid.arrange(healthy_height, lightly_scorched_height, heavily_scorched_height, defoliated_height, nrow = 2, ncol = 2)
# saved plot manually, for some reason code wasn't working

## WIDTH

# healthy

healthy_width <- data %>%
  add_count(Width, sort = FALSE, name = "width_total") %>%
  filter(Score == "healthy") %>%
  group_by(Width, width_total) %>%
  summarise(frequency = n()) %>%
  mutate(percent = (frequency/width_total)*100) %>%
  filter(Width != "0")

healthy_width <- ggplot(healthy_width, aes(x = Width, y = percent)) +
  geom_bar(stat = "identity") +
  labs(title = "Healthy", x = "Width (m)", y = "Percentage")

healthy_width

# lightly scorched

lightly_scorched_width <- data %>%
  add_count(Width, sort = FALSE, name = "width_total") %>%
  filter(Score == "lightly scorched") %>%
  group_by(Width, width_total) %>%
  summarise(frequency = n()) %>%
  mutate(percent = (frequency/width_total)*100)

lightly_scorched_width <- ggplot(lightly_scorched_width, aes(x = Width, y = percent)) +
  geom_bar(stat = "identity") +
  labs(title = "Lightly scorched", x = "Width (m)", y = "Percentage")

lightly_scorched_width

# heavily scorched

heavily_scorched_width <- data %>%
  add_count(Width, sort = FALSE, name = "width_total") %>%
  filter(Score == "heavily scorched") %>%
  group_by(Width, width_total) %>%
  summarise(frequency = n()) %>%
  mutate(percent = (frequency/width_total)*100)

heavily_scorched_width <- ggplot(heavily_scorched_width, aes(x = Width, y = percent)) +
  geom_bar(stat = "identity") +
  labs(title = "Heavily scorched", x = "Width (m)", y = "Percentage")

heavily_scorched_width

# defoliated

defoliated_width <- data %>%
  add_count(Width, sort = FALSE, name = "width_total") %>%
  filter(Score == "defoliated") %>%
  group_by(Width, width_total) %>%
  summarise(frequency = n()) %>%
  mutate(percent = (frequency/width_total)*100)

defoliated_width <- ggplot(defoliated_width, aes(x = Width, y = percent)) +
  geom_bar(stat = "identity") +
  labs(title = "Defoliated", x = "Width (m)", y = "Percentage")

defoliated_width

# join plots together

library(gridExtra)

grid.arrange(healthy_width, lightly_scorched_width, heavily_scorched_width, defoliated_width, nrow = 2, ncol = 2)
# saved plot manually, for some reason code wasn't working

## HEIGHT X WIDTH

# healthy

healthy_height_width <- data %>%
  mutate(height_x_width = Height * Width) %>%
  add_count(height_x_width, sort = FALSE, name = "height_x_width_total") %>%
  filter(Score == "healthy") %>%
  group_by(height_x_width, height_x_width_total) %>%
  summarise(frequency = n()) %>%
  mutate(percent = (frequency/height_x_width_total)*100) %>%
  filter(height_x_width != "0")

healthy_height_width <- ggplot(healthy_height_width, aes(x = height_x_width, y = percent)) +
  geom_bar(stat = "identity") +
  labs(title = "Healthy", x = expression("Height x Width" ~ (m^{2})), y = "Percentage")

healthy_height_width

# lightly scorched

lightly_scorched_height_width <- data %>%
  mutate(height_x_width = Height * Width) %>%
  add_count(height_x_width, sort = FALSE, name = "height_x_width_total") %>%
  filter(Score == "lightly scorched") %>%
  group_by(height_x_width, height_x_width_total) %>%
  summarise(frequency = n()) %>%
  mutate(percent = (frequency/height_x_width_total)*100)

lightly_scorched_height_width <- ggplot(lightly_scorched_height_width, aes(x = height_x_width, y = percent)) +
  geom_bar(stat = "identity") +
  labs(title = "Lightly scorched", x = expression("Height x Width" ~ (m^{2})), y = "Percentage")

lightly_scorched_height_width

# heavily scorched

heavily_scorched_height_width <- data %>%
  mutate(height_x_width = Height * Width) %>%
  add_count(height_x_width, sort = FALSE, name = "height_x_width_total") %>%
  filter(Score == "heavily scorched") %>%
  group_by(height_x_width, height_x_width_total) %>%
  summarise(frequency = n()) %>%
  mutate(percent = (frequency/height_x_width_total)*100)

heavily_scorched_height_width <- ggplot(heavily_scorched_height_width, aes(x = height_x_width, y = percent)) +
  geom_bar(stat = "identity") +
  labs(title = "Heavily scorched", x = expression("Height x Width" ~ (m^{2})), y = "Percentage")

heavily_scorched_height_width

# defoliated

defoliated_height_width <- data %>%
  mutate(height_x_width = Height * Width) %>%
  add_count(height_x_width, sort = FALSE, name = "height_x_width_total") %>%
  filter(Score == "defoliated") %>%
  group_by(height_x_width, height_x_width_total) %>%
  summarise(frequency = n()) %>%
  mutate(percent = (frequency/height_x_width_total)*100)

defoliated_height_width <- ggplot(defoliated_height_width, aes(x = height_x_width, y = percent)) +
  geom_bar(stat = "identity") +
  labs(title = "Defoliated", x = expression("Height x Width" ~ (m^{2})), y = "Percentage")

defoliated_height_width

# join plots together

library(gridExtra)

grid.arrange(healthy_height_width, lightly_scorched_height_width, heavily_scorched_height_width, defoliated_height_width, nrow = 2, ncol = 2)
# saved plot manually, for some reason code wasn't working

# let's pull out the species with =< 50% healthy trees

damaged_50 <- damaged %>%
  filter(Score == "healthy") %>%
  filter(percent < 50 | percent == 50) # only 4 species (Acer, Magnolia, Platanus, Quercus)

# let's pull these species out of the master database

records_50 <- filter(data, Species == "Acer negundo" | Species == "Magnolia grandiflora" | Species == "Platanus x acerifolia" 
                     | Species == "Quercus palustris")

# group acording to score and height

summary_records_50 <- records_50 %>%
  select(Species, Score, Height) %>%
  add_count(Species, Height, name = "Height_total") %>%
  group_by(Species, Score, Height, Height_total) %>%
  summarise(frequency = n()) %>%
  mutate(percent = (frequency/Height_total)*100)

# graph it

summary_records_50$Score <- factor(summary_records_50$Score, levels = c("healthy", "lightly scorched", "heavily scorched", "defoliated"))

# most_damaged <- ggplot(summary_records_50, aes(x = Height, y = percent, fill = Score)) +
#  geom_bar(position = "stack", stat = "identity") +
#  scale_fill_grey(start = 0.8, end = 0.2) +
#  labs(title = "Most damaged species", x = "Height (m)", y = "Percentage") +
#  theme(axis.text.x = element_text(angle=90, hjust=1), # rotate x axis labels
#        panel.background = element_rect(fill = "white"),
#        panel.border = element_rect(fill = NA, linetype = "solid", colour = "black")) +
#  facet_wrap(~Species)

most_damaged <- ggplot(summary_records_50, aes(x = Height, y = percent, fill = Score)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_grey(start = 0.8, end = 0.2) +
  labs(title = "Most damaged species", x = "Height (m)", y = "Percentage") +
  theme_bw() +
  facet_wrap(~Species)

most_damaged

# save the output
pdf("Western_Sydney_Heatwave_output/summary_plots.pdf") # Create a new pdf device
print(graph)
print(native_origin)
print(exotic_origin)
print(evergreen_leafloss)
print(deciduous_leafloss)
print(most_damaged)
dev.off() # Close the pdf device

##########################################################################################################################################

###### Create summary tables with damage, height, width and frequency to work out cost of replacement

library(tidyverse)

data <- read.csv("Western_Sydney_Heatwave_output/cleaned_data.csv")

# just pull out defoliated and heavily scorched

all_damaged <- data %>%
  filter(Score == "defoliated" | Score == "heavily scorched") %>%
  select(Species, Height, Score) %>%
  group_by(Species, Height, Score) %>%
  summarise(frequency = n()) %>%
  select(Species, Score, Height, frequency) %>%
  arrange(Score, Species)

write_csv(all_damaged, "Western_Sydney_Heatwave_output/damage_height.csv")

##########################################################################################################################################

# for each species summarise the percentage of trees in each damage category

data <- read.csv("Western_Sydney_Heatwave_output/cleaned_data.csv")

# let's pull out the species which we have >= 20 records for

records <- data %>%
  group_by(Species) %>%
  summarise(frequency = n()) %>%
  filter(frequency > 20 | frequency == 20) # 40 species with >= 20 records

# let's pull these species out of the master database

records_data <- left_join(records, data, by = "Species")

damage_summary <- records_data %>%
  select(Species, Score) %>%
  add_count(Species, name = "species_frequency") %>%
  group_by(Species, Score, species_frequency) %>%
  summarise(score_frequency = n()) %>%
  mutate(percent = (score_frequency/species_frequency)*100)

write_csv(damage_summary, "Western_Sydney_Heatwave_output/damage_summary.csv")

##########################################################################################################################################

#### PCA on assessed trees and climatic variables

niches <- read.csv("Western_Sydney_Heatwave_data/niches_damage_Penrith.csv")

# only look at the means of the climatic variables that I think are important

niches_mean <- select(niches, speciesName, damaged, Annual_mean_temp_mean, Mean_temp_dry_qu_mean, Max_temp_warm_month_mean)

library(tidyverse)
library("FactoMineR")
library("factoextra")

# removed the categorical species columns

PCA_analysis_pairs <- select(niches_mean, -speciesName)

# let's see if the data are linearly related

pairs(PCA_analysis_pairs)

# looks ok

# make species code the row name

PCA_analysis <- niches_mean %>%
  remove_rownames %>%
  column_to_rownames(var="speciesName")

# doing the PCA

res.pca <- PCA(PCA_analysis, graph = FALSE)

# looking at the eigenvalues

eig.val <- get_eigenvalue(res.pca)
eig.val
# first 3 PCs explain 97% of the data

# let's look at the scree plot

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 60))

# let's look at the variables

var <- get_pca_var(res.pca)
var

# plot the variables

var.plot <- fviz_pca_var(res.pca, col.var = "black")
var.plot

# graph of individuals

ind <- get_pca_ind(res.pca)
ind

# plot the individuals

ind.plot <- fviz_pca_ind(res.pca, repel = TRUE)
ind.plot

# Hierarchical clustering

# let's only do the PCA on the first 3 dimensions

res.pca <- PCA(PCA_analysis, ncp = 3, graph = FALSE)

# clustering
res.hcpc <- HCPC(res.pca, max = 3, graph = FALSE)

# dendrogram
fviz_dend(res.hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8)     # Augment the room for labels

# graph
cluster <- fviz_cluster(res.hcpc,
                        repel = TRUE,            # Avoid label overlapping
                        show.clust.cent = TRUE, # Show cluster centers
                        palette = "jco",         # Color palette see ?ggpubr::ggpar
                        ggtheme = theme_minimal(),
                        main = "PCA")
cluster

# Let's look at the HCPC output

# display the qualtitative variables that explain the most variance in each cluster

res.hcpc$desc.var$quanti

# principle dimensions that are most associated with clusters
res.hcpc$desc.axes$quanti

#####################################

## remove Zelkova and Gleditsia

niches <- read.csv("Western_Sydney_Heatwave_data/niches_damage_Penrith.csv")

# only look at the means of the climatic variables that I think are important

niches_mean <- select(niches, speciesName, damaged, Annual_mean_temp_mean, Mean_temp_dry_qu_mean, Max_temp_warm_month_mean)

niches_mean <- filter(niches_mean, speciesName != "Gleditsia triacanthos", speciesName != "Zelkova serrata")

library(tidyverse)
library("FactoMineR")
library("factoextra")

# removed the categorical species columns

PCA_analysis_pairs <- select(niches_mean, -speciesName)

# let's see if the data are linearly related

pairs(PCA_analysis_pairs)

# looks ok

# make species code the row name

PCA_analysis <- niches_mean %>%
  remove_rownames %>%
  column_to_rownames(var="speciesName")

# doing the PCA

res.pca <- PCA(PCA_analysis, graph = FALSE)

# looking at the eigenvalues

eig.val <- get_eigenvalue(res.pca)
eig.val
# first 3 PCs explain 97% of the data

# let's look at the scree plot

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 60))

# let's look at the variables

var <- get_pca_var(res.pca)
var

# plot the variables

var.plot <- fviz_pca_var(res.pca, col.var = "black")
var.plot

# graph of individuals

ind <- get_pca_ind(res.pca)
ind

# plot the individuals

ind.plot <- fviz_pca_ind(res.pca, repel = TRUE)
ind.plot

# Hierarchical clustering

# let's only do the PCA on the first 3 dimensions

res.pca <- PCA(PCA_analysis, ncp = 3, graph = FALSE)

# clustering
res.hcpc <- HCPC(res.pca, max = 3, graph = FALSE)

# dendrogram
fviz_dend(res.hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8)     # Augment the room for labels

# graph
cluster <- fviz_cluster(res.hcpc,
                        repel = TRUE,            # Avoid label overlapping
                        show.clust.cent = TRUE, # Show cluster centers
                        palette = "jco",         # Color palette see ?ggpubr::ggpar
                        ggtheme = theme_minimal(),
                        main = "PCA")
cluster

# Let's look at the HCPC output

# display the qualtitative variables that explain the most variance in each cluster

res.hcpc$desc.var$quanti

# principle dimensions that are most associated with clusters
res.hcpc$desc.axes$quanti

#############################################################################################################

# only do PCA on enviro variables and colour code individuals by damage


niches <- read.csv("Western_Sydney_Heatwave_data/niches_damage_Penrith.csv")

# only look at the means of the climatic variables that I think are important

niches_mean <- select(niches, speciesName, Annual_mean_temp_mean, Mean_temp_dry_qu_mean, Max_temp_warm_month_mean, Range_Ann_Tmean, Range_Tmax_warmM)

library(tidyverse)
library("FactoMineR")
library("factoextra")

# removed the categorical species columns

PCA_analysis_pairs <- select(niches_mean, -speciesName)

# let's see if the data are linearly related

pairs(PCA_analysis_pairs)

# looks ok

# make species code the row name

PCA_analysis <- niches_mean %>%
  remove_rownames %>%
  column_to_rownames(var="speciesName")

# doing the PCA

res.pca <- PCA(PCA_analysis, graph = FALSE)

# looking at the eigenvalues

eig.val <- get_eigenvalue(res.pca)
eig.val
# first 2 PCs explain 97% of the data

# let's look at the scree plot

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 60))

# let's look at the variables

var <- get_pca_var(res.pca)
var

# plot the variables

var.plot <- fviz_pca_var(res.pca, col.var = "black")
var.plot

# graph of individuals

ind <- get_pca_ind(res.pca)
ind

# plot the individuals

ind.plot <- fviz_pca_ind(res.pca, repel = TRUE)
ind.plot

# create the biplot by colouring individuals according to damage
plot <- fviz_pca_biplot(res.pca, 
                        col.ind = niches$damaged, # colour individuals by damage
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                        col.var = "black",
                        repel = TRUE,
                        geom = c("point", "text"),       
                        ellipse.alpha = 0.2, # transparency of ellipses   
                        pointsize = 2, # size of points
                        mean.point = FALSE, # don't show group centers
                        legend.title = "", # no legend title
                        ggtheme = theme_gray())

plot

#######################################################################################################################################

###### ORDINAL REGRESSION

# https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/

library(tidyverse)

data <- read.csv("Western_Sydney_Heatwave_output/cleaned_data.csv")

# let's pull out the species which we have >= 20 records for

records <- data %>%
  group_by(Species) %>%
  summarise(frequency = n()) %>%
  filter(frequency > 20 | frequency == 20) # 40 species with >= 20 records

# let's pull these species out of the master database

records_data <- left_join(records, data, by = "Species")

# only select the variables we are interested in

records_data <- select(records_data, Species, Score)

# change the names for score categories as per Michelle and Ale's suggestions

records_data[] <-lapply(records_data, gsub, pattern = "healthy", replacement = "no damage")
records_data[] <-lapply(records_data, gsub, pattern = "lightly scorched", replacement = "lightly damaged")
records_data[] <-lapply(records_data, gsub, pattern = "heavily scorched", replacement = "heavily damaged")

records_data$Score <- factor(records_data$Score, levels = c("no damage", "lightly damaged", "heavily damaged", "defoliated"))

# add origin and leaf loss info

origin <- read.csv("Western_Sydney_Heatwave_output/40_species.csv")

origin <- select(origin, -frequency)

records_data <- left_join(records_data, origin, by = "Species")

# create a new binary variable for origin
records_data <- records_data %>%
  mutate(origin_binary = case_when(Origin == "Exotic" ~ "1",
                                 Origin == "Native" ~ "0"))

records_data$origin_binary <- as.numeric(as.character(records_data$origin_binary))

# create a new binary variable for leaf loss
records_data <- records_data %>%
  mutate(leaf_loss_binary = case_when(Leaf_loss == "Deciduous" ~ "1",
                                   Leaf_loss == "Evergreen" ~ "0"))

records_data$leaf_loss_binary <- as.numeric(as.character(records_data$leaf_loss_binary))

glimpse(records_data)

# doing the ordinal regression

install.packages("MASS")
library("MASS")

model <- polr(Score ~ origin_binary + leaf_loss_binary, data = records_data, Hess = TRUE)

summary(model)

# calculating the odds ratio

exp(cbind(OR = coef(model), confint(model)))

# interpretation

# https://stats.idre.ucla.edu/r/faq/ologit-coefficients/
# http://onbiostatistics.blogspot.com/2012/02/how-to-interpret-odds-ratios-that-are.html

# If you are an exotic species, the odds of being damaged (lightly damaged, heavily damaged, defolitated) versus undamaged is 3.4
# times that of native species

# If you are a deciduous species, the odds of being damaged (lightly damaged, heavily damaged, defolitated) versus undamaged is 3
# times that of evergreen species









