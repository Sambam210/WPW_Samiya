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

# Let's do some summary stats and graphs

library(tidyverse)
library(ggplot2)

data <- read.csv("Western_Sydney_Heatwave_output/cleaned_data.csv")

summary_scores <- data %>%
  group_by(Score) %>%
  summarise(frequency=n()) %>%
  mutate(completeness = (frequency / 5591))

write.csv(summary_scores, "Western_Sydney_Heatwave_output/summary_scores.csv", row.names = FALSE)

# let's look at just the damaged species

damaged <- data %>%
  filter(Score != "healthy") %>%
  group_by(Species, Score) %>%
  summarise(frequency = n())

graph <- ggplot(damaged, aes(x = reorder(Species, frequency), Species, y = frequency, fill = Score)) + # https://stackoverflow.com/questions/37480949/re-ordering-bars-in-rs-barplot/37481077
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("#CC6633", "#FF3399", "#FF9933", "#FF9900", "#FFFF66", "#0033FF")) +
  labs(title = "Damaged trees", x = "Species", y = "Frequency") +
  coord_flip()

graph

# let's look at the damaged species IN RELATION to how many of them are healthy

damaged2 <- data %>%
  filter(Score != "healthy") %>%
  group_by(Species, Score) %>%
  summarise(frequency = n()) %>%
  distinct(Species)

damaged_healthy <- left_join(damaged2, data, by = "Species")

damaged_healthy <- damaged_healthy %>%
  group_by(Species, Score) %>%
  summarise(frequency = n())

damaged_healthy <- ggplot(damaged_healthy, aes(x = reorder(Species, frequency), Species, y = frequency, fill = Score)) + # https://stackoverflow.com/questions/37480949/re-ordering-bars-in-rs-barplot/37481077
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Damaged trees", x = "Species", y = "Frequency") +
  coord_flip()

damaged_healthy

# let's just look at lightly scorched species

lightly_scorched <- damaged %>%
  filter(Score == "lightly scorched")

lightly_scorched <- ggplot(lightly_scorched, aes(x = reorder(Species, frequency), Species, y = frequency)) +
  geom_bar(stat = "identity") +
  labs(title = "Lightly scorched", x = "Species", y = "Frequency") +
  coord_flip()

lightly_scorched

# let's just look at heavily scorched species

heavily_scorched <- damaged %>%
  filter(Score == "heavily scorched")

heavily_scorched <- ggplot(heavily_scorched, aes(x = reorder(Species, frequency), Species, y = frequency)) +
  geom_bar(stat = "identity") +
  labs(title = "Heavily scorched", x = "Species", y = "Frequency") +
  coord_flip()

heavily_scorched

# let's just look at defoliated species

defoliated <- damaged %>%
  filter(Score == "defoliated")

defoliated <- ggplot(defoliated, aes(x = reorder(Species, frequency), Species, y = frequency)) +
  geom_bar(stat = "identity") +
  labs(title = "Defoliated", x = "Species", y = "Frequency") +
  coord_flip()

defoliated
  
# save the output
pdf("Western_Sydney_Heatwave_output/summary_graphs.pdf") # Create a new pdf device
print(graph)
print(damaged_healthy)
print(lightly_scorched)
print(heavily_scorched)
print(defoliated)
dev.off() # Close the pdf device

############################ let's look at tree dimensions
# healthy

healthy_height <- data %>%
  filter(Score == "healthy") %>%
  group_by(Height) %>%
  summarise(frequency = n())

healthy_height <- ggplot(healthy_height, aes(x = Height, y = frequency)) +
  geom_bar(stat = "identity") +
  labs(title = "Healthy", x = "Height (m)", y = "Frequency")

healthy_height

# lightly scorched

lightly_scorched_height <- data %>%
  filter(Score == "lightly scorched") %>%
  group_by(Height) %>%
  summarise(frequency = n())

lightly_scorched_height <- ggplot(lightly_scorched_height, aes(x = Height, y = frequency)) +
  geom_bar(stat = "identity") +
  labs(title = "Lightly scorched", x = "Height (m)", y = "Frequency")

lightly_scorched_height

# heavily scorched

heavily_scorched_height <- data %>%
  filter(Score == "heavily scorched") %>%
  group_by(Height) %>%
  summarise(frequency = n())

heavily_scorched_height <- ggplot(heavily_scorched_height, aes(x = Height, y = frequency)) +
  geom_bar(stat = "identity") +
  labs(title = "Heavily scorched", x = "Height (m)", y = "Frequency")

heavily_scorched_height

# defoliated

defoliated_height <- data %>%
  filter(Score == "defoliated") %>%
  group_by(Height) %>%
  summarise(frequency = n())

defoliated_height <- ggplot(defoliated_height, aes(x = Height, y = frequency)) +
  geom_bar(stat = "identity") +
  labs(title = "Defoliated", x = "Height (m)", y = "Frequency")

defoliated_height

# join plots together

library(gridExtra)

grid.arrange(healthy_height, lightly_scorched_height, heavily_scorched_height, defoliated_height, nrow = 2, ncol = 2)
# saved plot manually, for some reason code wasn't working

#########################################################################################################################################

#                                                       LET'S DO THIS BETTER

# Report percentages instead

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

damaged$Score <- factor(damaged$Score, levels = c("healthy", "lightly scorched", "heavily scorched", "defoliated"))

graph <- ggplot(damaged, aes(x = Species, y = percent, fill = Score)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("#339900", "#FFFF00", "#FF9933", "#FF0000")) +
  labs(title = "Trees >= 20 records", x = "Species", y = "Percentage") +
  coord_flip()

graph

# https://community.rstudio.com/t/creating-a-barplot-with-ordered-bars/13681/3
# https://community.rstudio.com/t/a-tidy-way-to-order-stacked-bar-chart-by-fill-subset/5134

# exoitc vs native species damage

origin <- read.csv("Western_Sydney_Heatwave_output/40_species.csv")

# join origin to damage datasheet

damage_origin <- left_join(damaged, origin, by = "Species")

# graph

damage_origin$Score <- factor(damage_origin$Score, levels = c("healthy", "lightly scorched", "heavily scorched", "defoliated"))

# native species

native <- filter(damage_origin, Origin == "Native")

native_origin <- ggplot(native, aes(x = Species, y = percent, fill = Score)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("#339900", "#FFFF00", "#FF9933", "#FF0000")) +
  labs(title = "Native species", x = "Species", y = "Percentage") +
  coord_flip()

native_origin

# exotic species

exotic <- filter(damage_origin, Origin == "Exotic")

exotic_origin <- ggplot(exotic, aes(x = Species, y = percent, fill = Score)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("#339900", "#FFFF00", "#FF9933", "#FF0000")) +
  labs(title = "Exotic species", x = "Species", y = "Percentage") +
  coord_flip()

exotic_origin

# deciduous vs evergreen species damage

leafloss <- read.csv("Western_Sydney_Heatwave_output/40_species.csv")

# join origin to damage datasheet

damage_leafloss <- left_join(damaged, origin, by = "Species")

# graph

damage_leafloss$Score <- factor(damage_leafloss$Score, levels = c("healthy", "lightly scorched", "heavily scorched", "defoliated"))

# evergreen species

evergreen <- filter(damage_leafloss, Leaf_loss == "Evergreen")

evergreen_leafloss <- ggplot(evergreen, aes(x = Species, y = percent, fill = Score)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("#339900", "#FFFF00", "#FF9933", "#FF0000")) +
  labs(title = "Evergreen species", x = "Species", y = "Percentage") +
  coord_flip()

evergreen_leafloss

# deciduous species

deciduous <- filter(damage_leafloss, Leaf_loss == "Deciduous")

deciduous_leafloss <- ggplot(deciduous, aes(x = Species, y = percent, fill = Score)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("#339900", "#FFFF00", "#FF9933", "#FF0000")) +
  labs(title = "Deciduous species", x = "Species", y = "Percentage") +
  coord_flip()

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

# let's pull out the species with =< 50% healthy trees

damaged_50 <- damaged %>%
  filter(Score == "healthy") %>%
  filter(percent < 50 | percent == 50) # only 4 species (Acer, Magnolia, Plantanus, Quercus)

# let's pull these species out of the master database

records_50 <- filter(data, Species == "Acer negundo" | Species == "Magnolia grandiflora" | Species == "Plantanus x acerifolia" 
                     | Species == "Quercus palustris")

# let's group them according to height and score

summary_records_50 <- records_50 %>%
  group_by(Species, Score, Height) %>%
  summarise(frequency = n())

# add in the total frequency column back

summary_records_50 <- summary_records_50 %>%
  mutate(total_frequency = case_when(Species == "Acer negundo" ~ "74",
                                     Species == "Magnolia grandiflora" ~ "55",
                                     Species == "Plantanus x acerifolia" ~ "55",
                                     Species == "Quercus palustris" ~ "57"))

glimpse(summary_records_50)

# need to change total frequency into integar

summary_records_50$total_frequency <- as.numeric(as.character(summary_records_50$total_frequency))

glimpse(summary_records_50)

# calculate percentage

summary_records_50 <- mutate(summary_records_50, percent = (frequency/total_frequency)*100)

# graph it

summary_records_50$Score <- factor(summary_records_50$Score, levels = c("healthy", "lightly scorched", "heavily scorched", "defoliated"))

most_damaged <- ggplot(summary_records_50, aes(x = Height, y = percent)) +
  geom_bar(stat = "identity") +
  labs(title = "Most damaged species", x = "Height (m)", y = "Percentage") +
  facet_grid(Species ~ Score)

most_damaged

# save the output
pdf("Western_Sydney_Heatwave_output/summary_plots.pdf") # Create a new pdf device
print(graph)
print(native_origin)
print(exotic_origin)
print(most_damaged)
dev.off() # Close the pdf device









