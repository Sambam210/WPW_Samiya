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
