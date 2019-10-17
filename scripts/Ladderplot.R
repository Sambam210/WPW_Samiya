install.packages("plotrix")
library(plotrix)
library(dplyr)


# script from example
x<-data.frame(A=c(1:10), B=c(2:11)+rnorm(10))
y<-data.frame(x, C=c(1:10)+rnorm(10))
opar <- par(mfrow=c(1,3))
ladderplot(x)
ladderplot(x, col=1:10, vertical=FALSE)
ladderplot(y, col=1:10)
par(opar)

# let's read in the data for PCA traits only and PCA traits and AI cluster analysis
# these csv files contain a column with the cluster that each species is in

traits <- read.csv("PCA_Cluster_output/PCA_traits_cluster_output.csv")
traitsAI <- read.csv("PCA_Cluster_output/PCA_traitsAI_cluster_output.csv")

# need column name for species

traits <- rename(traits, Species=X)
traitsAI <- rename(traitsAI, Species=X)

# need to remove Phro and Kebe from the traits csv as they are not in the traitsAi csv (didn't have enviro data)
traits <- filter(traits, Species != "Phro", Species != "Kebe")

# let's extract the cluster columns from each dataset

traits_cluster <- traits %>%
  select(Species, clust) %>%
  rename(traits=clust)

traitsAI_cluster <- traitsAI %>%
  select(Species, clust) %>%
  rename(traitsAI=clust)

# let's join these together
all.data <- left_join(traits_cluster, traitsAI_cluster, by = "Species")

# remove species column
all.data <- select(all.data, -Species)

# ladderplot
ladderplot(all.data, method = "jitter") # doesn't work

ladderplot(all.data, col=1:4)

# change the numbers into categorical values
# https://stackoverflow.com/questions/19503266/replace-all-particular-values-in-a-data-frame

all.data[all.data == "1"] <- "tolerators"
all.data[all.data == "2"] <- "mixture"
all.data[all.data == "3"] <- "avoiders"
all.data[all.data == "4"] <- "succulent tolerators"

# ladderplot
ladderplot(all.data) # doesn't work

###########################################################################################################################

# let's try the ggplot way
# https://stackoverflow.com/questions/52138418/how-to-make-ladder-plot

library(dplyr)
library(tidyverse)

# let's read in the data for PCA traits only and PCA traits and AI cluster analysis
# these csv files contain a column with the cluster that each species is in

traits <- read.csv("PCA_Cluster_output/PCA_traits_cluster_output.csv")
traitsAI <- read.csv("PCA_Cluster_output/PCA_traitsAI_cluster_output.csv")

# need column name for species

traits <- rename(traits, Species=X)
traitsAI <- rename(traitsAI, Species=X)

# need to remove Phro and Kebe from the traits csv as they are not in the traitsAi csv (didn't have enviro data)
traits <- filter(traits, Species != "Phro", Species != "Kebe")

# let's extract the cluster columns from each dataset

traits_cluster <- traits %>%
  select(Species, clust) %>%
  rename(traits=clust)

traitsAI_cluster <- traitsAI %>%
  select(Species, clust) %>%
  rename(traitsAI=clust)

# let's join these together
all.data <- left_join(traits_cluster, traitsAI_cluster, by = "Species")

write.csv(all.data, "Ladderplot_data_input/traitsandtraitsAI.csv", row.names = FALSE)

# species for which the clusters have changed
filter(all.data, traits != traitsAI)

# filter out those species
all.data.same <- filter(all.data, Species != "Anco", Species != "Coau", Species != "Limu", Species != "Meaf", Species != "Pyca")

# create a random column to jitter the points
all.data.same <- all.data.same %>%
  mutate(random = runif(103, -0.1, 0.1) + traits)

# replace the traits and traits AI values with the random values                        
all.data.same$traits <- all.data.same$random
all.data.same$traitsAI <- all.data.same$random

# remove the ramdom column
all.data.same <- select(all.data.same, -random)

# species for which the clusters have changed
all.data.diff <- filter(all.data, traits != traitsAI)

# create a random column for traits to jitter the points
all.data.diff <- all.data.diff %>%
  mutate(randomtraits = runif(5, -0.1, 0.1) + traits)


# create a random column for traitsAI to jitter the points
all.data.diff <- all.data.diff %>%
  mutate(randomtraitsAI = runif(5, -0.1, 0.1) + traitsAI)

# replace the traits and traits AI values with the random values                        
all.data.diff$traits <- all.data.diff$randomtraits
all.data.diff$traitsAI <- all.data.diff$randomtraitsAI

# remove the random column
all.data.diff <- select(all.data.diff, -randomtraits, -randomtraitsAI)

# join everything together
all.data.jitter <- rbind(all.data.same, all.data.diff)

# gather so there is one column for model and cluster
# https://www.rdocumentation.org/packages/tidyr/versions/0.8.3/topics/gather
all.data.jitter <- gather(all.data.jitter, key="model", value="cluster", -Species)

library(ggplot2)

ggplot(all.data.jitter, aes(x=model, y=cluster, group=Species)) +
  geom_line() +
  scale_y_continuous(labels=c("1" = "tolerators", "2" = "mixture",
                            "3" = "avoiders", "4" = "succulent tolerators")) +
  theme_bw()

#########################################################################################################################################

## ggalluvial vignette
# https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html

all.data <- read.csv("Ladderplot_data_input/traitsandtraitsAI.csv")

library(tidyverse)

#transform data into 'wide' format
all.data.wide <- gather(all.data, key="model", value="cluster", -Species)

install.packages("ggalluvial")
library(ggalluvial)
library(ggplot2)

# vignette code

data(vaccinations)

levels(vaccinations$response) <- rev(levels(vaccinations$response))

ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           y = freq,
           fill = response, label = response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none") +
  ggtitle("vaccination survey responses at three points in time")

# let's try on my data
# first need a 'classification' column

all.data.wide <- all.data.wide %>%
  mutate(classification = case_when(cluster == "1" ~ "tolerators",
                                    cluster == "2" ~ "mixture",
                                    cluster == "3" ~ "avoiders",
                                    cluster == "4" ~ "succulent tolerators"))


traitsandtraitsAIplot <- ggplot(all.data.wide,
                      aes(x = model, stratum = classification, alluvium = Species,
                      fill = classification, label = classification)) +
                      scale_x_discrete(expand = c(.1, .1)) +
                      geom_flow() +
                      geom_stratum(alpha = .5) +
                      geom_text(stat = "stratum", size = 3) +
                      theme(legend.position = "none") +
                      labs(y = "Number of species")

traitsandtraitsAIplot

# save the output
pdf("Ladderplot_output/traitsandtraitsAI.pdf") # Create a new pdf device
print(traitsandtraitsAIplot)
dev.off() # Close the pdf device

###############################################################################################################

# Loading in my horticultural classification data

hort.class <- read.csv("Ladderplot_data_input/gh_drought_summary_Samiya.csv")

library(tidyverse)

# get rid of source and line info

hort.class <- hort.class[,1:7]

# filter out the species with less than 4 sources

hort.class <- filter(hort.class, total >= 4) # this gives us 92/113 species

# create percentage columns for each of the columns 'no, 'moderate' and 'yes'

hort.class <- hort.class %>%
  mutate(No_percentage = No/total*100,
         Moderate_percentage = Moderate/total*100,
         Yes_percentage = Yes/total*100)

# assign 'yes', 'no' or 'mixture' classifications 
# Protocol: 
# If >=60% of the records is either 'yes', 'no' or 'medium' then that species has that corresponding classification
# If 'medium' is >=60% then that species is said to be 'yes' for drought tolerance
# If no classification is >=60% then that species is 'mixture'

hort.class <- hort.class %>%
  mutate(hort_classification = case_when(No_percentage >= 60 ~ "drought intolerant",
                                         Moderate_percentage >= 60 ~ "drought tolerant",
                                         Yes_percentage >= 60 ~ "drought tolerant",
                                         TRUE ~ "mixture")) %>%
  select(Species_Code, hort_classification)

# load my traits cluster data

traits <- read.csv("PCA_Cluster_output/PCA_traits_cluster_output.csv")

traits <- traits %>%
  rename(Species_Code=X) %>%
  mutate(traits_classification = case_when(clust == "1" ~ "drought tolerant",
                                    clust == "2" ~ "mixture",
                                    clust == "3" ~ "drought intolerant",
                                    clust == "4" ~ "drought tolerant")) %>%
  select(Species_Code, traits_classification)

# merge the two datasets together

hortandtraits <- left_join(hort.class, traits, by = "Species_Code")

# remove rows with NAs
# https://stackoverflow.com/questions/26665319/removing-na-in-dplyr-pipe

hortandtraits <- drop_na(hortandtraits) # ended up with 90 species 

write.csv(hortandtraits, "Ladderplot_data_input/hortandtraits.csv", row.names = FALSE)

# let's look at some summaries of what has changed
summary <- hortandtraits %>%
  select(-Species_Code) %>%
  group_by(hort_classification, traits_classification) %>%
  add_count() %>%
  distinct(hort_classification, traits_classification, .keep_all = TRUE)

write.csv(summary, "Ladderplot_output/hortandtraits_summary.csv", row.names = FALSE)
  
#############################
# Constructing the alluvial plot for hort and traits classification

hortandtraits <- read.csv("Ladderplot_data_input/hortandtraits.csv")

#transform data into 'wide' format
hortandtraits.wide <- gather(hortandtraits, key="method", value="classification", -Species_Code)

# constructing the alluvial plot

library(ggalluvial)
library(ggplot2)

hortandtraitsplot <- ggplot(hortandtraits.wide,
                       aes(x = method, stratum = classification, alluvium = Species_Code,
                       fill = classification, label = classification)) +
                       scale_x_discrete(expand = c(.1, .1)) +
                       geom_flow() +
                       geom_stratum(alpha = .5) +
                       geom_text(stat = "stratum", size = 3) +
                       annotate("text", x = 1, y = 82, label = "(12.2%)", size = 3) +
                       annotate("text", x = 1, y = 46, label = "(67.8%)", size = 3) +
                       annotate("text", x = 1, y = 6.8, label = "(20%)", size = 3) +
                       annotate("text", x = 2, y = 71, label = "(36.7%)", size = 3) +
                       annotate("text", x = 2, y = 38.5, label = "(35.5%)", size = 3) +
                       annotate("text", x = 2, y = 10, label = "(27.8%)", size = 3) +
                       theme(legend.position = "none") +
                       labs(y = "Number of species",
                            x = "Method")

hortandtraitsplot

# save the output
pdf("Ladderplot_output/hortandtraits.pdf") # Create a new pdf device
print(hortandtraitsplot)
dev.off() # Close the pdf device



