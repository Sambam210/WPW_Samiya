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

################################################

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

#######################################################

## ggalluvial vignette
# https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html

all.data <- read.csv("Ladderplot_data_input/traitsandtraitsAI.csv")

library(tidyverse)

#transform data into 'wide' format
all.data.long <- gather(all.data, key="model", value="cluster", -Species)

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

all.data.long <- all.data.long %>%
  mutate(classification = case_when(cluster == "1" ~ "tolerators",
                                    cluster == "2" ~ "mixture",
                                    cluster == "3" ~ "avoiders",
                                    cluster == "4" ~ "succulent tolerators"))


traitsandstraitsAIplot <- ggplot(all.data.long,
                      aes(x = model, stratum = classification, alluvium = Species,
                      fill = classification, label = classification)) +
                      scale_x_discrete(expand = c(.1, .1)) +
                      geom_flow() +
                      geom_stratum(alpha = .5) +
                      geom_text(stat = "stratum", size = 3) +
                      theme(legend.position = "none") +
                      labs(y = "Number of species")

# save the output
pdf("Ladderplot_output/traitsandtraitsAI.pdf") # Create a new pdf device
print(traitsandstraitsAIplot)
dev.off() # Close the pdf device










