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



# let's try the ggplot way
# https://stackoverflow.com/questions/52138418/how-to-make-ladder-plot

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

# gather so there is one column for model and cluster
# https://www.rdocumentation.org/packages/tidyr/versions/0.8.3/topics/gather
all.data.new <- gather(all.data, key="model", value="cluster", -Species)



library(ggplot2)

ggplot(all.data.new, aes(x=model, y=cluster, group=Species)) +
  geom_line() +
  theme_bw()









