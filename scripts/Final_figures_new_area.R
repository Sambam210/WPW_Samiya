
library(tidyverse)
library("FactoMineR")
library("factoextra")

other.variables <- read.csv("PCA_Cluster_data/PCA_data.csv")

# Renee unloaded individual leaf areas in the LMA file - I will use these instead of the ones I calculated
# means I'm not missing any species anymore!
area <- read.csv("PCA_Cluster_data/WPW_MOD3_GH_LMA_20171101-20190418_L3.csv")

# average these leaf areas by species
area <- area %>%
  filter(Treatment == "C") %>%
  group_by(Species_Code) %>%
  summarise(leaf_area = mean(Leaf_Area_cm2, na.rm = TRUE)) %>%
  select(Species_Code, leaf_area)

# join to other datasheet
other.variables <- left_join(other.variables, area, by = "Species_Code")
  
other.variables <- select(other.variables, Species, Species_Code, mean_LMA_gm2, Thickness_mm_Avg, leaf_area)

# load the LMA data, Renee said everything else I was using (LMA, weights, etc) was clean
LMA <- read.csv("GH_data/WPW_GH_LMA_clean.csv")

# transform fresh and dry weights into LDMC
LDMC <- LMA %>%
  filter(Treatment == "C") %>%
  select(Species, Species_Code, Fresh_Weight_g, Dry_Weight_g) %>%
  mutate(LDMC = Dry_Weight_g/Fresh_Weight_g) %>%
  group_by(Species_Code) %>%
  summarise(mean_LDMC = mean(LDMC, na.rm = TRUE))

# join to master spreadsheet
PCA_analysis <- left_join(other.variables, LDMC, by = "Species_Code")

# load the osmpot data
osmpot <- read.csv("GH_data/WPW_GH_OSMPOT.csv")

# transform osmpot into tlp
osmpot <- osmpot %>%
  filter(Treatment == "C") %>%
  select(Species_Code, OsmPot_MPa) %>%
  mutate(TLP = (0.832 * OsmPot_MPa) - 0.631) %>%
  group_by(Species_Code) %>%
  summarise(mean_TLP = mean(TLP, na.rm = TRUE))

# join to master spreadsheet
PCA_analysis <- left_join(PCA_analysis, osmpot, by = "Species_Code")

## let's see if the data are linearly related

# removed the categorical species columns

PCA_analysis_pairs <- select(PCA_analysis, -Species, -Species_Code)

# let's see if the data are linearly related

pairs(PCA_analysis_pairs)

# might need to transform the thickness and area variables

PCA_analysis_pairs <- PCA_analysis_pairs %>%
  mutate(log_Thickness_mm_Avg = log(Thickness_mm_Avg),
         log_leaf_area_cm2 = log(leaf_area)) %>%
  select(-Thickness_mm_Avg, -leaf_area)

pairs(PCA_analysis_pairs) # looks better

# transform in the main spreadsheet

PCA_analysis <- PCA_analysis %>%
  mutate(log_Thickness_mm_Avg = log(Thickness_mm_Avg),
         log_leaf_area_cm2 = log(leaf_area)) %>%
  select(-Thickness_mm_Avg, -leaf_area, -Species)

# rename the columns so they look better in the biplot

PCA_analysis <- PCA_analysis %>%
  rename(LMA = mean_LMA_gm2,
         LDMC = mean_LDMC,
         TLP = mean_TLP,
         Thickness = log_Thickness_mm_Avg,
         Area = log_leaf_area_cm2)

# make species code the row name

PCA_analysis <- PCA_analysis %>%
  remove_rownames %>%
  column_to_rownames(var="Species_Code")

# doing the PCA

res.pca <- PCA(PCA_analysis, graph = FALSE)

# looking at the eigenvalues

eig.val <- get_eigenvalue(res.pca)
eig.val
# first 3 PCs explain 89% of the data

# let's look at the scree plot

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 60))

# let's look at the variables

var <- get_pca_var(res.pca)
var

# plot the variables

var.plot <- fviz_pca_var(res.pca, col.var = "black")
var.plot

# colour variables on the correlation circle by their contrib value -> contribution of the variable on the factor map

fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), # specify custom gradient
             repel = TRUE) # avoid text overlap

# Contributions of variables to PC1

fviz_contrib(res.pca, choice = "var", axes = 1, top = 4) # red dashed line is the expected average contribution (i.e. 4 variables = 25%)
# LDMC and OsmPot

# Contributions of variables to PC2

fviz_contrib(res.pca, choice = "var", axes = 2, top = 4)
# Thickness and LMA

# dimension description
# identify the most significanlty associated variables for a given PC

res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1 # LDMC and osmpot
# Description of dimension 2
res.desc$Dim.2 # thickness and LMA

# graph of individuals

ind <- get_pca_ind(res.pca)
ind

# plot the individuals

ind.plot <- fviz_pca_ind(res.pca, repel = TRUE)
ind.plot

# colour by groups

# confidence ellipses
# shows 95% confidence ellipse, see ?coord.ellipse()

fviz_pca_ind(res.pca,
             geom.ind = "point", # show points only (but not "text")
             col.ind = PCA_analysis$Growth_Form, # color by growth form
             palette = c("Dark2"),
             addEllipses = TRUE, ellipse.type = "confidence", # confidence ellipses
             legend.title = "Growth form") # grasses look sign different from everything else

# Hierarchical clustering

# let's only do the PCA on the first 3 dimensions

res.pca <- PCA(PCA_analysis, ncp = 3, graph = FALSE)

# clustering
res.hcpc <- HCPC(res.pca, graph = FALSE) 

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

# display the original data with a new column indicating which cluster they belong to

head(res.hcpc$data.clust)
# save this output for ladderplot
PCA_traits_cluster_output <- res.hcpc$data.clust
write.csv(PCA_traits_cluster_output,"PCA_Cluster_output/PCA_traits_cluster_output_LDMC_new_area.csv", row.names = TRUE) # need row names for species

# display the qualtitative variables that explain the most variance in each cluster

res.hcpc$desc.var$quanti

# principle dimensions that are most associated with clusters
res.hcpc$desc.axes$quanti

# save the output
pdf("PCA_Cluster_output/glasshouse_no_climate_LDMC_TLP_newarea.pdf") # Create a new pdf device
print(var.plot)
print(ind.plot)
print(cluster)
dev.off() # Close the pdf device

##### let's make a biplot with the clusters

# load the csv with the clusters
cluster <- read.csv("PCA_Cluster_output/PCA_traits_cluster_output_LDMC_new_area.csv")

# create a new column with the clusters rearranged so they are easy to describe
cluster <- cluster %>%
  mutate(cluster_new = case_when(clust == "1" ~ "Drought avoider",
                                 clust == "2" ~ "Intermediate",
                                 clust == "3" ~ "Drought tolerator"))

# create the biplot but group according to cluster
plot <- fviz_pca_biplot(res.pca, 
                        col.ind = cluster$cluster_new, # colour individuals by cluster
                        palette = c("#00AFBB","#FC4E07","#E7B800"),
                        addEllipses = TRUE, ellipse.type = "convex", # use convex ellipses like cluster analysis
                        col.var = "black",
                        repel = TRUE,
                        geom = "point", # just want the points, no writing        
                        ellipse.alpha = 0.2, # transparency of ellipses   
                        pointsize = 2, # size of points
                        title = "Fig. 1", # no main title
                        mean.point = FALSE, # don't show group centers
                        legend.title = "", # no legend title
                        ggtheme = theme_gray())

plot

# plot for growth form (following reviewer's comments)

plot <- fviz_pca_biplot(res.pca, 
                        col.ind = cluster$Growth_Form, # colour individuals by growth form
                        pointshape = 19, # makes everything circles
                        col.var = "black",
                        repel = TRUE,
                        geom = "point", # just want the points, no writing        
                        ellipse.alpha = 0.2, # transparency of ellipses   
                        pointsize = 2, # size of points
                        title = "Fig. 1", # no main title
                        mean.point = FALSE, # don't show group centers
                        legend.title = "", # no legend title
                        ggtheme = theme_gray())


plot

# https://stackoverflow.com/questions/61156095/r-pca-with-the-fviz-pca-ind-function-can-we-have-two-categorical-variables-o

# save the plot

dev.print(pdf, 'plot.pdf')
# https://stackoverflow.com/questions/7144118/how-to-save-a-plot-as-image-on-the-disk

##########################################################################################################################################
####                                                          LADDERPLOT                                                          ####
##########################################################################################################################################

#################################### HORT CLASSIFICATION VS TRAITS CLASSIFICATION W/ LDMC INSTEAD OF SUCCULENCE
########## 75% concensus

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
# If >=75% of the records is either 'yes', 'no' or 'medium' then that species has that corresponding classification
# If 'medium' is >=75% then that species is said to be 'yes' for drought tolerance
# If no classification is >=75% then that species is 'mixture'

hort.class <- hort.class %>%
  mutate(hort_classification = case_when(No_percentage >= 75 ~ "drought intolerant",
                                         Moderate_percentage >= 75 ~ "drought tolerant",
                                         Yes_percentage >= 75 ~ "drought tolerant",
                                         TRUE ~ "intermediate")) %>%
  select(Species_Code, hort_classification)

# load my traits cluster data

traits <- read.csv("PCA_Cluster_output/PCA_traits_cluster_output_LDMC_new_area.csv")

traits <- traits %>%
  rename(Species_Code=X) %>%
  mutate(traits_classification = case_when(clust == "1" ~ "drought avoider",
                                           clust == "2" ~ "intermediate",
                                           clust == "3" ~ "drought tolerator")) %>%
  select(Species_Code, traits_classification)

# merge the two datasets together

hortandtraits <- left_join(hort.class, traits, by = "Species_Code")

# remove rows with NAs
# https://stackoverflow.com/questions/26665319/removing-na-in-dplyr-pipe

hortandtraits <- drop_na(hortandtraits) # ended up with 92 species 
write.csv(hortandtraits,"Ladderplot_data_input/hortandtraits_LDMC_75%concensus_new_area.csv", row.names = FALSE)

#############################
# Constructing the alluvial plot for hort and traits classification

#transform data into 'wide' format
hortandtraits.wide <- gather(hortandtraits, key="method", value="classification", -Species_Code)

# changing the names for the variables so the graphs look better
# https://stackoverflow.com/questions/29271549/replace-all-occurrences-of-a-string-in-a-data-frame
hortandtraits.wide[] <-lapply(hortandtraits.wide, gsub, pattern = "_", replacement = " ")
hortandtraits.wide[] <-lapply(hortandtraits.wide, gsub, pattern = "hort", replacement = "horticultural")

# need to rearrange the levels
# hortandtraits.wide[,'classification'] <- as.factor(hortandtraits.wide[,'classification'])
# hortandtraits.wide[,'classification'] <- factor(hortandtraits.wide[,'classification'], levels = c("drought intolerant", "drought avoider", "intermediate","drought tolerant","drought tolerator"))

# constructing the alluvial plot

library(ggalluvial)
library(ggplot2)

hortandtraitsplot <- ggplot(hortandtraits.wide,
                            aes(x = method, stratum = classification, alluvium = Species_Code,
                                fill = classification, label = classification)) +
  scale_x_discrete(expand = c(0.1,0.1)) +
  scale_fill_manual(values = c("#CC6633", "#FF3399", "#FF9933", "#FF9900", "#FFFF66")) +
  geom_flow(stat = "alluvium", lode.guidance = "frontback", color = "darkgray", alpha = 0.5) +
  geom_stratum(alpha = .5, fill = "white") +
  geom_text(stat = "stratum", size = 4) +
  theme(legend.position = "none") +
  labs(y = "Number of species",
       x = "Method",
       title = "Fig. 2") +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12))

hortandtraitsplot

# scale fill manual values
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/ 

# save the plot

dev.print(pdf, 'plot.pdf')
# https://stackoverflow.com/questions/7144118/how-to-save-a-plot-as-image-on-the-disk

#####################################################################################################################################

# PCA for climate precip and AI variables to be included as supplemental material

library(tidyverse)
library("FactoMineR")
library("factoextra")

other.variables <- read.csv("PCA_Cluster_data/PCA_data.csv")

hugh.data <- read.csv("MFA_data/niche.data.HB.csv")

hugh.data <- hugh.data %>%
  select(searchTaxon, Annual_precip_mean, Precip_dry_qu_mean, Precip_dry_month_mean, AI_mean) %>% # selecting the relevant variables
  rename(Species=searchTaxon)

all.data <- left_join(other.variables, hugh.data, by = "Species")

all.data <- filter(all.data, Species_Code != "Phro", Species_Code != "Kebe") # filter out species with missing climate data

all.data <- select(all.data, Species_Code, Annual_precip_mean, Precip_dry_qu_mean, Precip_dry_month_mean, AI_mean) # select only the climate variables

## let's see if the data are linearly related

# removed the categorical species columns

PCA_analysis_pairs <- select(all.data, -Species_Code)

# let's see if the data are linearly related

pairs(PCA_analysis_pairs) # looks good, everything is linearly related

# make species code the row name

PCA_analysis <- all.data %>%
  remove_rownames %>%
  column_to_rownames(var="Species_Code")

# rename the columns so they look better in the biplot

PCA_analysis <- PCA_analysis %>%
  rename('Annual precipitation' = Annual_precip_mean,
         'Aridity index' = AI_mean,
         'Precipitation driest quarter' = Precip_dry_qu_mean,
         'Precipitation driest month' = Precip_dry_month_mean)

# doing the PCA

res.pca <- PCA(PCA_analysis, graph = FALSE)

# looking at the eigenvalues

eig.val <- get_eigenvalue(res.pca)
eig.val
# first 3 PCs explain 99% of the data

# let's look at the scree plot

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 60))

# let's look at the variables

var <- get_pca_var(res.pca)
var

# plot the variables

var.plot <- fviz_pca_var(res.pca, col.var = "black")
var.plot

# Contributions of variables to PC1

fviz_contrib(res.pca, choice = "var", axes = 1, top = 4) # red dashed line is the expected average contribution (i.e. 4 variables = 25%)
# aridity index, precip driest quarter, precip driest month

# Contributions of variables to PC2

fviz_contrib(res.pca, choice = "var", axes = 2, top = 4)
# annual precip

# dimension description
# identify the most significanlty associated variables for a given PC

res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1
# Description of dimension 2
res.desc$Dim.2

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
res.hcpc <- HCPC(res.pca, graph = FALSE)

# graph
cluster <- fviz_cluster(res.hcpc,
                        repel = TRUE,            # Avoid label overlapping
                        show.clust.cent = TRUE, # Show cluster centers
                        palette = "jco",         # Color palette see ?ggpubr::ggpar
                        ggtheme = theme_minimal(),
                        main = "PCA")
cluster

# Let's look at the HCPC output

# display the original data with a new column indicating which cluster they belong to

head(res.hcpc$data.clust)
# save this output for ladderplot
PCA_climate_cluster_output <- res.hcpc$data.clust
write.csv(PCA_climate_cluster_output,"PCA_Cluster_output/PCA_climate_cluster_output.csv", row.names = TRUE) # need row names for species

# display the qualtitative variables that explain the most variance in each cluster

res.hcpc$desc.var$quanti

# principle dimensions that are most associated with clusters
res.hcpc$desc.axes$quanti

##### let's make a biplot with the clusters

# load the csv with the clusters
cluster <- read.csv("PCA_Cluster_output/PCA_climate_cluster_output.csv")

# create a new column with the clusters rearranged so they are easy to describe
cluster <- cluster %>%
  mutate(cluster_new = case_when(clust == "1" ~ "Dry",
                                 clust == "2" ~ "Moderate",
                                 clust == "3" ~ "Wet"))

# create the biplot but group according to cluster
plot <- fviz_pca_biplot(res.pca, 
                        col.ind = cluster$cluster_new, # colour individuals by cluster
                        palette = c("#FC4E07","#E7B800","#00AFBB"),
                        addEllipses = TRUE, ellipse.type = "convex", # use convex ellipses like cluster analysis
                        col.var = "black",
                        repel = TRUE,
                        geom = "point", # just want the points, no writing        
                        ellipse.alpha = 0.2, # transparency of ellipses   
                        pointsize = 2, # size of points
                        title = "(a)", # no main title
                        mean.point = FALSE, # don't show group centers
                        legend.title = "", # no legend title
                        ggtheme = theme_gray())

plot

# save the plot

dev.print(pdf, 'plot.pdf')
# https://stackoverflow.com/questions/7144118/how-to-save-a-plot-as-image-on-the-disk

############################################################################################################################################
##### Ladderplots for the supplementary material
############################################################################################################################################

#############################
# Constructing the alluvial plot for traits and climate classification

traitsandclimate <- read.csv("Ladderplot_data_input/traitsandclimate_newarea.csv")

#transform data into 'wide' format
traitsandclimate.wide <- gather(traitsandclimate, key="method", value="classification", -Species_Code)

# changing the names for the variables so the graphs look better
# https://stackoverflow.com/questions/29271549/replace-all-occurrences-of-a-string-in-a-data-frame
traitsandclimate.wide[] <-lapply(traitsandclimate.wide, gsub, pattern = "_", replacement = " ")
traitsandclimate.wide[] <-lapply(traitsandclimate.wide, gsub, pattern = "hort", replacement = "horticultural")

# need to rearrange the levels so that climate_classification doesn't come first in the plot
traitsandclimate.wide[,'method'] <- as.factor(traitsandclimate.wide[,'method'])
traitsandclimate.wide[,'method'] <- factor(traitsandclimate.wide[,'method'], levels = c("traits classification", "climate classification"))

# constructing the alluvial plot

library(ggalluvial)
library(ggplot2)

traitsandclimateplot <- ggplot(traitsandclimate.wide,
                               aes(x = method, stratum = classification, alluvium = Species_Code,
                                   fill = classification, label = classification)) +
  scale_x_discrete(expand = c(0.1,0.1)) +
  scale_fill_manual(values = c("#CC6633", "#FF3399", "#FF9933", "#FF9900", "#FFFF66", "#0033FF")) +
  geom_flow(stat = "alluvium", lode.guidance = "frontback", color = "darkgray", alpha = 0.5) +
  geom_stratum(alpha = .5, fill = "white") +
  geom_text(stat = "stratum", size = 4) +
  theme(legend.position = "none") +
  labs(y = "Number of species",
       x = "Method",
       title = "(b)") +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12))

traitsandclimateplot

# scale fill manual values
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/ 

# save the plot

dev.print(pdf, 'plot.pdf')
# https://stackoverflow.com/questions/7144118/how-to-save-a-plot-as-image-on-the-disk



