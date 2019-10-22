#### new script just for PCA and cluster analysis
#### now have Crla, Atmo, Pich, Ulpa leaf area
#### Still missing Brru, Crma, Atfi leaf area
#### Phro and Kebe have no environmental data due to low records

######################################################################################################################################
################################### PCA on glasshouse variables and selected environmental variables #################################
######################################################## plus leaf area ##############################################################
######################################################################################################################################

library(tidyverse)
library("FactoMineR")
library("factoextra")

other.variables <- read.csv("PCA_Cluster_data/PCA_data.csv")

hugh.data <- read.csv("MFA_data/niche.data.HB.csv")

hugh.data <- hugh.data %>%
  select(searchTaxon, Annual_precip_mean, Precip_dry_qu_mean) %>% # selecting the relevant variables
  rename(Species=searchTaxon)

all.data <- left_join(other.variables, hugh.data, by = "Species")

all.data <- filter(all.data, Species_Code != "Phro", Species_Code != "Kebe", 
                   Species_Code != "Crma", Species_Code != "Atfi",
                   Species_Code != "Brru") # filter out species with missing climate or leaf area


# need to transform the succulance variable and leaf area variable to make it linear
PCA_analysis <- all.data %>%
  mutate(log_mean_succulence_g = log(mean_succulance_g), log_mean_leaf_area_cm2 = log(mean_leaf_area_cm2)) %>%
  select(-mean_succulance_g, -mean_leaf_area_cm2, -mean_Lobosity) %>% # need to remove lobosity
  remove_rownames %>%
  column_to_rownames(var="Species_Code") # make species code the row names

# remove the categorical vaiables

PCA_analysis_cont <- select(PCA_analysis, 9:15)

# doing the PCA

res.pca <- PCA(PCA_analysis_cont, graph = FALSE)

print(res.pca)

# looking at the eigenvalues

eig.val <- get_eigenvalue(res.pca)
eig.val
# first 3 PCs explain 73% of the data

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
# succulence and leaf area

# Contributions of variables to PC2

fviz_contrib(res.pca, choice = "var", axes = 2, top = 4)
# thickness, annual precip, LMA

# dimension description
# identify the most significanlty associated variables for a given PC

res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1 # succulence and leaf area
# Description of dimension 2
res.desc$Dim.2 # LMA and thickness

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


# biplot

biplot <- fviz_pca_biplot(res.pca, 
                          col.ind = PCA_analysis$Growth_Form, palette = "jco", 
                          addEllipses = TRUE, ellipse.type = "confidence", label = "var",
                          col.var = "black", repel = TRUE,
                          legend.title = "Growth Form")

biplot

# Hierarchical clustering

# let's only do the PCA on the first 3 dimensions

res.pca <- PCA(PCA_analysis_cont, ncp = 3, graph = FALSE)

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

# display the qualtitative variables that explain the most variance in each cluster

res.hcpc$desc.var$quanti

# principle dimensions that are most associated with clusters
res.hcpc$desc.axes$quanti

# save the output
pdf("PCA_Cluster_output/glasshouse_all.pdf") # Create a new pdf device
print(var.plot)
print(biplot)
print(ind.plot)
print(cluster)
dev.off() # Close the pdf device

# Hierarchical clustering WITH NO CONSOLIDATION
# https://www.youtube.com/watch?v=4XrgWmN9erg&list=PLnZgp6epRBbTsZEFXi_p6W48HhNyqwxIu&index=7

# let's only do the PCA on the first 3 dimensions

res.pca <- PCA(PCA_analysis_cont, ncp = 3, graph = FALSE)

# clustering
res.hcpc <- HCPC(res.pca, consol = FALSE, graph = FALSE)

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
cluster # looks worse!

######################################################################################################################################
################################### PCA on glasshouse variables and selected environmental variables #################################
######################################################## minus OsmPot ##############################################################
######################################################################################################################################

library(tidyverse)
library("FactoMineR")
library("factoextra")

other.variables <- read.csv("PCA_Cluster_data/PCA_data.csv")

hugh.data <- read.csv("MFA_data/niche.data.HB.csv")

hugh.data <- hugh.data %>%
  select(searchTaxon, Annual_precip_mean, Precip_dry_qu_mean) %>% # selecting the relevant variables
  rename(Species=searchTaxon)

all.data <- left_join(other.variables, hugh.data, by = "Species")

all.data <- filter(all.data, Species_Code != "Phro", Species_Code != "Kebe", Species_Code != "Crma", Species_Code != "Atfi",
                   Species_Code != "Brru") # filter out species with missing climate or leaf area

# need to transform the succulance variable and leaf area variable to make it linear
PCA_analysis <- all.data %>%
  mutate(log_mean_succulence_g = log(mean_succulance_g), log_mean_leaf_area_cm2 = log(mean_leaf_area_cm2)) %>%
  select(-mean_succulance_g, -mean_leaf_area_cm2, -mean_Lobosity, -mean_OsmPot) %>% # need to remove lobosity
  remove_rownames %>%
  column_to_rownames(var="Species_Code") # make species code the row names

# remove the categorical vaiables

PCA_analysis_cont <- select(PCA_analysis, 9:14)

# doing the PCA

res.pca <- PCA(PCA_analysis_cont, graph = FALSE)

# looking at the eigenvalues

eig.val <- get_eigenvalue(res.pca)
eig.val
# first 3 PCs explain 83% of the data

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
# succulence and leaf area

# Contributions of variables to PC2

fviz_contrib(res.pca, choice = "var", axes = 2, top = 4)
# thickness, annual precip, LMA

# dimension description
# identify the most significanlty associated variables for a given PC

res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1 # succulence and leaf area
# Description of dimension 2
res.desc$Dim.2 # LMA and thickness

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


# biplot

biplot <- fviz_pca_biplot(res.pca, 
                          col.ind = PCA_analysis$Growth_Form, palette = "jco", 
                          addEllipses = TRUE, ellipse.type = "confidence", label = "var",
                          col.var = "black", repel = TRUE,
                          legend.title = "Growth Form")

biplot

# Hierarchical clustering

# let's only do the PCA on the first 3 dimensions

res.pca <- PCA(PCA_analysis_cont, ncp = 3, graph = FALSE)

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

# display the qualtitative variables that explain the most variance in each cluster

res.hcpc$desc.var$quanti

# principle dimensions that are most associated with clusters
res.hcpc$desc.axes$quanti

# save the output
pdf("PCA_Cluster_output/glasshouse_no_osmpot.pdf") # Create a new pdf device
print(var.plot)
print(biplot)
print(ind.plot)
print(cluster)
dev.off() # Close the pdf device

# Hierarchical clustering WITH NO CONSOLIDATION
# https://www.youtube.com/watch?v=4XrgWmN9erg&list=PLnZgp6epRBbTsZEFXi_p6W48HhNyqwxIu&index=7

# let's only do the PCA on the first 3 dimensions

res.pca <- PCA(PCA_analysis_cont, ncp = 3, graph = FALSE)

# clustering
res.hcpc <- HCPC(res.pca, consol = FALSE, graph = FALSE)

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
cluster # looks worse!

######################################################################################################################################
########################################################## PCA on glasshouse variables ONLY ############################################
######################################################################################################################################
######################################################################################################################################

library(tidyverse)
library("FactoMineR")
library("factoextra")

other.variables <- read.csv("PCA_Cluster_data/PCA_data.csv")

other.variables <- filter(other.variables, Species_Code != "Crma", Species_Code != "Atfi",
                   Species_Code != "Brru") # filter out species with missing leaf area


# need to transform the succulance variable and leaf area variable to make it linear
PCA_analysis <- other.variables %>%
  mutate(log_mean_succulence_g = log(mean_succulance_g), log_mean_leaf_area_cm2 = log(mean_leaf_area_cm2)) %>%
  select(-mean_succulance_g, -mean_leaf_area_cm2, -mean_Lobosity) %>% # need to remove lobosity
  remove_rownames %>%
  column_to_rownames(var="Species_Code") # make species code the row names

# remove the categorical vaiables

PCA_analysis_cont <- select(PCA_analysis, 9:13)

# doing the PCA

res.pca <- PCA(PCA_analysis_cont, graph = FALSE)

# looking at the eigenvalues

eig.val <- get_eigenvalue(res.pca)
eig.val
# first 3 PCs explain 92% of the data

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
# succulence and leaf area

# Contributions of variables to PC2

fviz_contrib(res.pca, choice = "var", axes = 2, top = 4)
# LMA

# dimension description
# identify the most significanlty associated variables for a given PC

res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1 # succulence and leaf area
# Description of dimension 2
res.desc$Dim.2 # LMA

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


# biplot

biplot <- fviz_pca_biplot(res.pca, 
                          col.ind = PCA_analysis$Growth_Form, palette = "jco", 
                          addEllipses = TRUE, ellipse.type = "confidence", label = "var",
                          col.var = "black", repel = TRUE,
                          legend.title = "Growth Form")

biplot

# Hierarchical clustering

# let's only do the PCA on the first 3 dimensions

res.pca <- PCA(PCA_analysis_cont, ncp = 3, graph = FALSE)

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
write.csv(PCA_traits_cluster_output,"PCA_Cluster_output/PCA_traits_cluster_output.csv", row.names = TRUE) # need row names for species

# display the qualtitative variables that explain the most variance in each cluster

res.hcpc$desc.var$quanti

# principle dimensions that are most associated with clusters
res.hcpc$desc.axes$quanti

# save the output
pdf("PCA_Cluster_output/glasshouse_no_climate.pdf") # Create a new pdf device
print(var.plot)
print(biplot)
print(ind.plot)
print(cluster)
dev.off() # Close the pdf device

# Hierarchical clustering WITH NO CONSOLIDATION
# https://www.youtube.com/watch?v=4XrgWmN9erg&list=PLnZgp6epRBbTsZEFXi_p6W48HhNyqwxIu&index=7

# let's only do the PCA on the first 3 dimensions

res.pca <- PCA(PCA_analysis_cont, ncp = 3, graph = FALSE)

# clustering
res.hcpc <- HCPC(res.pca, consol = FALSE, graph = FALSE)

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
cluster # looks worse!

#####################################
### PCA on glasshouse variables 
# LDMC instead of succulence

library(tidyverse)
library("FactoMineR")
library("factoextra")

other.variables <- read.csv("PCA_Cluster_data/PCA_data.csv")

other.variables <- filter(other.variables, Species_Code != "Crma", Species_Code != "Atfi",
                          Species_Code != "Brru") # filter out species with missing leaf area

other.variables <- select(other.variables, Species, Species_Code, mean_OsmPot, mean_LMA_gm2, Thickness_mm_Avg, mean_leaf_area_cm2)

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
PCA_analysis <- left_join(other.variables, LDMC, by = "Species_Code")

## let's see if the data are linearly related

# removed the categorical species columns

PCA_analysis_pairs <- select(PCA_analysis, -Species, -Species_Code)

# let's see if the data are linearly related

pairs(PCA_analysis_pairs)

# might need to transform the thickness and area variables

PCA_analysis_pairs <- PCA_analysis_pairs %>%
  mutate(log_Thickness_mm_Avg = log(Thickness_mm_Avg),
         log_leaf_area_cm2 = log(mean_leaf_area_cm2)) %>%
  select(-Thickness_mm_Avg, -mean_leaf_area_cm2)

pairs(PCA_analysis_pairs) # looks better

# transform in the main spreadsheet

PCA_analysis <- PCA_analysis %>%
  mutate(log_Thickness_mm_Avg = log(Thickness_mm_Avg),
         log_leaf_area_cm2 = log(mean_leaf_area_cm2)) %>%
  select(-Thickness_mm_Avg, -mean_leaf_area_cm2, -Species)

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
write.csv(PCA_traits_cluster_output,"PCA_Cluster_output/PCA_traits_cluster_output_LDMC.csv", row.names = TRUE) # need row names for species

# display the qualtitative variables that explain the most variance in each cluster

res.hcpc$desc.var$quanti

# principle dimensions that are most associated with clusters
res.hcpc$desc.axes$quanti

# save the output
pdf("PCA_Cluster_output/glasshouse_no_climate_LDMC.pdf") # Create a new pdf device
print(var.plot)
print(ind.plot)
print(cluster)
dev.off() # Close the pdf device


######################################################################################################################################
################################################## PCA on glasshouse variables and aridity ###############################################################
#####################################################################################################################################
######################################################################################################################################

library(tidyverse)
library("FactoMineR")
library("factoextra")

other.variables <- read.csv("PCA_Cluster_data/PCA_data.csv")

hugh.data <- read.csv("MFA_data/niche.data.HB.csv")

hugh.data <- hugh.data %>%
  select(searchTaxon, AI_mean) %>% # selecting the relevant variables
  rename(Species=searchTaxon)

all.data <- left_join(other.variables, hugh.data, by = "Species")

all.data <- filter(all.data, Species_Code != "Phro", Species_Code != "Kebe", Species_Code != "Crma", Species_Code != "Atfi",
                   Species_Code != "Brru") # filter out species with missing climate or leaf area

# need to transform the succulance variable and leaf area variable to make it linear
PCA_analysis <- all.data %>%
  mutate(log_mean_succulence_g = log(mean_succulance_g), log_mean_leaf_area_cm2 = log(mean_leaf_area_cm2)) %>%
  select(-mean_succulance_g, -mean_leaf_area_cm2, -mean_Lobosity) %>% # need to remove lobosity
  remove_rownames %>%
  column_to_rownames(var="Species_Code") # make species code the row names

# remove the categorical vaiables

PCA_analysis_cont <- select(PCA_analysis, 9:14)

# doing the PCA

res.pca <- PCA(PCA_analysis_cont, graph = FALSE)

# looking at the eigenvalues

eig.val <- get_eigenvalue(res.pca)
eig.val
# first 3 PCs explain 79% of the data

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
# succulence and leaf area

# Contributions of variables to PC2

fviz_contrib(res.pca, choice = "var", axes = 2, top = 4)
# LMA, thickness

# dimension description
# identify the most significanlty associated variables for a given PC

res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1 # succulence and leaf area
# Description of dimension 2
res.desc$Dim.2 # LMA and thickness

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


# biplot

biplot <- fviz_pca_biplot(res.pca, 
                          col.ind = PCA_analysis$Growth_Form, palette = "jco", 
                          addEllipses = TRUE, ellipse.type = "confidence", label = "var",
                          col.var = "black", repel = TRUE,
                          legend.title = "Growth Form")

biplot

# Hierarchical clustering

# let's only do the PCA on the first 3 dimensions

res.pca <- PCA(PCA_analysis_cont, ncp = 3, graph = FALSE)

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
PCA_traitsAI_cluster_output <- res.hcpc$data.clust
write.csv(PCA_traitsAI_cluster_output,"PCA_Cluster_output/PCA_traitsAI_cluster_output.csv", row.names = TRUE) # need row names for species

# display the qualtitative variables that explain the most variance in each cluster

res.hcpc$desc.var$quanti

# principle dimensions that are most associated with clusters
res.hcpc$desc.axes$quanti

# save the output
pdf("PCA_Cluster_output/glasshouse_aridity.pdf") # Create a new pdf device
print(var.plot)
print(biplot)
print(ind.plot)
print(cluster)
dev.off() # Close the pdf device

# Hierarchical clustering WITH NO CONSOLIDATION
# https://www.youtube.com/watch?v=4XrgWmN9erg&list=PLnZgp6epRBbTsZEFXi_p6W48HhNyqwxIu&index=7

# let's only do the PCA on the first 3 dimensions

res.pca <- PCA(PCA_analysis_cont, ncp = 3, graph = FALSE)

# clustering
res.hcpc <- HCPC(res.pca, consol = FALSE, graph = FALSE)

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
cluster # looks worse!

#####################################
### PCA on glasshouse variables and aridity
# LDMC instead of succulence

library(tidyverse)
library("FactoMineR")
library("factoextra")

other.variables <- read.csv("PCA_Cluster_data/PCA_data.csv")

hugh.data <- read.csv("MFA_data/niche.data.HB.csv")

hugh.data <- hugh.data %>%
  select(searchTaxon, AI_mean) %>% # selecting the relevant variables
  rename(Species=searchTaxon)

other.variables <- filter(other.variables, Species_Code != "Crma", Species_Code != "Atfi",
                          Species_Code != "Brru", Species_Code != "Kebe", Species_Code != "Phro") # filter out species with missing leaf area or climate data

other.variables <- select(other.variables, Species, Species_Code, mean_OsmPot, mean_LMA_gm2, Thickness_mm_Avg, mean_leaf_area_cm2)

all.data <- left_join(other.variables, hugh.data, by = "Species")

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
PCA_analysis <- left_join(all.data, LDMC, by = "Species_Code")

## let's see if the data are linearly related

# removed the categorical species columns

PCA_analysis_pairs <- select(PCA_analysis, -Species, -Species_Code)

# let's see if the data are linearly related

pairs(PCA_analysis_pairs)

# might need to transform the thickness and area variables

PCA_analysis_pairs <- PCA_analysis_pairs %>%
  mutate(log_Thickness_mm_Avg = log(Thickness_mm_Avg),
         log_leaf_area_cm2 = log(mean_leaf_area_cm2)) %>%
  select(-Thickness_mm_Avg, -mean_leaf_area_cm2)

pairs(PCA_analysis_pairs) # looks better

# transform in the main spreadsheet

PCA_analysis <- PCA_analysis %>%
  mutate(log_Thickness_mm_Avg = log(Thickness_mm_Avg),
         log_leaf_area_cm2 = log(mean_leaf_area_cm2)) %>%
  select(-Thickness_mm_Avg, -mean_leaf_area_cm2, -Species)

# make species code the row name

PCA_analysis <- PCA_analysis %>%
  remove_rownames %>%
  column_to_rownames(var="Species_Code")

# doing the PCA

res.pca <- PCA(PCA_analysis, graph = FALSE)

# looking at the eigenvalues

eig.val <- get_eigenvalue(res.pca)
eig.val
# first 3 PCs explain 78% of the data

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
# LDMC and osmpot

# Contributions of variables to PC2

fviz_contrib(res.pca, choice = "var", axes = 2, top = 4)
# LMA, thickness

# dimension description
# identify the most significanlty associated variables for a given PC

res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1 # LDMC and osmpot
# Description of dimension 2
res.desc$Dim.2 # LMA and thickness

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
PCA_traitsAI_cluster_output <- res.hcpc$data.clust
write.csv(PCA_traitsAI_cluster_output,"PCA_Cluster_output/PCA_traitsAI_cluster_output_LDMC.csv", row.names = TRUE) # need row names for species

# display the qualtitative variables that explain the most variance in each cluster

res.hcpc$desc.var$quanti

# principle dimensions that are most associated with clusters
res.hcpc$desc.axes$quanti

# save the output
pdf("PCA_Cluster_output/glasshouse_aridity_LDMC.pdf") # Create a new pdf device
print(var.plot)
print(ind.plot)
print(cluster)
dev.off() # Close the pdf device

# IT LOOKS LIKE A MESS

######################################################################################################################################
################################### PCA on glasshouse variables, precip and aridity ##################################################
######################################################################################################################################
######################################################################################################################################

library(tidyverse)
library("FactoMineR")
library("factoextra")

other.variables <- read.csv("PCA_Cluster_data/PCA_data.csv")

hugh.data <- read.csv("MFA_data/niche.data.HB.csv")

hugh.data <- hugh.data %>%
  select(searchTaxon, Annual_precip_mean, Precip_dry_qu_mean, AI_mean) %>% # selecting the relevant variables
  rename(Species=searchTaxon)

all.data <- left_join(other.variables, hugh.data, by = "Species")

all.data <- filter(all.data, Species_Code != "Phro", Species_Code != "Kebe", Species_Code != "Crma", Species_Code != "Atfi",
                   Species_Code != "Brru") # filter out species with missing climate or leaf area


# need to transform the succulance variable and leaf area variable to make it linear
PCA_analysis <- all.data %>%
  mutate(log_mean_succulence_g = log(mean_succulance_g), log_mean_leaf_area_cm2 = log(mean_leaf_area_cm2)) %>%
  select(-mean_succulance_g, -mean_leaf_area_cm2, -mean_Lobosity) %>% # need to remove lobosity
  remove_rownames %>%
  column_to_rownames(var="Species_Code") # make species code the row names

# remove the categorical vaiables

PCA_analysis_cont <- select(PCA_analysis, 9:16)

# doing the PCA

res.pca <- PCA(PCA_analysis_cont, graph = FALSE)

print(res.pca)

# looking at the eigenvalues

eig.val <- get_eigenvalue(res.pca)
eig.val
# first 3 PCs explain 75% of the data

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
# AI, precip

# Contributions of variables to PC2

fviz_contrib(res.pca, choice = "var", axes = 2, top = 4)
# succulence, thickness, area

# dimension description
# identify the most significanlty associated variables for a given PC

res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1 # Ai and precip
# Description of dimension 2
res.desc$Dim.2 # succulence, thickness, area
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


# biplot

biplot <- fviz_pca_biplot(res.pca, 
                          col.ind = PCA_analysis$Growth_Form, palette = "jco", 
                          addEllipses = TRUE, ellipse.type = "confidence", label = "var",
                          col.var = "black", repel = TRUE,
                          legend.title = "Growth Form")

biplot

# Hierarchical clustering

# let's only do the PCA on the first 3 dimensions

res.pca <- PCA(PCA_analysis_cont, ncp = 3, graph = FALSE)

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

# display the qualtitative variables that explain the most variance in each cluster

res.hcpc$desc.var$quanti

# principle dimensions that are most associated with clusters
res.hcpc$desc.axes$quanti

# save the output
pdf("PCA_Cluster_output/glasshouse_precip_aridity.pdf") # Create a new pdf device
print(var.plot)
print(biplot)
print(ind.plot)
print(cluster)
dev.off() # Close the pdf device

# Hierarchical clustering WITH NO CONSOLIDATION
# https://www.youtube.com/watch?v=4XrgWmN9erg&list=PLnZgp6epRBbTsZEFXi_p6W48HhNyqwxIu&index=7

# let's only do the PCA on the first 3 dimensions

res.pca <- PCA(PCA_analysis_cont, ncp = 3, graph = FALSE)

# clustering
res.hcpc <- HCPC(res.pca, consol = FALSE, graph = FALSE)

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
cluster # looks worse!
