# Links

# https://rstudio-pubs-static.s3.amazonaws.com/423873_adfdb38bce8d47579f6dc916dd67ae75.html
# https://www.datanovia.com/en/blog/cluster-analysis-in-r-simplified-and-enhanced/
# https://medium.com/@rumman1988/clustering-categorical-and-numerical-datatype-using-gower-distance-ab89b3aa90d9

# try this first
# cluser analysis after 

############################################################################################################################################################################
################################################## Hierarchical Clustering after FAMD analysis #############################################################################
############################################################################################################################################################################

# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/117-hcpc-hierarchical-clustering-on-principal-components-essentials/
# http://www.sthda.com/english/wiki/print.php?id=245
# https://www.youtube.com/watch?v=4XrgWmN9erg&list=PLnZgp6epRBbTsZEFXi_p6W48HhNyqwxIu&index=7

# FAMD reduces the dimensionality f the data which can lead to more stable clustering

# Run the FAMD first
library(tidyverse)

# Load the data, this includes the glasshouse variables + my scrapped variables (I have modified them to be more informative)
FAMD_data <- read.csv("MFA_data/FAMD_data.csv")

# need to transform the succulance variable to make it linear
FAMD_data <- FAMD_data %>%
  mutate(log_mean_succulance_g = log(mean_succulance_g)) %>%
  select(-mean_succulance_g) %>%
  remove_rownames %>%
  column_to_rownames(var="Species_Code") # make species code the row names

# remove the categorical vaiables that aren't needed/used

FAMD_data_analysis <- select(FAMD_data, -Species, -Origin, -Growth_Form)

library("FactoMineR")
library("factoextra")

str(FAMD_data_analysis)

# running the FAMD analysis

res.famd <- FAMD(FAMD_data_analysis, graph = FALSE)

# extract the eigenvalues

eig.val <- get_eigenvalue(res.famd)
head(eig.val) # first 3 dimensions explain 49% of the data

# draw the scree plot
fviz_screeplot(res.famd)

## quantitative variables

quanti.var <- get_famd_var(res.famd, "quanti.var")

# plot the quantitative variables

quanti.plot <- fviz_famd_var(res.famd, "quanti.var", repel = TRUE, col.var = "black")
quanti.plot

## qualitative variables

quali.var <- get_famd_var(res.famd, "quali.var")

# plot the qualitative variables

quali.plot <- fviz_famd_var(res.famd, "quali.var", repel = TRUE, col.var = "black")
quali.plot

### graph of individuals

ind <- get_famd_ind(res.famd)

ind.graph <- fviz_famd_ind(res.famd, repel = TRUE, invisible = "quali.var", col.ind = "black")
ind.graph

# colour individuals by growth form (used ?fviz_famd_ind to figure out)

library(ggplot2)

individuals <- fviz_famd_ind(res.famd, 
                             geom = "point", # show points only (but not "text")
                             col.ind = FAMD_data$Growth_Form, # color by growth form 
                             palette = c("Dark2"),
                             addEllipses = TRUE, ellipse.type = "confidence", # confidence ellipse
                             legend.title = "Growth from",
                             invisible = "quali.var")

individuals

# Compute hierarchical clustering

res.hcpc <- HCPC(res.famd, graph = FALSE)

# dendrogram
fviz_dend(res.hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8)     # Augment the room for labels

# graph
fviz_cluster(res.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map")

# Let's look at the HCPC output

# display the original data with a new column indicating which cluster they belong to

head(res.hcpc$data.clust)

# display the quantitative variables that explain the most variance in each cluster

res.hcpc$desc.var$quanti

# same thing but for categorical variables
res.hcpc$desc.var$category

# principle dimensions that are most associated with clusters
res.hcpc$desc.axes$quanti

# save the output
pdf("MFA_data_output/FAMD_scraped_glasshouse.pdf") # Create a new pdf device
print(quanti.plot)
print(quali.plot)
print(individuals)
dev.off() # Close the pdf device

################################################################################################################################################
################################################################### Ale's suggestion ####################################################################
################################################################################################################################################

# 1. PCA and cluster analysis on glasshouse ecophys variables
# 2. MCA and cluster analysis on the scraped data
# 3. FAMD and cluster analysis on everything (already did)
# 4. Repeat all this but use only trees and shrubs

# we want to look at how much variation is explained by each analysis and where an additive model is better
# 'can soft traits be used to predict drought tolerance of plants?'

####################################### 1. PCA and cluster analysis on glasshouse ecophys variables

# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

# load my practise dataset, this is a subset of the variables I have (only the physiological stuff) + corrected growth form

PCA_analysis <- read.csv("MFA_data/FAMD_data.csv")

library(tidyverse)
library("FactoMineR")
library("factoextra")

# need to transform the succulance variable to make it linear
PCA_analysis <- PCA_analysis %>%
  mutate(log_mean_succulance_g = log(mean_succulance_g)) %>%
  select(-mean_succulance_g) %>%
  remove_rownames %>%
  column_to_rownames(var="Species_Code") # make species code the row names

# remove the categorical vaiables that aren't needed/used

PCA_analysis_cont <- select(PCA_analysis, 9:12)

## scaling the data
# data need to be in the same scale before analysis
# PCA() in FActoMineR does this automatically

# doing the PCA

res.pca <- PCA(PCA_analysis_cont, graph = FALSE)

print(res.pca)

# looking at the eigenvalues

eig.val <- get_eigenvalue(res.pca)
eig.val
# first three PCs explain 92% of the data

# let's look at the scree plot

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 60))

# let's look at the variables

var <- get_pca_var(res.pca)
var

# plot the variables

var.plot <- fviz_pca_var(res.pca, col.var = "black")
var.plot

# quality of representation

install.packages("corrplot")
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)
# think this means that OsmPot is correlated with Dim2 while succulance, thickness and LMA is correlated with Dim1

# colour variables on the correlation circle by their contrib value -> contribution of the variable on the factor map

fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), # specify custom gradient
             repel = TRUE # avoid text overlap
)


# Contributions of variables to PC1

fviz_contrib(res.pca, choice = "var", axes = 1, top = 4) # red dashed line is the expected average contribution (i.e. 4 variables = 25%)

# Contributions of variables to PC2

fviz_contrib(res.pca, choice = "var", axes = 2, top = 4)

# Contributions of variables to PC3

fviz_contrib(res.pca, choice = "var", axes = 3, top = 4)

# dimension description
# identify the most significanlty associated variables for a given PC

res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1 # thickness
# Description of dimension 2
res.desc$Dim.2 # osmotic potential

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

# I'm trying to add the biplot of the variables into this but it isn't working. The coordinates are off. Will need to fix later.
# I actually want the biplot coordinates! Need to figure out how to extract those coordinates

# quanti <- fviz_add(cluster, res.pca$var$coord, 
                   # repel = TRUE,
                   # col = "blue",
                   # linetype = "solid",
                   # geom = c("arrow", "text"))

# Let's look at the HCPC output

# display the original data with a new column indicating which cluster they belong to

head(res.hcpc$data.clust)

# display the qualtitative variables that explain the most variance in each cluster

res.hcpc$desc.var$quanti

# principle dimensions that are most associated with clusters
res.hcpc$desc.axes$quanti

# save the output
pdf("MFA_data_output/Cluster_analysis/PCA_glasshouse_all.pdf") # Create a new pdf device
print(var.plot)
print(biplot)
print(ind.plot)
print(cluster)
dev.off() # Close the pdf device

#################################### subsetting the data for only the trees and shrubs

PCA_analysis <- read.csv("MFA_data/FAMD_data.csv")

# filter out just trees and shrubs

PCA_analysis <- filter(PCA_analysis, Growth_Form == "Tree" | Growth_Form == "Shrub") # 92 native and exotic trees and shrubs

# pull out just the continuous variables

PCA_analysis_cont <- PCA_analysis %>%
  remove_rownames %>%
  column_to_rownames(var="Species_Code") %>%
  select(9:12)

# let's see if the data are linearly related

pairs(PCA_analysis_cont)

# might need to transform the mean_succulance variable

PCA_analysis_cont <- PCA_analysis_cont %>%
  mutate(log_mean_succulance_g = log(mean_succulance_g))

PCA_analysis_cont <- select(PCA_analysis_cont, -mean_succulance_g) # remove the untransformed variable

pairs(PCA_analysis_cont) # looks better

# add back the Species_Code

PCA_analysis_cont <- cbind(PCA_analysis$Species_Code, PCA_analysis_cont)

# make species code the row name

PCA_analysis_cont <- PCA_analysis_cont %>%
  remove_rownames %>%
  column_to_rownames(var="PCA_analysis$Species_Code")

# doing the PCA

res.pca <- PCA(PCA_analysis_cont, graph = FALSE)

print(res.pca)

# looking at the eigenvalues

eig.val <- get_eigenvalue(res.pca)
eig.val
# first three PCs explain 92% of the data

# let's look at the scree plot

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 60))

# let's look at the variables

var <- get_pca_var(res.pca)
var

# plot the variables

var.plot <- fviz_pca_var(res.pca, col.var = "black")
var.plot

# quality of representation

install.packages("corrplot")
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)
# think this means that OsmPot is correlated with Dim2 while succulance, thickness and LMA is correlated with Dim1

# colour variables on the correlation circle by their contrib value -> contribution of the variable on the factor map

fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), # specify custom gradient
             repel = TRUE # avoid text overlap
)


# Contributions of variables to PC1

fviz_contrib(res.pca, choice = "var", axes = 1, top = 4) # red dashed line is the expected average contribution (i.e. 4 variables = 25%)

# Contributions of variables to PC2

fviz_contrib(res.pca, choice = "var", axes = 2, top = 4)

# Contributions of variables to PC3

fviz_contrib(res.pca, choice = "var", axes = 3, top = 4)

# dimension description
# identify the most significanlty associated variables for a given PC

res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1 # thickness
# Description of dimension 2
res.desc$Dim.2 # osmotic potential and LMA

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

biplot # not really telling us anything

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
                        main = "PCA - trees and shrubs")

cluster

# Let's look at the HCPC output

# display the original data with a new column indicating which cluster they belong to

head(res.hcpc$data.clust)

# display the qualtitative variables that explain the most variance in each cluster

res.hcpc$desc.var$quanti

# principle dimensions that are most associated with clusters
res.hcpc$desc.axes$quanti

# save the output
pdf("MFA_data_output/Cluster_analysis/PCA_glasshouse_trees_shrubs.pdf") # Create a new pdf device
print(var.plot)
print(biplot)
print(ind.plot)
print(cluster)
dev.off() # Close the pdf device

####################################### 2. MCA and cluster analysis on scraped variables

# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials/

MCA_analysis <- read.csv("MFA_data/FAMD_data.csv")

MCA_analysis_cat <- MCA_analysis %>%
  filter(Species_Code != "Mero") %>% # remove Mero as it has NAs
  select(1:9) %>%
  remove_rownames %>%
  column_to_rownames(var="Species_Code") %>%
  select(-Species, -Origin, -Growth_Form, -Water_storage_organs) # remove the other categorical variables we won't need
# for variables categories with a low frequency (i.e. water storage) - these variables can distort the analysis

# running the MCA

res.mca <- MCA(MCA_analysis_cat, graph = FALSE)

print(res.mca)

# eigen values

eig.val <- get_eigenvalue(res.mca)
head(eig.val) # removing Mero has significantly improved it!
# first 3 dimensions explain 65%

## graph of variables

var <- get_mca_var(res.mca)
var

# Contributions of variables to PC1

fviz_contrib(res.mca, choice = "var", axes = 1, top = 4) # red dashed line is the expected average contribution (i.e. 4 variables = 25%)
# drought senescent

# Contributions of variables to PC2

fviz_contrib(res.mca, choice = "var", axes = 2, top = 4)
# fully hairy, not woody

# correlation between variables and principle dimensions

fviz_mca_var(res.mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())

# coordinates of variable categories

cat.var <- fviz_mca_var(res.mca, 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())
cat.var

# contribution of the variable categories to the dimensions

var$contrib

## graph of individuals

ind <- get_mca_ind(res.mca)
ind

# colour individuals by cos2 value
fviz_mca_ind(res.mca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping (slow if many points)
             ggtheme = theme_minimal())

# biplot

biplot <- fviz_mca_biplot(res.mca, repel = TRUE,
                ggtheme = theme_minimal())
biplot

# colour individuals by group

# need to remove Mero from original dataset first

MCA_analysis <- filter(MCA_analysis, Species_Code != "Mero")

ind <- fviz_mca_ind(res.mca,
             label = "none", # show points only (but not "text")
             habillage = MCA_analysis$Growth_Form, # color by growth form
             palette = c("Dark2"),
             addEllipses = TRUE, ellipse.type = "confidence", # confidence ellipses
             legend.title = "Growth form")
ind

# v. small number of points - only so many combinations of categorial variables there could be

# Hierarchical clustering

# let's only do the MCA on the first 3 dimensions

res.mca <- MCA(MCA_analysis_cat, ncp = 3, graph = FALSE)

# clustering
res.hcpc <- HCPC(res.mca, graph = FALSE)

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
                        main = "MCA")

cluster

#Let's look at the HCPC output

# display the original data with a new column indicating which cluster they belong to

head(res.hcpc$data.clust)

# display the qualitative variables that explain the most variance in each cluster

res.hcpc$desc.var$category

# principle dimensions that are most associated with clusters
res.hcpc$desc.axes$quanti

# save the output
pdf("MFA_data_output/Cluster_analysis/MCA_glasshouse_all.pdf") # Create a new pdf device
print(cat.var)
print(biplot)
print(ind)
print(cluster)
dev.off() # Close the pdf device

#################################### subsetting the data for only the trees and shrubs

MCA_analysis <- read.csv("MFA_data/FAMD_data.csv")

MCA_analysis_cat <- MCA_analysis %>%
  filter(Species_Code != "Mero") %>% # remove Mero as it has NAs
  filter(Growth_Form == "Tree" | Growth_Form == "Shrub") %>% # filter only the trees and shrubs
  select(1:9) %>%
  remove_rownames %>%
  column_to_rownames(var="Species_Code") %>%
  select(-Species, -Origin, -Growth_Form, -Growth_structure, -Water_storage_organs) # remove the other categorical variables we won't need
# for variables categories with a low frequency (i.e. water storage) - these variables can distort the analysis

# running the MCA

res.mca <- MCA(MCA_analysis_cat, graph = FALSE)

print(res.mca)

# eigen values

eig.val <- get_eigenvalue(res.mca)
head(eig.val) # removing Mero has significantly improved it!
# first 3 dimensions explain 75%

## graph of variables

var <- get_mca_var(res.mca)
var

# Contributions of variables to PC1

fviz_contrib(res.mca, choice = "var", axes = 1, top = 4) # red dashed line is the expected average contribution (i.e. 4 variables = 25%)
# drought senescent

# Contributions of variables to PC2

fviz_contrib(res.mca, choice = "var", axes = 2, top = 4)
# fully hairy and deciduous

# correlation between variables and principle dimensions

fviz_mca_var(res.mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())

# coordinates of variable categories

cat.var <- fviz_mca_var(res.mca, 
                        repel = TRUE, # Avoid text overlapping
                        ggtheme = theme_minimal())
cat.var

# contribution of the variable categories to the dimensions

var$contrib

## graph of individuals

ind <- get_mca_ind(res.mca)
ind

# colour individuals by cos2 value
fviz_mca_ind(res.mca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping (slow if many points)
             ggtheme = theme_minimal())

# biplot

biplot <- fviz_mca_biplot(res.mca, repel = TRUE,
                          ggtheme = theme_minimal())
biplot

# colour individuals by group

# need to remove Mero and filter for trees and shrubs from original dataset first

MCA_analysis <- MCA_analysis %>%
  filter(Species_Code != "Mero") %>%
  filter(Growth_Form == "Tree" | Growth_Form == "Shrub")

ind <- fviz_mca_ind(res.mca,
                    label = "none", # show points only (but not "text")
                    habillage = MCA_analysis$Growth_Form, # color by growth form
                    palette = c("Dark2"),
                    addEllipses = TRUE, ellipse.type = "confidence", # confidence ellipses
                    legend.title = "Growth form")
ind

# v. small number of points - only so many combinations of categorial variables there could be

# Hierarchical clustering

# let's only do the MCA on the first 3 dimensions

res.mca <- MCA(MCA_analysis_cat, ncp = 3, graph = FALSE)

# clustering
res.hcpc <- HCPC(res.mca, graph = FALSE)

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
                        main = "MCA")

cluster

# Let's look at the HCPC output

# display the original data with a new column indicating which cluster they belong to

head(res.hcpc$data.clust)

# display the qualitative variables that explain the most variance in each cluster

res.hcpc$desc.var$category

# principle dimensions that are most associated with clusters
res.hcpc$desc.axes$quanti

# save the output
pdf("MFA_data_output/Cluster_analysis/MCA_glasshouse_trees_shrubs.pdf") # Create a new pdf device
print(cat.var)
print(biplot)
print(ind)
print(cluster)
dev.off() # Close the pdf device

####################################### 3. FAMD and cluster analysis on glasshouse ecophys variables and scrapped data

FAMD_data <- read.csv("MFA_data/FAMD_data.csv")

# need to transform the succulance variable to make it linear and remove Mero
FAMD_data <- FAMD_data %>%
  filter(Species_Code != "Mero") %>%
  mutate(log_mean_succulance_g = log(mean_succulance_g)) %>%
  select(-mean_succulance_g) %>%
  remove_rownames %>%
  column_to_rownames(var="Species_Code") # make species code the row names

# remove the categorical variables that aren't needed/used

FAMD_data_analysis <- select(FAMD_data, -Species, -Origin, -Growth_Form, -Water_storage_organs) # removing water storage organ trait as it is too skewed

str(FAMD_data_analysis)

# running the FAMD analysis

res.famd <- FAMD(FAMD_data_analysis, graph = FALSE)

# extract the eigenvalues

eig.val <- get_eigenvalue(res.famd)
head(eig.val) # first 3 dimensions explain 50% of the data

# draw the scree plot
fviz_screeplot(res.famd)

## quantitative variables

quanti.var <- get_famd_var(res.famd, "quanti.var")

# plot the quantitative variables

quanti.plot <- fviz_famd_var(res.famd, "quanti.var", repel = TRUE, col.var = "black")
quanti.plot

## qualitative variables

quali.var <- get_famd_var(res.famd, "quali.var")

# plot the qualitative variables

quali.plot <- fviz_famd_var(res.famd, "quali.var", repel = TRUE, col.var = "black")
quali.plot

### graph of individuals

ind <- get_famd_ind(res.famd)

ind.graph <- fviz_famd_ind(res.famd, repel = TRUE, invisible = "quali.var", col.ind = "black")
ind.graph

# colour individuals by growth form (used ?fviz_famd_ind to figure out)

library(ggplot2)

individuals <- fviz_famd_ind(res.famd, 
                             geom = "point", # show points only (but not "text")
                             col.ind = FAMD_data$Growth_Form, # color by growth form 
                             palette = c("Dark2"),
                             addEllipses = TRUE, ellipse.type = "confidence", # confidence ellipse
                             legend.title = "Growth from",
                             invisible = "quali.var")

individuals

# Compute hierarchical clustering

# need to reduce the number of dimensions first
res.famd <- FAMD(FAMD_data_analysis, ncp = 3, graph = FALSE)

res.hcpc <- HCPC(res.famd, graph = FALSE)

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
             main = "Factor map")
cluster

# Let's look at the HCPC output

# display the original data with a new column indicating which cluster they belong to

head(res.hcpc$data.clust)

# display the quantitative variables that explain the most variance in each cluster

res.hcpc$desc.var$quanti

# same thing but for categorical variables
res.hcpc$desc.var$category

# principle dimensions that are most associated with clusters
res.hcpc$desc.axes$quanti

# save the output
pdf("MFA_data_output/Cluster_analysis/FAMD_glasshouse_all.pdf") # Create a new pdf device
print(quanti.plot)
print(quali.plot)
print(individuals)
print(cluster)
dev.off() # Close the pdf device

#################################### subsetting the data for only the trees and shrubs

FAMD_data <- read.csv("MFA_data/FAMD_data.csv")

# need to transform the succulance variable to make it linear and remove Mero
FAMD_data <- FAMD_data %>%
  filter(Growth_Form == "Tree" | Growth_Form == "Shrub") %>%
  mutate(log_mean_succulance_g = log(mean_succulance_g)) %>%
  select(-mean_succulance_g) %>%
  remove_rownames %>%
  column_to_rownames(var="Species_Code") # make species code the row names

# remove the categorical variables that aren't needed/used

FAMD_data_analysis <- select(FAMD_data, -Species, -Origin, -Growth_Form, -Water_storage_organs) # removing water storage organ trait as it is too skewed

str(FAMD_data_analysis)

# running the FAMD analysis

res.famd <- FAMD(FAMD_data_analysis, graph = FALSE)

# extract the eigenvalues

eig.val <- get_eigenvalue(res.famd)
head(eig.val) # first 3 dimensions explain 54% of the data

# draw the scree plot
fviz_screeplot(res.famd)

## quantitative variables

quanti.var <- get_famd_var(res.famd, "quanti.var")

# plot the quantitative variables

quanti.plot <- fviz_famd_var(res.famd, "quanti.var", repel = TRUE, col.var = "black")
quanti.plot

## qualitative variables

quali.var <- get_famd_var(res.famd, "quali.var")

# plot the qualitative variables

quali.plot <- fviz_famd_var(res.famd, "quali.var", repel = TRUE, col.var = "black")
quali.plot

### graph of individuals

ind <- get_famd_ind(res.famd)

ind.graph <- fviz_famd_ind(res.famd, repel = TRUE, invisible = "quali.var", col.ind = "black")
ind.graph

# colour individuals by growth form (used ?fviz_famd_ind to figure out)

library(ggplot2)

individuals <- fviz_famd_ind(res.famd, 
                             geom = "point", # show points only (but not "text")
                             col.ind = FAMD_data$Growth_Form, # color by growth form 
                             palette = c("Dark2"),
                             addEllipses = TRUE, ellipse.type = "confidence", # confidence ellipse
                             legend.title = "Growth from",
                             invisible = "quali.var")

individuals

# Compute hierarchical clustering

# need to reduce the number of dimensions first
res.famd <- FAMD(FAMD_data_analysis, ncp = 3, graph = FALSE)

res.hcpc <- HCPC(res.famd, graph = FALSE)

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
                        main = "Factor map")
cluster

# Let's look at the HCPC output

# display the original data with a new column indicating which cluster they belong to

head(res.hcpc$data.clust)

# display the quantitative variables that explain the most variance in each cluster

res.hcpc$desc.var$quanti

# same thing but for categorical variables
res.hcpc$desc.var$category

# principle dimensions that are most associated with clusters
res.hcpc$desc.axes$quanti

# save the output
pdf("MFA_data_output/Cluster_analysis/FAMD_glasshouse_trees_shrubs.pdf") # Create a new pdf device
print(quanti.plot)
print(quali.plot)
print(individuals)
print(cluster)
dev.off() # Close the pdf device

#########################################################################################################################################
#################################################### FAMD on glasshouse, scraped and selected climate ###################################
#################################################### Removed water storage organs and species with NAs ##################################
#########################################################################################################################################

library(tidyverse)

other.variables <- read.csv("MFA_data/FAMD_data.csv")

hugh.data <- read.csv("MFA_data/niche.data.HB.csv")

hugh.data <- hugh.data %>%
  select(searchTaxon, Annual_precip_mean, Precip_dry_qu_mean) %>% # selecting the relevant variables
  rename(Species=searchTaxon)

all.data <- left_join(other.variables, hugh.data, by = "Species")

all.data <- filter(all.data, Species_Code != "Kebe", Species_Code != "Phro", Species_Code != "Mero") # filter out species with NAs

# let's see if the data are linearly related

all.data.cont <- select(all.data, 10:15) # select only the continuous variables

pairs(all.data.cont)
# might need to tranasform the succulance variables

all.data.cont <- all.data.cont %>%
  mutate(log_mean_succulance_g = log(mean_succulance_g)

pairs(all.data.cont) # looks better

# transform in the main datasheet

all.data <- all.data %>%
  mutate(log_mean_succulance_g = log(mean_succulance_g)) %>%
  select(-mean_succulance_g) %>%
  remove_rownames %>%
  column_to_rownames(var="Species_Code") # make species code the row names

# remove the categorical vaiables that aren't needed/used to colour points later

FAMD_data_analysis <- select(all.data, -Species, -Origin, -Growth_Form, -Water_storage_organs) # removing water storage organs

library("FactoMineR")
library("factoextra")

str(FAMD_data_analysis)

res.famd <- FAMD(FAMD_data_analysis, graph = FALSE)

# extract the eigenvalues

eig.val <- get_eigenvalue(res.famd)
head(eig.val) # first 3 dimensions explain 46% of the data

# draw the scree plot
fviz_screeplot(res.famd)

### graph of variables

## all variables

var <- get_famd_var(res.famd)

# plot of variables
fviz_famd_var(res.famd, repel = TRUE)

# colour the variables according to their contribution to the dimensions (used ?fviz_famd_var to figure out)
fviz_famd_var(res.famd, choice = "var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
              repel = TRUE,
              geom = c("arrow", "text"))

# contribution to the 1st dimension
fviz_contrib(res.famd, "var", axes = 1) # leaf drop traits contribute most to dimension 1

# contribution to 2nd dimension
fviz_contrib(res.famd, "var", axes = 2) # LMA and environmental variables contribute most to dimension 2

## quantitative variables

quanti.var <- get_famd_var(res.famd, "quanti.var")

# plot the quantitative variables

quanti.plot <- fviz_famd_var(res.famd, "quanti.var", repel = TRUE, col.var = "black")
quanti.plot

## qualitative variables

quali.var <- get_famd_var(res.famd, "quali.var")

# plot the qualitative variables

quali.plot <- fviz_famd_var(res.famd, "quali.var", repel = TRUE, col.var = "black")
quali.plot

### graph of individuals

ind <- get_famd_ind(res.famd)

# plot the individuals with the qualitative variables

fviz_famd_ind(res.famd, col.ind = "cos2", # colour by their cos2
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)

# colour individuals by growth form (used ?fviz_famd_ind to figure out)

library(ggplot2)

# first need to remove Kebe and Phro

individuals <- fviz_famd_ind(res.famd, 
                             geom = "point", # show points only (but not "text")
                             col.ind = all.data$Growth_Form, # color by growth form 
                             palette = c("Dark2"),
                             addEllipses = TRUE, ellipse.type = "confidence", # confidence ellipse
                             legend.title = "Growth from",
                             invisible = "quali.var")

individuals # doesn't show any differences!         

# Compute hierarchical clustering

# need to reduce the number of dimensions first
res.famd <- FAMD(FAMD_data_analysis, ncp = 3, graph = FALSE)

res.hcpc <- HCPC(res.famd, graph = FALSE)

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
                        main = "Factor map")
cluster

# Let's look at the HCPC output

# display the original data with a new column indicating which cluster they belong to

head(res.hcpc$data.clust)

# display the quantitative variables that explain the most variance in each cluster

res.hcpc$desc.var$quanti

# same thing but for categorical variables
res.hcpc$desc.var$category

# principle dimensions that are most associated with clusters
res.hcpc$desc.axes$quanti

# save the output
pdf("MFA_data_output/Cluster_analysis/FAMD_scraped_glasshouse_climate_all.pdf") # Create a new pdf device
print(quanti.plot)
print(quali.plot)
print(individuals)
print(cluster)
dev.off() # Close the pdf device

######################################################################################################################################
################################### PCA on glasshouse variables and selected environmental variables #################################
######################################################################################################################################

other.variables <- read.csv("MFA_data/FAMD_data.csv")

hugh.data <- read.csv("MFA_data/niche.data.HB.csv")

hugh.data <- hugh.data %>%
  select(searchTaxon, Annual_precip_mean, Precip_dry_qu_mean) %>% # selecting the relevant variables
  rename(Species=searchTaxon)

all.data <- left_join(other.variables, hugh.data, by = "Species")

all.data <- filter(all.data, Species_Code != "Kebe", Species_Code != "Phro", Species_Code != "Mero") # filter out species with NAs

library(tidyverse)
library("FactoMineR")
library("factoextra")

# need to transform the succulance variable to make it linear
PCA_analysis <- all.data %>%
  mutate(log_mean_succulance_g = log(mean_succulance_g)) %>%
  select(-mean_succulance_g) %>%
  remove_rownames %>%
  column_to_rownames(var="Species_Code") # make species code the row names

# remove the categorical vaiables

PCA_analysis_cont <- select(PCA_analysis, 9:14)

## scaling the data
# data need to be in the same scale before analysis
# PCA() in FActoMineR does this automatically

# doing the PCA

res.pca <- PCA(PCA_analysis_cont, graph = FALSE)

print(res.pca)

# looking at the eigenvalues

eig.val <- get_eigenvalue(res.pca)
eig.val
# first three PCs explain 76% of the data

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

# Contributions of variables to PC2

fviz_contrib(res.pca, choice = "var", axes = 2, top = 4)

# Contributions of variables to PC3

fviz_contrib(res.pca, choice = "var", axes = 3, top = 4)

# dimension description
# identify the most significanlty associated variables for a given PC

res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1 # succulance
# Description of dimension 2
res.desc$Dim.2 # thickness

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
pdf("MFA_data_output/Cluster_analysis/PCA_glasshouse_climate_all.pdf") # Create a new pdf device
print(var.plot)
print(biplot)
print(ind.plot)
print(cluster)
dev.off() # Close the pdf device

#################################### subsetting the data for only the trees and shrubs

other.variables <- read.csv("MFA_data/FAMD_data.csv")

hugh.data <- read.csv("MFA_data/niche.data.HB.csv")

hugh.data <- hugh.data %>%
  select(searchTaxon, Annual_precip_mean, Precip_dry_qu_mean) %>% # selecting the relevant variables
  rename(Species=searchTaxon)

all.data <- left_join(other.variables, hugh.data, by = "Species")

all.data <- filter(all.data, Growth_Form == "Tree" | Growth_Form == "Shrub") # 92 native and exotic trees and shrubs

library(tidyverse)
library("FactoMineR")
library("factoextra")

# need to transform the succulance variable to make it linear
PCA_analysis <- all.data %>%
  mutate(log_mean_succulance_g = log(mean_succulance_g)) %>%
  filter(Species_Code != "Phro") %>% # remove Phro because it has NAs for climate variables
  select(-mean_succulance_g) %>%
  remove_rownames %>%
  column_to_rownames(var="Species_Code") # make species code the row names

# remove the categorical vaiables

PCA_analysis_cont <- select(PCA_analysis, 9:14)

# doing the PCA

res.pca <- PCA(PCA_analysis_cont, graph = FALSE)

print(res.pca)

# looking at the eigenvalues

eig.val <- get_eigenvalue(res.pca)
eig.val
# first three PCs explain 77% of the data

# let's look at the scree plot

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 60))

# let's look at the variables

var <- get_pca_var(res.pca)
var

# plot the variables

var.plot <- fviz_pca_var(res.pca, col.var = "black", repel = TRUE)
var.plot

# colour variables on the correlation circle by their contrib value -> contribution of the variable on the factor map

fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), # specify custom gradient
             repel = TRUE) # avoid text overlap

# Contributions of variables to PC1

fviz_contrib(res.pca, choice = "var", axes = 1, top = 4) # red dashed line is the expected average contribution (i.e. 4 variables = 25%)

# Contributions of variables to PC2

fviz_contrib(res.pca, choice = "var", axes = 2, top = 4)

# dimension description
# identify the most significanlty associated variables for a given PC

res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1 # LMA, thickness, annual precip
# Description of dimension 2
res.desc$Dim.2 # succulence

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

biplot # not really telling us anything

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
                        main = "PCA - trees and shrubs")

cluster

# Let's look at the HCPC output

# display the original data with a new column indicating which cluster they belong to

head(res.hcpc$data.clust)

# display the qualtitative variables that explain the most variance in each cluster

res.hcpc$desc.var$quanti

# principle dimensions that are most associated with clusters
res.hcpc$desc.axes$quanti

# save the output
pdf("MFA_data_output/Cluster_analysis/PCA_glasshouse_climate_trees_shrubs.pdf") # Create a new pdf device
print(var.plot)
print(biplot)
print(ind.plot)
print(cluster)
dev.off() # Close the pdf device

######################################################################################################################################
################################### PCA on glasshouse variables and selected environmental variables #################################
######################################################## plus leaf area ##############################################################
######################################################################################################################################

other.variables <- read.csv("MFA_data/FAMD_data2.csv")

hugh.data <- read.csv("MFA_data/niche.data.HB.csv")

hugh.data <- hugh.data %>%
  select(searchTaxon, Annual_precip_mean, Precip_dry_qu_mean) %>% # selecting the relevant variables
  rename(Species=searchTaxon)

all.data <- left_join(other.variables, hugh.data, by = "Species")

all.data <- filter(all.data, Species_Code != "Phro", Species_Code != "Kebe", Species_Code != "Atmo", Species_Code != "Pich", 
                     Species_Code != "Ulpa", Species_Code != "Crla", Species_Code != "Crma", Species_Code != "Atfi",
                     Species_Code != "Brru") # filter out species with missing climate or leaf area

library(tidyverse)
library("FactoMineR")
library("factoextra")

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
# first 3 PCs explain 74% of the data

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
pdf("MFA_data_output/Cluster_analysis/PCA_glasshouse_climate_leafarea_all.pdf") # Create a new pdf device
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
cluster

# Let's look at the HCPC output

# display the original data with a new column indicating which cluster they belong to

head(res.hcpc$data.clust)

# display the qualtitative variables that explain the most variance in each cluster

res.hcpc$desc.var$quanti

# principle dimensions that are most associated with clusters
res.hcpc$desc.axes$quanti

# save the output
pdf("MFA_data_output/Cluster_analysis/PCA_glasshouse_climate_leafarea_noconsol_all.pdf") # Create a new pdf device
print(var.plot)
print(biplot)
print(ind.plot)
print(cluster)
dev.off() # Close the pdf device

#################################### subsetting the data for only the trees and shrubs

other.variables <- read.csv("MFA_data/FAMD_data2.csv")

hugh.data <- read.csv("MFA_data/niche.data.HB.csv")

hugh.data <- hugh.data %>%
  select(searchTaxon, Annual_precip_mean, Precip_dry_qu_mean) %>% # selecting the relevant variables
  rename(Species=searchTaxon)

all.data <- left_join(other.variables, hugh.data, by = "Species")

all.data <- filter(all.data, Growth_Form == "Tree" | Growth_Form == "Shrub") # 92 native and exotic trees and shrubs

all.data <- filter(all.data, Species_Code != "Atmo", Species_Code != "Atfi", Species_Code != "Brru", Species_Code != "Crma", Species_Code != "Crla", Species_Code != "Pich",
                   Species_Code != "Phro", Species_Code != "Ulpa")

library(tidyverse)
library("FactoMineR")
library("factoextra")

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
# first three PCs explain 74% of the data

# let's look at the scree plot

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 60))

# let's look at the variables

var <- get_pca_var(res.pca)
var

# plot the variables

var.plot <- fviz_pca_var(res.pca, col.var = "black", repel = TRUE)
var.plot

# colour variables on the correlation circle by their contrib value -> contribution of the variable on the factor map

fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), # specify custom gradient
             repel = TRUE) # avoid text overlap

# Contributions of variables to PC1

fviz_contrib(res.pca, choice = "var", axes = 1, top = 4) # red dashed line is the expected average contribution (i.e. 4 variables = 25%)

# Contributions of variables to PC2

fviz_contrib(res.pca, choice = "var", axes = 2, top = 4)

# dimension description
# identify the most significanlty associated variables for a given PC

res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1 # Leaf area, succulence, annual precip
# Description of dimension 2
res.desc$Dim.2 # thickness, LMA

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

biplot # not really telling us anything

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
                        main = "PCA - trees and shrubs")

cluster

# Let's look at the HCPC output

# display the original data with a new column indicating which cluster they belong to

head(res.hcpc$data.clust)

# display the qualtitative variables that explain the most variance in each cluster

res.hcpc$desc.var$quanti

# principle dimensions that are most associated with clusters
res.hcpc$desc.axes$quanti

# save the output
pdf("MFA_data_output/Cluster_analysis/PCA_glasshouse_climate_leafarea_trees_shrubs.pdf") # Create a new pdf device
print(var.plot)
print(biplot)
print(ind.plot)
print(cluster)
dev.off() # Close the pdf device

######################################################################################################################################
################################### FAMD on glasshouse variables, selected environmental variables ###################################
######################################################## plus leaf area ##############################################################
################################################# and drought deciduousness ##########################################################
######################################################################################################################################

other.variables <- read.csv("MFA_data/FAMD_data2.csv")

hugh.data <- read.csv("MFA_data/niche.data.HB.csv")

hugh.data <- hugh.data %>%
  select(searchTaxon, Annual_precip_mean, Precip_dry_qu_mean) %>% # selecting the relevant variables
  rename(Species=searchTaxon)

all.data <- left_join(other.variables, hugh.data, by = "Species")

all.data <- filter(all.data, Species_Code != "Phro", Species_Code != "Kebe", Species_Code != "Atmo", Species_Code != "Pich", 
                   Species_Code != "Ulpa", Species_Code != "Crla", Species_Code != "Crma", Species_Code != "Atfi",
                   Species_Code != "Brru", Species_Code != "Mero") # filter out species with missing climate, drought deciduous or leaf area

library(tidyverse)
library("FactoMineR")
library("factoextra")

# need to transform the succulance variable and leaf area variable to make it linear
FAMD_analysis_all <- all.data %>%
  mutate(log_mean_succulence_g = log(mean_succulance_g), log_mean_leaf_area_cm2 = log(mean_leaf_area_cm2)) %>%
  select(-mean_succulance_g, -mean_leaf_area_cm2, -mean_Lobosity) %>% # need to remove lobosity
  remove_rownames %>%
  column_to_rownames(var="Species_Code") # make species code the row names

# remove the categorical vaiables

FAMD_analysis <- select(FAMD_analysis_all, -Species, -Origin, -Growth_Form, -Growth_structure, -Deciduous_or_evergreen,
                        -Water_storage_organs, -Leaf_hairs)

# running the FAMD analysis

res.famd <- FAMD(FAMD_analysis, graph = FALSE)

# extract the eigenvalues

eig.val <- get_eigenvalue(res.famd)
head(eig.val) # first 3 dimensions explain 67% of the data

# draw the scree plot
fviz_screeplot(res.famd)

## all variables

var <- get_famd_var(res.famd)

# plot of variables
fviz_famd_var(res.famd, repel = TRUE)

# contribution to the 1st dimension
fviz_contrib(res.famd, "var", axes = 1) # area, succulance

# contribution to 2nd dimension
fviz_contrib(res.famd, "var", axes = 2) # thickness, LMA, annual precip

## quantitative variables

quanti.var <- get_famd_var(res.famd, "quanti.var")

# plot the quantitative variables

quanti.plot <- fviz_famd_var(res.famd, "quanti.var", repel = TRUE, col.var = "black")
quanti.plot

## qualitative variables

quali.var <- get_famd_var(res.famd, "quali.var")

# plot the qualitative variables

quali.plot <- fviz_famd_var(res.famd, "quali.var", repel = TRUE, col.var = "black")
quali.plot

### graph of individuals

ind <- get_famd_ind(res.famd)

ind.graph <- fviz_famd_ind(res.famd, repel = TRUE, invisible = "quali.var", col.ind = "black")
ind.graph

# colour individuals by growth form (used ?fviz_famd_ind to figure out)

library(ggplot2)

individuals <- fviz_famd_ind(res.famd, 
                             geom = "point", # show points only (but not "text")
                             col.ind = all.data$Growth_Form, # color by growth form 
                             palette = c("Dark2"),
                             addEllipses = TRUE, ellipse.type = "confidence", # confidence ellipse
                             legend.title = "Growth from",
                             invisible = "quali.var")

individuals

# redo FAMD with only the first 3 dimensions

res.famd <- FAMD(FAMD_analysis, ncp = 3, graph = FALSE)

# Compute hierarchical clustering

res.hcpc <- HCPC(res.famd, graph = FALSE)

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
                        main = "Factor map")
cluster

# Let's look at the HCPC output

# display the original data with a new column indicating which cluster they belong to

head(res.hcpc$data.clust)

# display the quantitative variables that explain the most variance in each cluster

res.hcpc$desc.var$quanti

# same thing but for categorical variables
res.hcpc$desc.var$category

# principle dimensions that are most associated with clusters
res.hcpc$desc.axes$quanti

# save the output
pdf("MFA_data_output/Cluster_analysis/FAMD_scraped_glasshouse_climate_all.pdf") # Create a new pdf device
print(quanti.plot)
print(quali.plot)
print(individuals)
print(cluster)
dev.off() # Close the pdf device

# Compute hierarchical clustering WITH NO CONSOLIDATION
# https://www.youtube.com/watch?v=4XrgWmN9erg&list=PLnZgp6epRBbTsZEFXi_p6W48HhNyqwxIu&index=7

res.hcpc <- HCPC(res.famd, consol = FALSE, graph = FALSE)

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
                        main = "Factor map")
cluster

# Let's look at the HCPC output

# display the original data with a new column indicating which cluster they belong to

head(res.hcpc$data.clust)

# display the quantitative variables that explain the most variance in each cluster

res.hcpc$desc.var$quanti

# same thing but for categorical variables
res.hcpc$desc.var$category

# principle dimensions that are most associated with clusters
res.hcpc$desc.axes$quanti

# save the output
pdf("MFA_data_output/Cluster_analysis/FAMD_scraped_glasshouse_climate_noconsol_all.pdf") # Create a new pdf device
print(quanti.plot)
print(quali.plot)
print(individuals)
print(cluster)
dev.off() # Close the pdf device

#################################### subsetting the data for only the trees and shrubs

other.variables <- read.csv("MFA_data/FAMD_data2.csv")

hugh.data <- read.csv("MFA_data/niche.data.HB.csv")

hugh.data <- hugh.data %>%
  select(searchTaxon, Annual_precip_mean, Precip_dry_qu_mean) %>% # selecting the relevant variables
  rename(Species=searchTaxon)

all.data <- left_join(other.variables, hugh.data, by = "Species")

all.data <- filter(all.data, Growth_Form == "Tree" | Growth_Form == "Shrub") # 92 native and exotic trees and shrubs

all.data <- filter(all.data, Species_Code != "Atmo", Species_Code != "Atfi", Species_Code != "Brru", Species_Code != "Crma", Species_Code != "Crla", Species_Code != "Pich",
                   Species_Code != "Phro", Species_Code != "Ulpa")

library(tidyverse)
library("FactoMineR")
library("factoextra")

# need to transform the succulance variable and leaf area variable to make it linear
FAMD_analysis_all <- all.data %>%
  mutate(log_mean_succulence_g = log(mean_succulance_g), log_mean_leaf_area_cm2 = log(mean_leaf_area_cm2)) %>%
  select(-mean_succulance_g, -mean_leaf_area_cm2, -mean_Lobosity) %>% # need to remove lobosity
  remove_rownames %>%
  column_to_rownames(var="Species_Code") # make species code the row names

# remove the categorical vaiables

FAMD_analysis <- select(FAMD_analysis_all, -Species, -Origin, -Growth_Form, -Growth_structure, -Deciduous_or_evergreen,
                        -Water_storage_organs, -Leaf_hairs)

# running the FAMD analysis

res.famd <- FAMD(FAMD_analysis, graph = FALSE)

# extract the eigenvalues

eig.val <- get_eigenvalue(res.famd)
head(eig.val) # first 3 dimensions explain 68% of the data

# draw the scree plot
fviz_screeplot(res.famd)

## all variables

var <- get_famd_var(res.famd)

# plot of variables
fviz_famd_var(res.famd, repel = TRUE)

# contribution to the 1st dimension
fviz_contrib(res.famd, "var", axes = 1) # annual precip, leaf area, succulence

# contribution to 2nd dimension
fviz_contrib(res.famd, "var", axes = 2) # thickness, LMA

## quantitative variables

quanti.var <- get_famd_var(res.famd, "quanti.var")

# plot the quantitative variables

quanti.plot <- fviz_famd_var(res.famd, "quanti.var", repel = TRUE, col.var = "black")
quanti.plot

## qualitative variables

quali.var <- get_famd_var(res.famd, "quali.var")

# plot the qualitative variables

quali.plot <- fviz_famd_var(res.famd, "quali.var", repel = TRUE, col.var = "black")
quali.plot

### graph of individuals

ind <- get_famd_ind(res.famd)

ind.graph <- fviz_famd_ind(res.famd, repel = TRUE, invisible = "quali.var", col.ind = "black")
ind.graph

# colour individuals by growth form (used ?fviz_famd_ind to figure out)

library(ggplot2)

individuals <- fviz_famd_ind(res.famd, 
                             geom = "point", # show points only (but not "text")
                             col.ind = all.data$Growth_Form, # color by growth form 
                             palette = c("Dark2"),
                             addEllipses = TRUE, ellipse.type = "confidence", # confidence ellipse
                             legend.title = "Growth from",
                             invisible = "quali.var")

individuals

# redo FAMD with only the first 3 dimensions

res.famd <- FAMD(FAMD_analysis, ncp = 3, graph = FALSE)

# Compute hierarchical clustering

res.hcpc <- HCPC(res.famd, graph = FALSE)

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
                        main = "Factor map")
cluster

# Let's look at the HCPC output

# display the original data with a new column indicating which cluster they belong to

head(res.hcpc$data.clust)

# display the quantitative variables that explain the most variance in each cluster

res.hcpc$desc.var$quanti

# same thing but for categorical variables
res.hcpc$desc.var$category # keeps saying 'null' - maybe doesn't cluster according to this variable?

# principle dimensions that are most associated with clusters
res.hcpc$desc.axes$quanti

# save the output
pdf("MFA_data_output/Cluster_analysis/FAMD_scraped_glasshouse_climate_trees_shrubs.pdf") # Create a new pdf device
print(quanti.plot)
print(quali.plot)
print(individuals)
print(cluster)
dev.off() # Close the pdf device