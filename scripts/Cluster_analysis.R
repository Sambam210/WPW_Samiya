# Links

# https://rstudio-pubs-static.s3.amazonaws.com/423873_adfdb38bce8d47579f6dc916dd67ae75.html
# https://www.datanovia.com/en/blog/cluster-analysis-in-r-simplified-and-enhanced/
# https://medium.com/@rumman1988/clustering-categorical-and-numerical-datatype-using-gower-distance-ab89b3aa90d9

# try this first
# cluser analysis after 

############################################################################################################################################################################
################################################## Hierarchical CLustering after FAMD analysis #############################################################################
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

res.famd <- FAMD(FAMD_data_analysis, graph = FALSE) # changed the number of principle components to 4 as the 5th conmponent doesn't seem to add much more explanantion. The default ncp for FAMD analysis is 5.

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

# display the qualtitative variables that explain the most variance in each cluster

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

# same thing but for categorical variables
res.hcpc$desc.var$category

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



####################################### 1. MCA and cluster analysis on scraped variables

# 
