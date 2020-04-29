
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
pdf("PCA_Cluster_output/glasshouse_no_climate_LDMC_TLP.pdf") # Create a new pdf device
print(var.plot)
print(ind.plot)
print(cluster)
dev.off() # Close the pdf device
# hasn't changed, looks the same as when osmpot wasn't transformed therefore didn't redo ladderplot etc, also doesn't look different by adding Brru and Atfi
