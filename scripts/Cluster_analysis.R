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

# we want to look at how much variation is explained by each analysis and where an additive model is better
# 'can soft traits be used to predict drought tolerance of plants?'



