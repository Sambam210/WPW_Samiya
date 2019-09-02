# Links

# https://rstudio-pubs-static.s3.amazonaws.com/423873_adfdb38bce8d47579f6dc916dd67ae75.html
# https://www.datanovia.com/en/blog/cluster-analysis-in-r-simplified-and-enhanced/
# https://medium.com/@rumman1988/clustering-categorical-and-numerical-datatype-using-gower-distance-ab89b3aa90d9

# try this first
# cluser analysis after 
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/117-hcpc-hierarchical-clustering-on-principal-components-essentials/

library(tidyverse)

# Load the data, this includes the glasshouse variables + my scrapped variables (I have modified them to be more informative)
FAMD_data <- read.csv("MFA_data/FAMD_data.csv")

# need to transform the succulance variable to make it linear
FAMD_data <- FAMD_data %>%
  mutate(log_mean_succulance_g = log(mean_succulance_g)) %>%
  select(-mean_succulance_g) %>%
  remove_rownames %>%
  column_to_rownames(var="Species_Code") # make species code the row names

# remove the categorical vaiables that aren't needed/used to colour points later

FAMD_data_analysis <- select(FAMD_data, -Species, -Origin, -Growth_Form)

library("FactoMineR")
library("factoextra")

str(FAMD_data_analysis)

# running the FAMD analysis

res.famd <- FAMD(FAMD_data_analysis, graph = FALSE)

# Compute hierarchical clustering
res.hcpc <- HCPC(res.famd, graph = FALSE)

# dendrogram
fviz_dend(res.hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8      # Augment the room for labels
)

# graph
fviz_cluster(res.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)

