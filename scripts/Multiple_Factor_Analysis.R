# Following a tutorial for MFA
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/116-mfa-multiple-factor-analysis-in-r-essentials/

install.packages(c("FactoMineR", "factoextra"))

library("FactoMineR")
library("factoextra")

# using the wine dataset in FactoMineR

data(wine)
colnames(wine)
wine
str(wine)

# first two columns are categorial and the next 29 columns are continous

res.mfa <- MFA(wine, # the dataset
               group = c(2, 5, 3, 10, 9, 2), # defining the groups
               type = c("n", "s", "s", "s", "s", "s"), # s = standardise continuous variables, n = categorical variables
               name.group = c("origin","odor","visual",
                              "odor.after.shaking", "taste","overall"), # name the groups
               num.group.sup = c(1, 6), # identify the groups you want to exclude from analysis, in this case origin (group 1) and overall judgement (group 6) were excluded
               graph = FALSE)

print(res.mfa)

### to visualise and interpret the results use the factoextra package

# eigenvalues/variances
# extract the proportion of variances retained by the different dimensions

eig.val <- get_eigenvalue(res.mfa)
head(eig.val)

# draw the scree plot

fviz_screeplot(res.mfa)

# graph of variables

# groups of variables

group <- get_mfa_var(res.mfa, "group") # extract the results for groups of variables
group

# plot the groups of variables

fviz_mfa_var(res.mfa, "group") # red = active groups of variables, green = supplementary groups of variables

# draw a bar plot of group contributions to the dimensions

# Contribution to the first dimension
fviz_contrib(res.mfa, "group", axes = 1)
# Contribution to the second dimension
fviz_contrib(res.mfa, "group", axes = 2)

# extract results of qualtitative variables

quanti.var <- get_mfa_var(res.mfa, "quanti.var")
quanti.var 





# I think if there is no groupings in the data then you should use FAMD 
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/115-famd-factor-analysis-of-mixed-data-in-r-essentials/
# I think MFA is a type of AFDM for when there is groupings/structure in the data
# https://stats.stackexchange.com/questions/5774/can-principal-component-analysis-be-applied-to-datasets-containing-a-mix-of-cont
# also check out all the other types of multivariate analyses
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/


#######################################################################################################################################################################
############################################################ Practice PCA analysis ####################################################################################
#######################################################################################################################################################################

# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

# load my practise dataset, this is a subset of the variables I have (only the physiological stuff) + corrected growth form

practise_PCA <- read.csv("MFA_data/PCA_practise_GH_variables.csv")

# FIRST STEP IN PCA ANALYSIS
# NEED TO CONVERT SPECIES CODE INTO ROW NAMES
# https://stackoverflow.com/questions/5555408/convert-the-values-in-a-column-into-row-names-in-an-existing-data-frame-in-r

library(tidyverse)

practise_PCA <- practise_PCA %>%
  remove_rownames %>%
  column_to_rownames(var="Species_Code")

library("FactoMineR")
library("factoextra")

# subset the data to remove the categorical variables, these will be used later to colour by groups

practise_PCA_cont_var <- select(practise_PCA, -Species, -Origin, -Growth_Form, -Woody)

## scaling the data
# data need to be in the same scale before analysis
# PCA() in FActoMineR does this automatically

# doing the PCA

res.pca <- PCA(practise_PCA_cont_var, graph = FALSE)

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

fviz_pca_var(res.pca, col.var = "black")

# quality of representation

install.packages("corrplot")
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)
# think this means that OsmPot is correlated with Dim2 while succulance, thickness and LMA is correlated with Dim1

# colour variables on the correlation circle by their cos2 value -> quality of the variable on the factor map

fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), # specify custom gradient
             repel = TRUE # avoid text overlap
             )

# change the transparecny of variables according to the cos2 value

fviz_pca_var(res.pca, alpha.var = "cos2")

# contributions of variables to PCs

corrplot(var$contrib, is.corr=FALSE)

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
res.desc$Dim.1
# Description of dimension 2
res.desc$Dim.2

# graph of individuals

ind <- get_pca_ind(res.pca)
ind

# plot the individuals

fviz_pca_ind(res.pca, repel = TRUE)

# colour individuals bt their cos2 value

fviz_pca_ind(res.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

# colour individuals by a custom continuous variable
# let's try mean aridity index

og_data <- read.csv("Data_output/SamiyaandReneeandHugh.csv")
# extract the AI_mean column
AI_mean <- select(og_data, AI_mean)
# colour individuals by mean AI
fviz_pca_ind(res.pca, col.ind = AI_mean,
             gradient.cols = c("blue", "yellow", "red"),
             legend.title = "AI_mean") # DOESN'T WORK!!!!

# try the example

# Create a random continuous variable of length 23,
# Same length as the number of active individuals in the PCA
set.seed(123)
my.cont.var <- rnorm(113)
# Color individuals by the continuous variable
fviz_pca_ind(res.pca, col.ind = my.cont.var,
             gradient.cols = c("blue", "yellow", "red"),
             legend.title = "Cont.Var") # WORKS - I THINK IT'S BECASUE THERE ARE NAS IN THE DATASET FOR AI_MEAN

# colour by groups

fviz_pca_ind(res.pca,
             geom.ind = "point", # show points only (but not "text")
             col.ind = practise_PCA$Growth_Form, # color by growth form
             palette = c("Dark2"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Growth form")

fviz_pca_ind(res.pca,
             geom.ind = "point", # show points only (but not "text")
             col.ind = practise_PCA$Origin, # color by origin
             palette = c("Dark2"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Origin")

fviz_pca_ind(res.pca,
             geom.ind = "point", # show points only (but not "text")
             col.ind = practise_PCA$Woody, # color by origin
             palette = c("Dark2"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Woody")

# confidence ellipses (prob better)
# shows 95% confidence ellipse, see ?coord.ellipse()

fviz_pca_ind(res.pca,
             geom.ind = "point", # show points only (but not "text")
             col.ind = practise_PCA$Growth_Form, # color by growth form
             palette = c("Dark2"),
             addEllipses = TRUE, ellipse.type = "confidence", # confidence ellipses
             legend.title = "Growth form") # grasses look sign different from everything else

fviz_pca_ind(res.pca,
             geom.ind = "point", # show points only (but not "text")
             col.ind = practise_PCA$Origin, # color by origin
             palette = c("Dark2"),
             addEllipses = TRUE, ellipse.type = "confidence", # confidence ellipses
             legend.title = "Origin") # doesn't look different


# biplot

fviz_pca_biplot(res.pca, 
                col.ind = practise_PCA$Growth_Form, palette = "jco", 
                addEllipses = TRUE, ellipse.type = "confidence", label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Growth Form")

######################################################################################################################################################
######################################## Let's try a PCA with all Hugh's environmental variables #####################################################
######################################################################################################################################################

practise_PCA <- read.csv("MFA_data/PCA_practise_GH_variables.csv")

library(tidyverse)

# adding Hugh's data
Hugh_data <- read.csv("MFA_data/niche.data.HB.csv")

# getting rid of column I don't need and renaming the species column to match mine
Hugh_data <- Hugh_data %>%
  select(-searchTaxon.20, -AOO, -Global_records) %>%
  rename(Species=searchTaxon)

# visualise correlations between variables

Hugh_data_cor <- select(Hugh_data, -Species) # need to remove the categorical variable first

str(Hugh_data_cor)

library("corrplot")

cor.mat <- round(cor(Hugh_data_cor),2)
head(cor.mat[, 1:6])

corrplot(cor.mat, type="upper", order="hclust", 
         tl.col="black", tl.srt=45) # HOLY COW THERE IS TOO MANY VARIABLES TO SEE THE PLOT!

## LET'S TRY AND SUBSET HUGH'S DATA TO ONLY INCLUDE THE MEANS AND RANGES FOR EACH WORDCLIM VARIABLE

Hugh_data_short <- Hugh_data %>%
  select(ends_with("mean"), ends_with("range"))

# need to join the species column back
Hugh_data_short <- cbin(Hugh_data$Species, Hugh_data_short)

# joining my data with Hugh's data
practise_PCA_enviro <- left_join(practise_PCA, Hugh_data_short, by = "Species")

# making species code the row name
practise_PCA_enviro <- practise_PCA_enviro %>%
  remove_rownames %>%
  column_to_rownames(var="Species_Code")

library("FactoMineR")
library("factoextra")

# subset the data to remove the categorical variables, these will be used later to colour by groups

practise_PCA_enviro_cont_var <- select(practise_PCA_enviro, -Species, -Origin, -Growth_Form, -Woody)
