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
# esp PCA analysis
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/



# FIRST STEP IN PCA ANALYSIS
# NEED TO CONVERT SPECIES CODE INTO ROW NAMES
# https://stackoverflow.com/questions/5555408/convert-the-values-in-a-column-into-row-names-in-an-existing-data-frame-in-r
