
library(tidyverse)
library(readxl)
library(factoextra)
library(mice)

df <- read_excel("Frida20190612en.xlsx",sheet = 2)

df <- df %>% 
  select(-Number)

df_scaled <- df %>%
  select(-Name) %>%
  scale() %>%
  data.frame() 

imp <- mice(df_scaled, m = 1)

df_scaled <- complete(imp)

# remove constant (NA) vars
df_scaled <- df_scaled %>%
  select_if(function(x) !all(is.na(x))) %>%
  data.frame()

# deal with duplicated row names

new_names <- df$Name

rownames(df_scaled) <- paste0(str_trunc(new_names, 13),
                              seq_along(df$Name))

fviz_nbclust(df_scaled, kmeans,method="wss")

km.res <- kmeans(df_scaled, 3, nstart = 25)

fviz_cluster(km.res, data = df_scaled,
             ellipse.type = "convex",
             repel = F,
             geom="text",
             labelsize = 6,
             ggtheme = theme_minimal(),
             main = "K-means Clustering") 

# correlation
df_scaled %>%
  cor() %>%
  ggcorrplot::ggcorrplot(type = "lower")


# dbscan
library(fpc)

set.seed(123)

dbscan::kNNdistplot(df_scaled, k =  5)
abline(h = 0.15, lty = 2)

db <- fpc::dbscan(df_scaled, eps = 7, MinPts = 5)
# Plot DBSCAN results
#plot(db, df_scaled, main = "DBSCAN", frame = FALSE)

fviz_cluster(db, df_scaled, stand = FALSE, geom = "point")

### distance matrix

df_dist <- get_dist(df_scaled, stand = TRUE, method = "pearson")
fviz_dist(df_dist,gradient = list(low = "steelblue", mid = "white", high = "indianred"),show_labels =F) +
  theme_minimal()
