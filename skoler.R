# ANALYSER AF SKOLER I ESBJERG KOMMUNE PÅ BAGGRUND AF SOCIALE FAKTORER
# PEER CHRISTENSEN, OKTOBER 2018

# VARIABLER (andel af..)
# k1: enlige forældre
# k2: anden etnisk herkomst, k2a -b -c svarer til landegrupper
# k3: lavtuddannede
# karaktergennemsnit , 9 kl. alle fag

# ANALYSER
# korrelation mellem

# ------------------------------------------------
### PAKKER #######################################

library(factoextra)
library(GGally)
library(tidyverse)
library(ggbiplot)
library(ggcorrplot)
library(magrittr)
library(viridis)
library(corrplot)
library(leaps)

theme_set(theme_bw())

# ------------------------------------------------
### DATA #########################################

df <- read_csv2("skoler.csv") %>%
  filter(!skole %in% c("0_5","6_16"))

df$karakter <- as.character(df$karakter)
df$karakter <- gsub('^([0-9]{1})([0-9]+)$', '\\1.\\2', df$karakter)
df$karakter <- as.numeric(df$karakter) 

df$skole = gsub("skolen|_Skole","",df$skole)

# ------------------------------------------------
### DESKRIPTIV ANALYSE ###########################

## overblik

# absolutte tal
df %>% 
  select(skole,k1,k2,k3) %>%
  gather(key   = kriterium, 
         value = antal, 
         k1,k2,k3) %>%
  ggplot(aes(x = kriterium,y=antal,fill=kriterium)) +
  geom_col() +
  scale_fill_viridis_d() +
  facet_wrap(~skole) +
  ggtitle("Absolutte elevtal for hvert kriterium pr. skole")

ggsave("abs_tal_pr_skole.png")

# procent af elevtal
df %<>% 
  mutate(k1  = k1/sum,
         k2  = k2/sum,
         k2a = k2a/sum,
         k2b = k2b/sum,
         k2c = k2c/sum,
         k3  = k3/sum,
         `0_opfyldt` = `0_opfyldt`/sum,
         `1_opfyldt` = `1_opfyldt`/sum,
         `2_opfyldt` = `2_opfyldt`/sum,
         `3_opfyldt` = `3_opfyldt`/sum) %>%
  select(-sum)

df %>% 
  gather(key = kriterium, value = andel, k1,k2,k3) %>%
  ggplot(aes(x=kriterium,y=andel,fill=kriterium)) + 
  geom_col() +
  scale_fill_viridis_d() +
  facet_wrap(~skole) +
  ggtitle("Andel af elever for hvert kriterium pr. skole")

ggsave("procent_pr_skole.png")

# anden etnisk herkomst fordeling

df %>% 
  gather(key = kriterium, value = andel, k2a,k2b,k2c) %>%
  ggplot(aes(x=kriterium,y=andel,fill=kriterium)) + 
  geom_col() +
  scale_fill_viridis_d() +
  facet_wrap(~skole) +
  ggtitle("Herkomst: andel af elever fordelt på landegrupper")

ggsave("herkomst_pr_skole.png")

# kriterier opfyldt

df %>% 
  gather(key = kriterium, value = andel, `0_opfyldt`,`1_opfyldt`,`2_opfyldt`,`3_opfyldt`) %>%
  ggplot(aes(x=kriterium,y=andel,fill=kriterium)) + 
  geom_col() +
  scale_fill_viridis_d() +
  facet_wrap(~skole) +
  ggtitle("Antal kriterier opfyldt pr. skole")

ggsave("n_kriterier_pr_skole.png")

# ------------------------------------------------
### KORRELATIONER ################################

sign_test <- df  %>% 
  select(-skole) %>%
  filter(!is.na(karakter)) %>%
  cor_pmat()

df  %>% 
  select(-skole) %>%
  filter(!is.na(karakter)) %>%
  cor() %>%
  ggcorrplot(type   = "lower", 
             p.mat  = sign_test,
             colors = c(viridis(3)[1], "white", viridis(5)[4])) +
  #scale_fill_viridis_c() +
  ggtitle("Korrelationer mellem faktorer",
          subtitle = "ikke-signifikante korrelationer markeret med X")

ggsave("korrelationer.png")

# ------------------------------------------------
### DISTANCE MATRIX ##############################

df_scale <- df %>%
  select(-skole) %>%
  filter(!is.na(karakter)) %>%
  scale() %>% 
  data.frame()
row.names(df_scale) = df$skole[!is.na(df$karakter)]

df_dist <- get_dist(df_scale, stand = TRUE, method = "pearson")
fviz_dist(df_dist,gradient = list(low = viridis(3)[1], mid = "white", high = viridis(5)[4])) +
  theme_minimal() +
  ggtitle("Distance matrix",
          subtitle = "hvor meget skolerne ligner hinanden på baggrund af samtlige faktorer")

ggsave("dist_matrix.png")

# ------------------------------------------------
### KLYNGEANALYSER ###############################

### K-means

# antal klynger
fviz_nbclust(df_scale, kmeans, method = "gap_stat")

set.seed(324789)
km.res <- kmeans(df_scale, 3, nstart = 25)

fviz_cluster(km.res, data = df_scale,
             ellipse.type = "convex",
             repel = T,
             palette = viridis(10)[c(1,4,8)],
             ggtheme = theme_minimal(),
             main = "K-means klynger") 

ggsave("k_means_klynger.png")

### H-Clustering

# Compute hierarchical clustering
res.hc <- df_scale %>%
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "ward.D2")     # Compute hierachical clustering

fviz_dend(res.hc, k = 3, # Cut in four groups
          cex = 1.5, # label size
          k_colors = viridis(10)[c(1,4,8)],
          color_labels_by_k = TRUE, 
          rect = TRUE) +
  ggtitle("Hierarkisk klyngeanalyse")

ggsave("h_klynger.png")

# ------------------------------------------------
### PCA ##########################################

df_pca <- df %>% 
  filter(!is.na(karakter)) %>%
  select(-skole) %>%
  prcomp(center = TRUE,scale = TRUE)
summary(df_pca)

ggbiplot(df_pca,
         ellipse=T,
         labels=df$skole[!is.na(df$karakter)], 
         groups = factor(km.res$cluster)) +
  scale_colour_manual(values = viridis(10)[c(1,4,8)]) +
  xlim(c(-2,3.2)) +
  ggtitle("Principal components analyse")


eig <-get_eigenvalue(df_pca)
format(eig,digits=5)
fviz_eig(df_pca, addlabels = TRUE, ylim = c(0, 50))

var <- get_pca_var(df_pca)
var

library(coorplot)
corrplot(var$cos2, is.corr=FALSE)
corrplot(var$contrib, is.corr=FALSE)  

# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(df_pca, choice = "var", axes = 1:2)

fviz_contrib(df_pca, choice = "var", axes = 1, top = 10)
fviz_contrib(df_pca, choice = "var", axes = 2, top = 10)

library(FactoMineR)
df_pca2 <- PCA(df_scale, graph = FALSE)
pca_desc <- dimdesc(df_pca2, axes = c(1,2), proba = 0.05)
# Description of dimension 1,2
pca_desc$Dim.1
pca_desc$Dim.2

fviz_contrib(df_pca2, choice = "ind", axes = 1:2)


fviz_pca_ind(df_pca2, 
             col.ind = factor(km.res$cluster), 
             palette=viridis(9)[c(2,5,8)],
             repel = TRUE,
             addEllipses = TRUE, # for få punkter i gr. 3
             legend.title="Gruppe")

fviz_pca_biplot(df_pca2, 
             fill.ind = factor(km.res$cluster), 
             col.ind = factor(km.res$cluster),
             palette=viridis(9)[c(2,5,8)],
             repel = TRUE,
             addEllipses = TRUE,
             #ellipse.type="confidence", # for få punkter i gr. 3
             legend.title="Gruppe",
             alpha.var="contrib",
             col.var = "gray47",
             title=NULL)
ggsave("pca.png")

fviz_pca_biplot(iris.pca, 
                fill.ind = iris$Species,
                pointshape = 2,
                addEllipses = TRUE,
                # Variables
                alpha.var ="contrib", col.var = "contrib" )

# circular

# ------------------------------------------------
### REGRESSION ###################################

df_regr <- df %>% select(-skole)

# best subsets
models <- regsubsets(karakter~k1+k2+k3, data = df_regr, nvmax = 3)
summary(models)

res.sum <- summary(models)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)

plot(models,scale="adjr2",col = viridis(10)[3:9])

fit <- lm(karakter~k2,data=df_regr)
summary(fit)
summ(fit)

df %>% 
  ggplot(aes(x=k2,y=karakter)) +
  geom_point(size = 5,alpha=.8) +
  geom_smooth(method="lm", colour = viridis(10)[10], fill = viridis(1)) +
  scale_fill_viridis_d()
