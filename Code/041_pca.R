# 041_pca.R
# Michael Boerman January 2021

# experiment with pca on the categories instead of aggreagting.

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

# for nice pc plots
library(factoextra)

cat_data <- read_csv(here("Results/csv/pop_std_scores_states.csv")) %>% 
  pivot_wider(names_from = Category,
              values_from = weighted_standardized_score) %>% 
  select(-c(population, scores, standardized_score)) %>% 
  identity()

pca_results <- prcomp(~., data = select_if(cat_data, is.numeric),
                                 na.action = na.omit,
                                 scale = FALSE
                      )

prcomp(~., data = cat_data[, 3:10], na.action = )


fviz_eig(pca_results)     
fviz_pca_var(pca_results, col.var = "contrib")
get_eigenvalue(pca_results)


pca_var <- get_pca_var(pca_results)
pca_var$coord          # Coordinates
pca_var$contrib        # Contributions to the PCs
pca_var$cos2           # Quality of representation 

# Results for individuals
pca_ind <- get_pca_ind(pca_results)
pca_ind$coord          # Coordinates
head(pca_ind$contrib)  # Contributions to the PCs
pca_ind$cos2           # Quality of representation 

first_pc <- pca_results$x[,1] %>%
  as.data.frame() %>% 
  dplyr::rename(first_pc = '.') %>% 
  mutate(period = seq(1:length(pca_results$x[,1]))) %>% 
  identity()

first_pc %>% 
  ggplot(aes(x=period, y = first_pc)) +
  geom_line()
