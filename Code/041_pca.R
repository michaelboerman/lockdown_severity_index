# 041_pca.R
# Michael Boerman January 2021

# experiment with pca on the categories instead of aggreagting.

# for nice pc plots
library(factoextra)

cat_data <- read_csv(here("Intermediate_Data/cat_data_reordered.csv"),
  col_types = cols(
    .default = col_double(),      # coerce to be numbers instead of logical if has NA
    Date = col_date(format = ""), # except column Date, read as date.
    Location = col_character()    # except column state, read as chr
  )
) %>% 
  select(-Emergency_Declaration) %>% 
  # mutate_all(~replace(is.na(select_if(is.numeric)), 0)) %>% 
  identity()

apply

pca_results <- prcomp(~., data = select_if(cat_data, is.numeric),
                                 # na.action = na.omit, 
                                 scale = FALSE)


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

first_pc <- pca_results$rotation[,1] %>%
  as.data.frame() %>% 
  dplyr::rename(first_pc = '.') %>% 
  mutate(period = seq(1:length(pca_ind$contrib[,1]))) %>% 
  identity()

first_pc %>% 
  ggplot(aes(x=period, y = first_pc)) +
  geom_line()
