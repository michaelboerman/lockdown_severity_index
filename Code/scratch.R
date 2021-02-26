# scratch

test <- weighted_std_scores %>% 
  group_by(date) %>% 
  summarize(index = sum(weighted_standardized_score, na.rm = TRUE)) %>% 
  mutate(index = rescale(index, c(0,100)))

plot(test)

test <- pop_weighted_scores %>% 
  filter(state == "AL")

plot(test$weighted_score)  
