library(tidyverse)




### load data 

nutrients <- readr::read_csv(file = "./raw/FoodData_Central_csv_2020-10-30/nutrient.csv")
nutrients <- nutrients %>% 
  dplyr::rename("nutrient_id" = id, 
                "nutrient_name" = name, 
                "nutrient_unit" = unit_name) %>% 
  select(1:3)

data <- readr::read_csv(file = "./raw/FoodData_Central_csv_2020-10-30/food_nutrient.csv", 
                        col_types = "dddddddddcd")



### filter data

# 1: select foods with amino acid measurements
foods_with_aa <- 
  data %>% 
  filter(nutrient_id %in% c(1210:1233)) %>% 
  pull(fdc_id) %>% 
  unique

# 2: select only those foods and keep only total protein and amino acid measurements
data <- data %>% 
  filter(fdc_id %in% foods_with_aa) %>% 
  filter(nutrient_id %in% c(1003, c(1210:1233)))
rm(foods_with_aa)



### wrangle 

# add nutrient name

data <- full_join(data, nutrients)



data %>% 
  filter(nutrient_name %in% c("Protein", "Tryptophan")) %>% 
  filter(amount < 1000) %>% 
  select(fdc_id, nutrient_name, amount) %>%
  pivot_wider(names_from = nutrient_name, values_from = amount) %>%
  mutate(Ratio = Tryptophan/Protein) %>% 
  filter(Ratio < 0.06) %>% 
  ggplot(aes(Ratio)) +
  geom_histogram(bins = 200)


glu <- data %>% 
  filter(nutrient_id %in% c(1003, 1224)) %>%
  filter(amount < 8000) %>% 
  select(fdc_id, nutrient_id, amount) %>% 
  pivot_wider(names_from = nutrient_id, values_from = amount) %>% 
  dplyr::rename(protein_total = `1003`,
                glutamate = `1224`) %>% 
  mutate(ratio = glutamate/protein_total)

glu %>% 
  ggplot(aes(ratio)) +
  geom_histogram(bins = 200)

glu %>% 
  mutate(high = ifelse(ratio > 0.25, "high", "low")) %>% 
  ggplot(aes(protein_total, glutamate)) +
  geom_point(aes(color = high))



sub <- 
  data %>% 
  filter(nutrient_name %in% c("Protein", "Tryptophan", "Tyrosine", "Glutamic acid")) %>% 
  select(fdc_id, amount, nutrient_name) %>% 
  mutate(nutrient_name = ifelse(nutrient_name == "Glutamic acid", "Glutamate", nutrient_name)) %>% 
  pivot_wider(names_from = nutrient_name, values_from = amount) %>% 
  rename(try = "Tryptophan", 
         tyr = "Tyrosine", 
         glu = "Glutamate", 
         prot = "Protein") %>% 
  mutate (tyr_ratio = tyr/prot, 
          glu_ratio = glu/prot, 
          try_ratio = try/prot) %>% 
  select(glu, glu_ratio, try, try_ratio, tyr, tyr_ratio, prot) %>% 
  filter(glu < 8000)
  
sub %>% 
  filter(try_ratio < 0.05) %>% 
  ggplot(aes(glu_ratio, tyr_ratio)) +
  geom_point(aes(colour = try_ratio))

sub <- sub[is.finite(rowSums(sub)), ]  




library(ggtern)
sub %>% 
  filter(try_ratio < 0.05) %>%
  mutate(glu_ratio = zscore(glu_ratio), 
         tyr_ratio = zscore(tyr_ratio), 
         try_ratio = zscore(try_ratio)) %>% 
  ggplot(aes(tyr_ratio, try_ratio)) +
  geom_point()

  
  
zscore <- function(vector){
  m = mean(vector, na.rm = TRUE)
  sd = sd(vector, na.rm = TRUE)
  res = (vector - m)/sd
  return(res)
}

mean(sub$glu_ratio, na.rm = TRUE)

