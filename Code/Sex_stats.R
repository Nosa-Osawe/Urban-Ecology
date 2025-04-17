library(vegan)
library(tidyverse)
library(readxl)
library(pairwiseAdonis)
library(ggtext)
library(ggforce)


sex_fly <- read_excel("C:\\Users\\DELL\\Documents\\Git in R\\Urban-Ecology\\Data\\Fly_community.xlsx",
                       sheet = "By_sex")

sum(is.na(sex_fly))
sex_fly <- replace(sex_fly, is.na(sex_fly), 0) %>% 
  rename(Method="Collection Method")
# view(sex_fly)

sex_fly <- sex_fly %>% 
  mutate(Longitude=  as.numeric(str_remove(Longitude, "°N")),
         Latitude = as.numeric(str_remove(Latitude, "°E")))

head(sex_fly, 10)

summary_sex <- sex_fly %>% 
  filter(Collector=="Tavern") %>% 
  select(-c(1:4)) %>% 
  pivot_longer(
    cols = -c(1,2),
    names_to = "Species",
    values_to = "Count"
  ) %>%
  mutate(Count = as.numeric(Count)) %>% 
  group_by(Method, Species, Sex) %>%
  summarise(Count = sum(Count)) %>%
  pivot_wider(
    names_from = Sex,
    values_from = Count) %>% 
  as.data.frame()

summary_sex %>% 
  filter(Species=="Drosophila spp.") %>% 
  select(-Species) %>%
  column_to_rownames("Method") %>%
  fisher.test()

summary_sex %>% 
  filter(Species=="Fannia canicularis") %>% 
  select(-Species) %>%
  column_to_rownames("Method") %>%
  fisher.test()


summary_sex %>% 
  filter(Species=="Musca domestica") %>% 
  select(-Species) %>%
  column_to_rownames("Method") %>%
  fisher.test()

summary_sex %>% 
  filter(Species=="Muscina spp.") %>% 
  select(-Species) %>%
  column_to_rownames("Method") %>%
  fisher.test()

summary_sex %>% 
  filter(Species=="Phaenicia cuprina") %>% 
  select(-Species) %>%
  column_to_rownames("Method") %>%
  fisher.test()

summary_sex %>% 
  filter(Species=="Phaenicia sericata") %>% 
  select(-Species) %>%
  column_to_rownames("Method") %>%
  fisher.test()

summary_sex %>% 
  filter(Species=="Sarcophaga spp.") %>% 
  select(-Species) %>%
  column_to_rownames("Method") %>%
  fisher.test()
