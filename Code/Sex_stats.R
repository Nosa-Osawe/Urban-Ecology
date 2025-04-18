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
  filter(Food_site=="Tavern") %>% 
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
 # t() %>% 
  fisher.test()

summary_sex %>% 
  filter(Species=="Sarcophaga spp.") %>% 
  select(-Species) %>%
  column_to_rownames("Method") %>%
  fisher.test()

summary_sex %>%
  filter(Species == "Drosophila spp." & Method=="Bottle trap") %>% 
  select(-Method) %>%
  column_to_rownames("Species") %>%
  chisq.test() 

# CHi-Square goodness of fit:
summary_sex %>%
  filter(Species == "Drosophila spp." & Method=="Bottle trap") %>% 
  select(-Method) %>%
  column_to_rownames("Species") %>%
  chisq.test()#X-squared = 2, df = 1, p-value = 0.1573



#----cannot compute for Fannia canicularis

summary_sex %>%
  filter(Species == "Musca domestica" & Method=="Bottle trap") %>% 
  select(-Method) %>%
  column_to_rownames("Species") %>%
  chisq.test()  #X-squared = 19.762, df = 1, p-value = 8.769e-06


summary_sex %>%
  filter(Species == "Muscina spp." & Method=="Bottle trap") %>% 
  select(-Method) %>%
  column_to_rownames("Species") %>%
  chisq.test() # X-squared = 0.66667, df = 1, p-value = 0.4142

summary_sex %>%
  filter(Species == "Phaenicia cuprina" & Method=="Bottle trap") %>% 
  select(-Method) %>%
  column_to_rownames("Species") %>%
  chisq.test() # X-squared = 0.4, df = 1, p-value = 0.5271

summary_sex %>%
  filter(Species == "Phaenicia sericata" & Method=="Bottle trap") %>% 
  select(-Method) %>%
  column_to_rownames("Species") %>%
  chisq.test() #X-squared = 1.6667, df = 1, p-value = 0.1967

summary_sex %>%
  filter(Species == "Sarcophaga spp." & Method=="Bottle trap") %>% 
  select(-Method) %>%
  column_to_rownames("Species") %>%
  chisq.test() # X-squared = 1, df = 1, p-value = 0.3173



summary_sex %>%
  filter(Species == "Drosophila spp." & Method=="Bottle trap") %>% 
  select(-Method) %>%
  column_to_rownames("Species") %>%
  chisq.test()


#----cannot compute for Fannia canicularis

summary_sex %>%
  filter(Species == "Musca domestica" & Method=="Sweepnet") %>% 
  select(-Method) %>%
  column_to_rownames("Species") %>%
  chisq.test() # X-squared = 38.297, df = 1, p-value = 6.076e-10


  #----cannot compute for Muscina spp.

summary_sex %>%
  filter(Species == "Phaenicia cuprina" & Method=="Sweepnet") %>% 
  select(-Method) %>%
  column_to_rownames("Species") %>%
  chisq.test() # X-squared = 0.33333, df = 1, p-value = 0.5637

summary_sex %>%
  filter(Species == "Phaenicia sericata" & Method=="Sweepnet") %>% 
  select(-Method) %>%
  column_to_rownames("Species") %>%
  chisq.test() # X-squared = 0.54545, df = 1, p-value = 0.4602

summary_sex %>%
  filter(Species == "Sarcophaga spp." & Method=="Sweepnet") %>% 
  select(-Method) %>%
  column_to_rownames("Species") %>%
  chisq.test() # X-squared = 0, df = 1, p-value = 1

summary_sex %>%
  filter(Species == "Drosophila spp." & Method=="Sweepnet") %>% 
  select(-Method) %>%
  column_to_rownames("Species") %>%
  chisq.test()

