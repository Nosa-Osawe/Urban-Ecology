
library(tidyverse)
library(readxl)


fly_site <- read_excel("C:\\Users\\DELL\\Documents\\Git in R\\Urban-Ecology\\Data\\Fly_community.xlsx",
                       sheet = "Site")

sum(is.na(fly_site))
fly_site <- replace(fly_site, is.na(fly_site), 0)
view(fly_site)
