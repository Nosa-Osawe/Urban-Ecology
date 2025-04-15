library(vegan)
library(tidyverse)
library(readxl)


fly_site <- read_excel("C:\\Users\\DELL\\Documents\\Git in R\\Urban-Ecology\\Data\\Fly_community.xlsx",
                       sheet = "Site")

sum(is.na(fly_site))
fly_site <- replace(fly_site, is.na(fly_site), 0)
# view(fly_site)

# to remove "°N" and "°E"
fly_site <- fly_site %>% 
  mutate(Longitude=  as.numeric(str_remove(Longitude, "°N")),
         Latitude = as.numeric(str_remove(Latitude, "°E")))

head(fly_site, 10)

#    PREP for MENTEL TESTS
fly_nmds <- fly_site[, 7:14]
fly_coor <- fly_site[, 3:4]

# Converting the data to presence-absence data
fly_pa <- fly_nmds > 0
fly_pa <- as.data.frame(fly_pa * 1)

fly_dist <- vegdist(fly_pa, method = "jaccard")
coor_dist <- dist(fly_coor)

mantel_result <- mantel(fly_dist, coor_dist, 
                        method = "pearson", permutations = 999)
print(mantel_result)

as.matrix(coor_dist)
###################################################################################

# Calculate community similarity  
fly_similarity <- 1 - as.matrix(vegdist(fly_pa, method = "jaccard"))

# Get geographic distance matrix
geo_dist <- as.matrix(dist(fly_coor))

# Convert to vectors
similarity_vec <- fly_similarity[lower.tri(fly_similarity)]
distance_vec <- geo_dist[lower.tri(geo_dist)]


fly_decay_df <- data.frame(
  Distance = distance_vec,
  Similarity = similarity_vec
)

# Plot with ggplot2
ggplot(fly_decay_df, aes(x = Distance, y = Similarity)) +
  geom_point(color = "black", size = 2, alpha = 0.7) +   
  geom_smooth(method = "lm", color = "red", se = TRUE) +   
  labs(
    title = "Distance Decay of Fly Community Similarity",
    x = "Geographic Distance",
    y = "Jaccard Similarity"
  ) +
  theme_minimal()

max(fly_decay_df$Distance)

max(coor_dist)


# difference in Latitude
lat_dist <- abs(outer(fly_coor$Latitude, fly_coor$Latitude, FUN = "-"))
length(lat_dist)

# Calculate community similarity (Jaccard dissimilarity)
fly_similarity <- 1 - as.matrix(vegdist(fly_pa, method = "jaccard"))


similarity_vec2 <- fly_similarity[lower.tri(fly_similarity)]
lat_distance_vec <- lat_dist[lower.tri(lat_dist)]


lat_decay_df <- data.frame(
  Latitude_Distance = lat_distance_vec,
  Similarity = similarity_vec
)

ggplot(lat_decay_df, aes(x = Latitude_Distance, y = Similarity)) +
  geom_point(color = "black", size = 2, alpha = 0.7) +   
  geom_smooth(method = "lm", color = "red", se = TRUE) +   
  labs(
    title = "Latitudinal Distance Decay of Fly Community Similarity",
    x = "Latitudinal Distance (°)",
    y = "Community Similarity (Jaccard)"
  ) +
  theme_minimal()
 
lm_result <- lm(Similarity ~ Latitude_Distance, data = lat_decay_df)
summary(lm_result)


# Mantel test using latitudinal distance and community similarity
mantel_result_lati <- mantel(fly_similarity, lat_dist, 
                             method = "pearson", permutations = 999)
mantel_result_lati

########################################################################################
fly_nmds_hell <- decostand(fly_nmds, method = "hellinger")
fly_bray_nmds <- metaMDS(fly_nmds_hell, distance = "bray", k=2, na.rm = TRUE)

fly_bray_nmds$stress
stressplot(fly_bray_nmds)

scores(fly_bray_nmds) 


pred_distance<- vegdist(pred1, method = "bray")
anova(betadisper(pred_distance, pred2$Predeliction))

adonis2 (pred1~Predeliction, 
         data = pred2, permutations = 9999, 
         method = "bray")

pairwise <- pairwise.adonis(pred_distance,pred2$Predeliction)
pairwise
