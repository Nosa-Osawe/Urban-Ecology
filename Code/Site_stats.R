library(vegan)
library(tidyverse)
library(readxl)
library(pairwiseAdonis)
library(ggtext)

fly_site <- read_excel("C:\\Users\\DELL\\Documents\\Git in R\\Urban-Ecology\\Data\\Fly_community.xlsx",
                       sheet = "Site")

sum(is.na(fly_site))
fly_site <- replace(fly_site, is.na(fly_site), 0) %>% 
  rename(Method="Collection Method")
# view(fly_site)
fly_site$`Collection Method`
fly_site <- fly_site %>% 
  mutate(Longitude=  as.numeric(str_remove(Longitude, "°N")),
         Latitude = as.numeric(str_remove(Latitude, "°E")))

head(fly_site, 10)

#    PREP for MENTEL TESTS
fly_nmds <- fly_site[, 7:14]
fly_cat <- fly_site[, 1:6]
fly_coor <- fly_site[, 3:4]

# Converting the data to presence-absence data
fly_pa <- fly_nmds > 0
fly_pa <- as.data.frame(fly_pa * 1)

fly_dist <- vegdist(fly_pa, method = "jaccard")
coor_dist <- dist(fly_coor)

mantel_result <- mantel(fly_dist, coor_dist, 
                        method = "pearson", permutations = 999)
print(mantel_result)


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

min(coor_dist)


# difference in Latitude
lat_dist <- abs(outer(fly_coor$Latitude, fly_coor$Latitude, FUN = "-"))
length(lat_dist)

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
                             method = "pearson", 
                             permutations = 999)
mantel_result_lati

########################################################################################
fly_nmds_hell <- decostand(fly_nmds, method = "hellinger")
fly_bray_nmds <- metaMDS(fly_nmds_hell, distance = "bray", k=2, na.rm = TRUE)

fly_bray_nmds$stress
stressplot(fly_bray_nmds)

scores(fly_bray_nmds) 


fly_distance<- vegdist(fly_nmds_hell, method = "bray")
anova(betadisper(fly_distance, fly_cat$Collector))

adonis2 (fly_nmds~Collector, 
         data = fly_cat, permutations = 9999, # fly_cat from line 21
         method = "bray")

pairwise1 <- pairwise.adonis(fly_distance,fly_cat$Collector)
pairwise1

color_pal <- c("#4daf4a","#ff7f00","#377eb8","purple","#e31a1c")


nmds_fly <- as.data.frame(fly_bray_nmds$points)
sc_nmds_fly <- as.data.frame(scores(fly_bray_nmds)$species)  
fly_comb <- as.data.frame(cbind(nmds_fly, fly_cat))



sc_nmds_fly$label_html <- ifelse(
  grepl("spp\\.$", rownames(sc_nmds_fly)),
  sub("^(\\w+) (spp\\.)$", "<i>\\1</i> \\2", rownames(sc_nmds_fly)),
  paste0("<i>", rownames(sc_nmds_fly), "</i>")
)

# 
ggplot() +
  geom_point(data = fly_comb, aes(x = MDS1, y = MDS2, 
                                       color = Collector, 
                                       fill = Collector,
                                  alpha = 0.95
  ), 
  size = 3) + 
  scale_colour_manual(values = color_pal)+
  scale_fill_manual(values = color_pal)+
  theme(
    text = element_text(family = "Times New Roman", size = 20)
  ) + labs(x = "NMDS1", y = "NMDS2")+
  xlim(-0.6, 1.3) +  # Set x-axis to start at -4, leave upper limit automatic
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black")+
  theme_minimal()+
  stat_ellipse(data = fly_comb, 
               aes(x = MDS1, y = MDS2, 
                   group = Collector, 
                   color = Collector), 
               geom = "path", 
               level = 0.90, 
               linewidth = 0.7,   
               show.legend = NA) +
  ggtext::geom_richtext(data = sc_nmds_fly, 
                        aes(x = NMDS1, y = NMDS2, label = label_html),
                        fill = "lightgrey", label.color = NA,
                        size = 4,
                        alpha = 0.3)+
  ggtext::geom_richtext(data = sc_nmds_fly, 
                        aes(x = NMDS1, y = NMDS2, label = label_html),
                        fill = NA, label.color = NA,
                        size = 4,
                        alpha = 1)+
  guides(
    color = guide_legend(title = "Site"),   
    shape = "none", 
    fill = "none",
    size = "none",
    alpha = "none"
  )
 


fly_distance<- vegdist(fly_nmds_hell, method = "bray")
anova(betadisper(fly_distance, fly_cat$Method))

adonis2 (fly_nmds~Method, 
         data = fly_cat, permutations = 9999, # fly_cat from line 21
         method = "bray")

pairwise2 <- pairwise.adonis(fly_distance,fly_cat$Method)
pairwise2
