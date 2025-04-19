library(vegan)
library(tidyverse)
library(readxl)
library(pairwiseAdonis)
library(ggtext)
library(ggforce)

fly_site <- read_excel("C:\\Users\\DELL\\Documents\\Git in R\\Urban-Ecology\\Data\\Fly_community.xlsx",
                       sheet = "Site")

sum(is.na(fly_site))
fly_site <- replace(fly_site, is.na(fly_site), 0) %>% 
  rename(Method="Collection Method")
# view(fly_site)
 
fly_site <- fly_site %>% 
  mutate(Longitude=  as.numeric(str_remove(Longitude, "°N")),
         Latitude = as.numeric(str_remove(Latitude, "°E")))

head(fly_site, 10)

#    PREP for MENTEL TESTS
fly_nmds <- fly_site[, 7:13]
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
  xlim(-1.3, 0.9) +  # Set x-axis to start at -4, leave upper limit automatic
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
 

# Method of fly collection (Bottle trap & Sweep nets)

fly_distance<- vegdist(fly_nmds_hell, method = "bray")
anova(betadisper(fly_distance, fly_cat$Method))

adonis2 (fly_nmds~Method, 
         data = fly_cat, permutations = 9999, # fly_cat from line 21
         method = "bray")

pairwise2 <- pairwise.adonis(fly_distance,fly_cat$Method)
pairwise2

###########################################################
set.seed(9999)
 
anova(betadisper(fly_dist, fly_cat$Method))

adonis2 (fly_dist~Method, 
         data = fly_cat, permutations = 9999, # fly_cat from line 21
         method = "jaccard")

pairwise2 <- pairwise.adonis(fly_dist, # from line 30
                             fly_cat$Method, sim.method = "jaccard",
                              perm = 9999 )
pairwise2


method_jacc_nmds <- metaMDS(fly_dist, distance = "jaccard", k=2, na.rm = TRUE)

method_jacc_nmds$stress
stressplot(method_jacc_nmds)

scores(method_jacc_nmds) 

nmds_method <- as.data.frame(method_jacc_nmds$points)
method_comb <- as.data.frame(cbind(nmds_method, fly_cat))

method_comb
# 
ggplot()  +
  
  geom_jitter(
  data = method_comb, 
  aes(x = MDS1, y = MDS2, colour = Method), 
  size = 6, 
  shape = 3,
  alpha = 0.7, 
  width = 0.04,  # horizontal jitter
  height = 0.04  # vertical jitter
) + 
  scale_colour_manual(values = c("orange","blue"))+
  scale_fill_manual(values = c("orange","blue"))+
  theme(
    text = element_text(family = "Times New Roman", size = 20)
  ) + labs(x = "NMDS1", y = "NMDS2")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black")+
  theme_minimal()+
  stat_ellipse(data = method_comb, 
               aes(x = MDS1, y = MDS2, 
                   group = Method, 
                   color = Method), 
               geom = "path", 
               level = 0.90, 
               linewidth = 0.2,   
               show.legend = NA) +
  guides(
    color = guide_legend(title = "Collection Method"),   
    shape = "none", 
    fill = "none",
    size = "none",
    alpha = "none"
  )


#################################################################################


anova(betadisper(fly_dist, fly_cat$Site))

adonis2 (fly_dist~Site, 
         data = fly_cat, permutations = 9999, # fly_cat from line 21
         method = "jaccard")

pairwise3 <- pairwise.adonis(fly_dist, # from line 30
                             fly_cat$Site, sim.method = "jaccard",
                             perm = 9999 )
pairwise3


ggplot()  +
  geom_jitter(
    data = method_comb, 
    aes(x = MDS1, y = MDS2, colour = Site, fill= Site), 
    size = 4, 
   # shape = 1,
    alpha = 0.6, 
    width = 0.09,  # horizontal jitter
    height = 0.09  # vertical jitter
  ) + 
  ylim(-1.3, 1) + 
  scale_colour_manual(values = c("black","blue", "red"))+
  scale_fill_manual(values = c("black","blue", "red"))+
  theme(
    text = element_text(family = "Times New Roman", size = 20)
  ) + labs(x = "NMDS1", y = "NMDS2")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black")+
  theme_minimal()+
  geom_mark_hull(
    data = method_comb,
    aes(x = MDS1, y = MDS2, group = Site, fill = Site, color = Site),
    concavity = 30,
    alpha = 0.05,
    linewidth = 0.6,
    show.legend = FALSE
  ) +
  guides(
    color = guide_legend(title = "Areas"),   
    shape = "none", 
    fill = "none",
    size = "none",
    alpha = "none"
  )



########################-------------------------------------------------------

# 1 to 1 plot of Bottle trap VS Sweepnet

method_compare <- fly_site %>%  # Based on relative abundance of catch!
  select(-c(1:4)) %>% 
  filter(Site !="Eatery") %>% 
  select(-Site) %>% 
  group_by(Method) %>% 
  summarise(across(where(is.numeric), sum)) %>% 
  pivot_longer(
    cols = -c("Method"),
    names_to = "Species",
    values_to = "Values"
  ) %>% 
  pivot_wider(
    names_from = Method, values_from = Values
  ) %>% 
  rename(BottleTrap = "Bottle trap") %>% 
  mutate(Total = BottleTrap+ Sweepnet) %>%
  filter(!Total <=1) %>% 
  mutate(
    BottleTrap_RA = BottleTrap / sum(BottleTrap),
    Sweepnet_RA = Sweepnet / sum(Sweepnet),
    SweepnetLog10= log10(Sweepnet+1),
    BottleTrapLog10= log10(BottleTrap+1)) %>% 
  mutate(BottleTrap_RA_Fac = ifelse((Species == "Musca domestica"),
                                 (0.1*BottleTrap_RA), BottleTrap_RA),
         Sweepnet_RA_Fac = ifelse((Species == "Musca domestica"),
                                  (0.1*Sweepnet_RA), Sweepnet_RA )) %>% 
  as.data.frame() %>% 
  mutate(Species= ifelse(  # The magic script that does the trick!
    grepl("spp\\.$",  Species),
    sub("^(\\w+) (spp\\.)$", "<i>\\1</i> \\2",  Species),
    paste0("<i>",  Species, "</i>"))) %>% 
  as.data.frame()

ggplot(method_compare,
       aes(x = BottleTrapLog10, y = SweepnetLog10)) + # using log10 transformed
  geom_point(size = 3, color = "red", alpha = 0.15) +   
  geom_abline(slope = 1, intercept = 0, 
              linetype = "dashed", color = "red" ) +   
  ggtext::geom_richtext(data = method_compare, 
                        aes(x = BottleTrapLog10, y = SweepnetLog10, 
                            label = Species),
                        fill = NA, label.color = "lightgrey",
                        size = 2.5,
                        alpha = 1)+ 
  labs(
    x = "Bottle Trap",
    y = "Sweepnet"
  ) +
  ylim(-0.02, 2.9) +
  xlim(-0.02, 2.9) +
  theme_bw()


ggplot(method_compare,
       aes(x = BottleTrap_RA_Fac, y = Sweepnet_RA_Fac)) + # using factored Relative Abundance
  geom_point(size = 3, color = "red", alpha = 0.15) +   
  geom_abline(slope = 1, intercept = 0, 
              linetype = "dashed", color = "red" ) +   
  ggtext::geom_richtext(data = method_compare, 
                        aes(x = BottleTrap_RA_Fac, y = Sweepnet_RA_Fac, 
                            label = Species),
                        fill = NA, label.color = "lightgrey",
                        size = 2.7,
                        alpha = 1)+ 
  labs(
    x = "Bottle Trap",
    y = "Sweepnet"
  ) +
  ylim(-0.001, 0.095) +
  xlim(-0.001, 0.095) +
  theme_bw()

####################################################################################

### Fishers test for significnat difference in proportion of 
# Samples collected in Bottle traps VS Sweepnets

BT_SN <- fly_site %>%   
  select(-c(1:4)) %>% 
  filter(Site !="Eatery") %>% 
  select(-Site) %>% 
  group_by(Method) %>% 
  summarise(across(where(is.numeric), sum)) %>% 
  pivot_longer(
    cols = -c("Method"),
    names_to = "Species",
    values_to = "Values"
  ) %>% 
  pivot_wider(
    names_from = Method, values_from = Values
  ) %>% 
  rename(BottleTrap = "Bottle trap") %>% 
  as.data.frame() 
  
row.names(BT_SN) <- BT_SN$Species
BT_SN <- BT_SN[, -1]  
fisher.test(BT_SN)

install.packages("DescTools")  # Only the first time
library(DescTools)

GTest(BT_SN)



sum(BT_SN$BottleTrap)
sum(BT_SN$Sweepnet)


# Some Exploratory Data analysis....
fly_site %>%   
  select(-c(1:4)) %>% 
  select(-Site) %>% 
  group_by(Method) %>% 
  summarise(across(where(is.numeric), sum)) %>% 
  pivot_longer(
    cols = -c("Method"),
    names_to = "Species",
    values_to = "Values"
  ) %>% 
  pivot_wider(
    names_from = Method, values_from = Values
  ) %>% 
  rename(BottleTrap = "Bottle trap") %>% 
  mutate(Total = BottleTrap+ Sweepnet) %>%
  summarise(sumBottle = sum(BottleTrap),
            sumSweep = sum(Sweepnet), 
            sumTotal = sum(Total))





