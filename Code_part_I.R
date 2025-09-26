######## PACKAGES ################
library(dplyr)
library(ggplot2)
library(ggpubr)
library(lubridate) 
library(scales) # for freq tables
library(plotly)
# for the maps #
library(udunits2)
library(sf)
library(ggspatial)
library(scatterpie)
library(gridExtra)
library(rnaturalearth)

######## READ DATA ################

actdir<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(actdir)

# Read full_table with questionnaire data and NUTS2 and NUTS3 map data (originally from .shp files)
load("full_df_and_shp_maps.RDA")
save(EU_NUTS2_map, file = "EU_NUTS2_map.RDA")

######## Contingency table with NUTS3 data ################

respondents_NUTS <- as.data.frame(table(full_df$Q_3))
sex_NUTS <- as.matrix(table(full_df$Q_3, full_df$Q_7),
                      dimnames = list(unique(full_df$Q_3), 
                                      unique(full_df$Q_7)))

age_NUTS <- as.matrix(table(full_df$Q_3, full_df$Q_8),
                      dimnames = list(unique(full_df$Q_3), 
                                      unique(full_df$Q_8)))

edu_NUTS <- as.matrix(table(full_df$Q_3, full_df$Q_9),
                      dimnames = list(unique(full_df$Q_3), 
                                      unique(full_df$Q_9)))

gardtype_NUTS <- as.matrix(table(full_df$Q_3, full_df$Q_17),
                           dimnames = list(unique(full_df$Q_3), 
                                           unique(full_df$Q_17)))

result <- as.data.frame(cbind(sex_NUTS, age_NUTS, edu_NUTS, 
                              gardtype_NUTS, respondents_NUTS$Freq))

result$NUTS <- rownames(result)

colnames(result)[4:22] <- c("Age_18_25", "Age_26_35", "Age_36_45", 
                            "Age_46_55", "Age_56_65", "Age_Over_65", 
                            "Age_Under_18", "Edu_Elementary", "Edu_Middle", 
                            "Edu_Pgraduate", "Edu_Psecond", "Community_g",
                            "Flower_g", "House_g", "Kitchen_g", "Orchard",
                            "Other_g", "Vineyard", "Respondents_num")
# for NUTS3 - except PL
country_code <- c("CZ", "EE", "HR", "HU", 
                  "LV", "RO", "SK", "SI") # except PL

filtered_map <- EU_NUTS3_map %>% 
  select(c( NUTS, NAME, NUTS0, POPULATION, 
            MALES, FEMALES, GDP)) %>% 
  filter(NUTS0 %in% country_code)

# Merger of the capital city and its county
county_list <- list(c("CZ010", "CZ020"), c("HR050", "HR065"), 
                    c("HU110", "HU120"), c("LV006", "LV007"),
                    c("RO321", "RO322"))

for(x in 1:length(county_list)){
  a = county_list[[x]][1]
  b = county_list[[x]][2]
  County <- st_union(filtered_map[filtered_map$NUTS == a, ], 
                     filtered_map[filtered_map$NUTS == b, ])
  
  County$POPULATION <- County$POPULATION+County$POPULATION.1
  County$MALES <- County$MALES+County$MALES.1
  County$FEMALES <- County$FEMALES+County$FEMALES.1
  County$GDP <- (County$GDP+County$GDP.1)/2
  filtered_map[filtered_map$NUTS == b, ] <- County[, c("NUTS.1", "NAME.1", 
                                                       "NUTS0", "POPULATION", 
                                                       "MALES", "FEMALES", "GDP")]
  filtered_map <- filtered_map %>% 
    filter(!NUTS == a)}

#windows()
#plot(filtered_map["NUTS"])
#length(sort(unique(filtered_map$NUTS)))
#length(sort(unique(result$NUTS)))
#result <- setdiff(result$NUTS, filtered_map$NUTS)
#result$NUTS[! result$NUTS %in% filtered_map$NUTS]

results2 <- merge(filtered_map, result, by = "NUTS", 
                  all.x=TRUE, all.y =FALSE)

# NUTS2 for Poland

filtered_map_PL <- EU_NUTS2_map %>% 
  select(c( NUTS, NAME, NUTS0, POPULATION, MALES, 
            FEMALES, GDP)) %>% 
  filter(NUTS0 == "PL") 

#plot(filtered_map_PL["NUTS"])

#Merger of the capital city and its county (PL)
County_PL <- st_union(filtered_map_PL[filtered_map_PL$NUTS == "PL91", ], 
                      filtered_map_PL[filtered_map_PL$NUTS == "PL92", ])

County_PL$POPULATION <- County_PL$POPULATION+County_PL$POPULATION.1
County_PL$MALES <- County_PL$MALES+County_PL$MALES.1
County_PL$FEMALES <- County_PL$FEMALES+County_PL$FEMALES.1
County_PL$GDP <- (County_PL$GDP+County_PL$GDP.1)/2
filtered_map_PL[filtered_map_PL$NUTS == "PL92", ] <- County_PL[, c("NUTS.1", "NAME.1", 
                                                                   "NUTS0", "POPULATION", 
                                                                   "MALES", "FEMALES", "GDP")]
filtered_map_PL <- filtered_map_PL %>% 
  filter(!NUTS == "PL91")

#plot(filtered_map_PL["NUTS"])

results3 <- merge(filtered_map_PL, result, by = "NUTS", 
                  all.x=TRUE, all.y =FALSE)

full_table <- bind_rows(results2, results3)
#The “full_table” is 5 rows longer than the “results” because
#it includes 5 NUTS3 categories from which we had no respondents.

#windows()
#plot(full_table["NUTS"])

#centroids <- st_centroid(full_table)

centroids_2 <- cbind(full_table$NUTS, st_coordinates(st_centroid(full_table$geometry)))
centroids_2 <- as.data.frame(centroids_2)
colnames(centroids_2)[1] <- "NUTS_cat"

centroids_2$X <- as.numeric(centroids_2$X)
centroids_2$Y <- as.numeric(centroids_2$Y)

######## Standardization ################

standard_NUTS <- round(full_table$Respondents_num/full_table$POPULATION*100000,2)
standard_NUTS[is.na(standard_NUTS)] <- 0
standard_NUTS <- log(standard_NUTS+0.1) # log!

#The “full_table” is supplemented with the standardization values.
full_table <- cbind(full_table, data.frame(Pop_stand = standard_NUTS))
#save(full_table, file = "full_table_benne_NUTS23_polygon.RDA")

######## FIGURE: Map ################

windows()
map_figure <- ggplot(data = full_table) + # code for only ONE COUNTRY: full_table[full_table$NUTS0 == "HU",]
  geom_sf(aes(fill = Pop_stand)) +
  #geom_sf_text(aes(label = NUTS), col = "black", size = 2.5, angle = 20)+
  scale_fill_gradient2(name = "The number of respondents for 100k inhabitants") +
  theme_classic()+
  scale_fill_gradient(low="cornsilk1", high='cornsilk4')+
  theme(axis.line = element_line(size = 0.1, linetype = "solid"))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  theme(axis.text.x = element_text(angle=40))+
  theme(axis.text.y = element_text(angle=40))+
  theme(legend.position=c(1,0.08),
        legend.justification=c(1,1),
        legend.direction="horizontal",
        legend.box="horizontal",
        legend.box.just = c("top"),
        legend.title=element_blank(),
        legend.background = element_rect(fill=alpha('white', 0.1))
  )

# ggsave("NUTS3_NUTS2_full_map.png", map_figure, width = 7, height = 8)


#full_table <- as_tibble(full_table)
#full_table <- as.data.frame(full_table)

attr(full_table, "class") <- "data.frame"
attr(full_table, "sf_column") <- NULL

full_df_2 <- merge(full_df, full_table[, c("NUTS", "POPULATION", 
                                           "GDP", "Respondents_num", "Pop_stand")], 
                   by.x = "Q_3", by.y = "NUTS",
                   all.x=TRUE, all.y =FALSE)

full_df_2 <- merge(full_df_2, centroids_2, 
                   by.x = "Q_3", by.y = "NUTS_cat",
                   all.x=TRUE, all.y =FALSE)

# The "full_df_2" contains all respondents (5255) along with 
# all the data we collected, as well as Pop, GDP, respondents_Num, 
# pol_standard, and the two coordinates; we will also add the indices to this. 

######## Add INDECES to the full data ################ 

# Please note: there are three separate codes for 
# creating our three indices (DIV, PES, PET)

load("DIV_index_df.RDA")
# Add data of Diversity (DIV) index to the full_df_2
full_df_2 <- merge(full_df_2, DIV_index_df[, c("SUM", "ID")], 
                   by = "ID", all=TRUE)
colnames(full_df_2)[colnames(full_df_2) == "SUM"] <- "Index_DIV"

load("RES_index_df.RDA")
# Add data of Respondents (RES) index to the full_df_2
full_df_2 <- merge(full_df_2, RES_index_df[, c("SUM", "ID")], 
                   by = "ID", all=TRUE)
colnames(full_df_2)[colnames(full_df_2) == "SUM"] <- "Index_RES"

load("PES_index_df.RDA")
# Add data of Pesticide (PES) index to the full_df_2
full_df_2 <- merge(full_df_2, PES_index_df[, c("SUM", "ID")], 
                   by = "ID", all=TRUE)
colnames(full_df_2)[colnames(full_df_2) == "SUM"] <- "Index_PES"

#save(full_df_2, file = "full_df_with_all_data.RDA") # In this dataset, data referring to NUTS2 and NUTS3 are mixed.
