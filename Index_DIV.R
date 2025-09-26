rescale <- scales::rescale

######## READ DATA ################

actdir<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(actdir)

# Read full_table with questionnaire data and NUTS2 and NUTS3 map data (originally from .shp files)
load("full_df_and_shp_maps.RDA")

# create a multiple list from the full dataframe

full_df_list <- lapply(1:nrow(full_df), function(x){
  full_df[x, ]
})

########################
# for Q_13 #
adjacent_area_value_list <- c(0, 2, 2, 4, 3, 4, 1) # max = 16
names(adjacent_area_value_list) <-  c("Urban area", "Garden, park", 
                                      "Agricultural area/farm", 
                                      "Forest", "Field, meadow", 
                                      "Wetland", "Other")

# for Q_18-25
diversity_value_list <- c(0, 1, 2, 3, 4)
names(diversity_value_list) <-  c("Do not have", "Very small",
                                  "Medium", "Significant", "Most of it")

weight_num_value_list <- c(1.5, 1.5, 1.75, 0.5, 1, 0.5, 0.5, 2)
names(weight_num_value_list) <-  c("Q_18", "Q_19", "Q_20", "Q_21", "Q_22", 
                                  "Q_23", "Q_24", "Q_25")

# for Q_72 #
support_poll_value_list <- c(3, 2, 2, 1, 0) # max = 8
names(support_poll_value_list) <-  c("Creating/preserving natural habitats", 
                                     "Creating artificial habitats", 
                                     "Providing food sources", 
                                     "Water sources",
                                     "I do not support them actively")

#############################
#Q_11: What is the area of your garden? 

values_of_garden_size_vector <- sapply(full_df_list, function(x){
  garden_area_size = x["Q_11"]
  case_when(
    garden_area_size == "Under 10 m2" ~ 1,
    garden_area_size == "10-50 m2" ~ 2,
    garden_area_size == "50-100 m2" ~ 3,
    garden_area_size == "100-500 m2" ~ 4,
    garden_area_size == "Over 500 m2" ~ 5
  )
})

#############################
#Q_13: What type of land your garden is adjacent to? 
#Multiple choices are allowed

values_of_adjacent_areas_vector <- sapply(full_df_list, function(x){
  adjacent_areas <- unlist(strsplit(as.character(x["Q_13"]),';'))
  values <- adjacent_area_value_list[adjacent_areas]
  sum(values)
})

#############################
#Q_18-25: How large area do the listed plants take up in your garden?
# Diversity
# GSw(θ) = ∑i  wi * pi * (1 − pi) (cit*)
# adott elemnem a relativ gyakorisága
# wi adott elem súlyozása

diversity_vector <- sapply(full_df_list, function(x){
  div_vector <- as.character(x[c("Q_18", "Q_19", "Q_20", "Q_21", "Q_22", 
                             "Q_23", "Q_24", "Q_25")])
  value <- diversity_value_list[div_vector]
  num_of_type = length(value[!value == 0])
  
  if (all(value == 0))
    {
    return_value = 0
    }
  else {
  pi = value/sum(value)
  return_value <- sum(sapply(1:length(value), function(g){
    weight_num_value_list[g] * pi[g] * (1-pi[g])
    }))
  }
  return_value*num_of_type
})

#############################
#Q_15: Do you have a pond in your garden?
#Q_36: Do you leave unmown patches when you mow?
#Q_37: Do you have any undisturbed areas in your garden?
# "puu" means pond-unmown-undisturbed

values_of_puu_vector <- sapply(full_df_list, function(x){
  x[c("Q_15", "Q_36", "Q_37")]
  pond <- ifelse(x["Q_15"]  == "No", 0, 3)
  unmown <- ifelse(x["Q_36"] == "No", 0, 1)
  undisturbed <- ifelse(x["Q_37"] == "No", 0, 2)
  puu <- c(pond, unmown, undisturbed)
  sum(puu)
  })

#############################
#Q_35: How often do you mow the lawn during the growing season? 

values_of_mow_freq_vector <- sapply(full_df_list, function(x){
  mowing_freq = x["Q_35"]
  
  if ((x["Q_18"]  == "Do not have") & (x["Q_19"]  == "Do not have") &
    (x["Q_20"]  == "Do not have") & (x["Q_21"]  == "Do not have") &
    (x["Q_22"]  == "Do not have") & (x["Q_23"]  == "Do not have") &
    (x["Q_24"]  == "Do not have") & (x["Q_25"]  == "Do not have"))
  {mowing_return <- 0}
  
  else {
  mowing_return <- case_when(
    mowing_freq == "Several times a month" ~ 0,
    mowing_freq == "Once a month" ~ 1,
    mowing_freq == "Every two months" ~ 2,
    mowing_freq == "Twice" ~ 3,
    mowing_freq == "Once" ~ 4,
    mowing_freq == "I do not mow the lawn at all" ~ 5 
  )}
  mowing_return
})

#############################
#Q_39: Do you use artificial grass in your garden? 

values_of_artificial_grass_vector <- sapply(full_df_list, function(x){
  artificial_grass = x["Q_39"]
  case_when(
    artificial_grass == "Yes, regularly" ~ -1,
    artificial_grass == "Yes, sometimes" ~ -1,
    artificial_grass == "No" ~ 0
  )
})

#############################
#Q_44: Do you use herbicides in your garden?

values_of_herbicides_vector <- sapply(full_df_list, function(x){
  herbicides = x["Q_44"]
  case_when(
    herbicides == "Yes, I use it" ~ -1,
    herbicides == "I use herbicides, but I do not know the main active ingredient" ~ -1,
    herbicides == "I use herbicides, but the main active ingredient is not glyphosate (2,4-D)" ~ -1,
    herbicides == "I do not use herbicides at all" ~ 0
  )
})

#############################
#Q_50: Do you support beneficial insects to promote the biocontrol of weeds and pest? 

values_of_beneficial_insects_vector <- sapply(full_df_list, function(x){
  biocontrol_insects = x["Q_50"]
  case_when(
    biocontrol_insects == "Yes" ~ 1,
    biocontrol_insects == "No" ~ 0
  )
})

#############################
#"Q_71": What pollinators do you regularly observe/see in your garden?

values_of_pollinators_vector <- sapply(full_df_list, function(x){
  poll_answers <- unlist(strsplit(as.character(x["Q_71"]),';'))
  if (any(poll_answers %in% "None of the above"))
  {
    poll = 0
  }
  else
  {
    poll = length(poll_answers)
  }
  poll
})

#############################
#"Q_72: How do you support wild pollinators? 
# Multiple choices are allowed

values_of_poll_support_type_vector <- sapply(full_df_list, function(x){
  poll_support_type <- unlist(strsplit(as.character(x["Q_72"]),';'))
  values <- support_poll_value_list[poll_support_type]
  sum(values)
})

#############################
# Createa a dataframe with the lists

diversity_index_df <- data.frame(Size = values_of_garden_size_vector, 
                 Adjacent_area = values_of_adjacent_areas_vector, 
                 Diversity = diversity_vector,
                 PUU = values_of_puu_vector,
                 Mow_freq = values_of_mow_freq_vector,
                 Aftif_grass = values_of_artificial_grass_vector,
                 Herbicides = values_of_herbicides_vector,
                 Benef_insects = values_of_beneficial_insects_vector,
                 Pollinators = values_of_pollinators_vector,
                 Poll_support = values_of_poll_support_type_vector,
                 ID = full_df$ID)

sapply(diversity_index_df, range)

# Rescale #

rescaled_diversity_index_df <- diversity_index_df

rescaled_diversity_index_df$Size <- 
  rescale(c(rescaled_diversity_index_df$Size, 1, 5), 
          to = c(1, 10))[1:length(rescaled_diversity_index_df$Size)]

rescaled_diversity_index_df$Adjacent_area <- 
  rescale(c(rescaled_diversity_index_df$Adjacent_area, 0, 16), 
          to = c(0, 20))[1:length(rescaled_diversity_index_df$Adjacent_area)]

rescaled_diversity_index_df$Diversity <- 
  rescale(c(rescaled_diversity_index_df$Diversity, 0, 10.20444), #elmeleti maximum, valojaban senki sem erte el
          to = c(0, 20))[1:length(rescaled_diversity_index_df$Diversity)]

rescaled_diversity_index_df$PUU <- 
  rescale(c(rescaled_diversity_index_df$PUU, 0, 6), 
          to = c(0, 10))[1:length(rescaled_diversity_index_df$PUU)]

rescaled_diversity_index_df$Mow_freq <- 
  rescale(c(rescaled_diversity_index_df$Mow_freq, 0, 5), 
          to = c(0, 5))[1:length(rescaled_diversity_index_df$Mow_freq)]

rescaled_diversity_index_df$Aftif_grass <- 
  rescale(c(rescaled_diversity_index_df$Aftif_grass, -1, 0), 
          to = c(0, 5))[1:length(rescaled_diversity_index_df$Aftif_grass)]

rescaled_diversity_index_df$Herbicides <- 
  rescale(c(rescaled_diversity_index_df$Herbicides, -1, 0), 
          to = c(0, 5))[1:length(rescaled_diversity_index_df$Herbicides)]

rescaled_diversity_index_df$Benef_insects <- 
  rescale(c(rescaled_diversity_index_df$Benef_insects, 0, 1), 
          to = c(0, 5))[1:length(rescaled_diversity_index_df$Benef_insects)]

rescaled_diversity_index_df$Pollinators <- 
  rescale(c(rescaled_diversity_index_df$Pollinators, 0, 9), 
          to = c(0, 10))[1:length(rescaled_diversity_index_df$Pollinators)]

rescaled_diversity_index_df$Poll_support  <- 
  rescale(c(rescaled_diversity_index_df$Poll_support , 0, 8), 
          to = c(0, 10))[1:length(rescaled_diversity_index_df$Poll_support )]


# SUM #
rescaled_diversity_index_df$SUM <- 
  (rowSums(rescaled_diversity_index_df[, 1:10])) 

DIV_index_df <- rescaled_diversity_index_df
#save(DIV_index_df, file = "DIV_index_df.RDA")

#############################################################
library(lattice)
windows()
histogram(DIV_index_df$SUM, nint = 200)

pdf("Histograms_diversity_index.pdf", width = 12, height = 8)
par(mfrow = c(3, 4))
sapply(DIV_index_df[,c(1:10, 12)], function(x){
  print(hist(x))
})
dev.off()

#cit*
# https://www.hindawi.com/journals/ijecol/2012/478728/
