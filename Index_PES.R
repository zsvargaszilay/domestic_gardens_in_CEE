rescale <- scales::rescale

# Create filtered full df
#non_pesticide_users <- full_df[full_df$Q_51 == "No", ] --> point: 0

pesticide_users <- full_df[full_df$Q_51 == "Yes", ]

# Create a multiple list from the full dataframe
pesticide_users_list <- lapply(1:nrow(pesticide_users), function(x){
  pesticide_users[x, ]
})

#############################
#############################
# for Q_42 #
unique(full_df$Q_42)
fertilizer_value_list <- c(2, 1, 1, 0)
names(fertilizer_value_list) <-  c("I use synthetic fertilizer",
                                   "I use animal manure",
                                   "I use compost", 
                                   "I do not fertilize")

# for Q_45 and Q_46 #
fleas_value_list <- c(2, 1, 0, 0)
names(fleas_value_list) <-  c("I have it/them, and I treat it/them several times a year",
                              "I have it/them, and I treat it/them once a year",
                              "I have it/them, but I do not give it/them preventive treatments",
                              "I do not have it/them")

# for Q_53 #
pesticide_type_value_list <- c(4, 2, 1)
names(pesticide_type_value_list) <-  c("Conventional pesticides/synthetic pesticides",
                                       "Eco/Bio/Green labelled pesticides",
                                       "Self-made pesticides, home practices")
#pesticide_type_max_value <- 7 # (4+2+1)

# for Q_53 #
pesti_products_value_list <- c(1, 2, 3, 4) 
names(pesti_products_value_list) <-  c("Only 1", "2-4", 
                                       "5-10", "More than 10")

# for Q_55 #
pests_value_list <-  c(3, 2, 2, 2, 1, 1, 1, 3, 2, 2, 1)
names(pests_value_list) <-  c("Arthropods", "Snails", "Nematodes", 
                              "Vertebrates", "Fungi", "Bacteria", 
                              "Viruses", "Weeds", "Mosses", 
                              "Trees", "Other")
#pests_max_value <- 20 # 3+2+2+2+1+1+1+3+2+2+1

# for Q_65 and Q_66 #
choice_value_list <- c(0, 1, 2, 3, 4)
names(choice_value_list) <- c("Crucial", "Important", "Moderately",  
                              "Negligible importance", "Not important")

# for Q_68 #
shops_value_list <- c(1, 2, 3, 3, 4, 2)
names(shops_value_list) <- c("Horticulture/floral shops", "Agricultural shops", 
                             "Supermarkets or other big shops","Online shops", 
                             "From acquaintances", "Other")
#shops_max_value <- 15 # 1+2+3+3+4+2

# for Q_69 #
packaging_value_list <- c(1, 3, 2, 4, 5)
names(packaging_value_list) <- c("Yes, before I buy", 
                                 "Yes, the first time I use", 
                                 "Yes, whenever I use", 
                                 "Occasionally, if I do not forget", 
                                 "Rarely or never")  

#############################
#Q_42: Do you regularly fertilizer your garden?

values_of_fertilizer_type_vector <- sapply(pesticide_users_list, function(x){
  fertilizer_types <- unlist(strsplit(as.character(x["Q_42"]),';'))
  return_val <- sum(fertilizer_value_list[fertilizer_types])
})

#############################
#Q_45 and Q_46: Do you have a dog(g) and/or cat(s) in your garden? 
#If so, do you regularly treat it/them against fleas in the form 
#of preventive collars or drops?

values_of_fleas_vector <- sapply(pesticide_users_list, function(x){
  vector <- as.character(x[c("Q_45", "Q_46")])
  value <- sum(fleas_value_list[vector])
})

#############################
#Q_52: What type of pesticies(s) do you use? 

values_of_pesticide_type_vector <- sapply(pesticide_users_list, function(x){
  pest_types <- unlist(strsplit(as.character(x["Q_52"]),';'))
  return_val <- sum(pesticide_type_value_list[pest_types])
})

#############################
#Q_53: How many different pesticides do you use in your garden? 

values_of_products_vector <- sapply(pesticide_users_list, function(x){
  products = x[["Q_53"]]
  value_products <- pesti_products_value_list[products]
})

#############################
#Q_54: Do you use additives with pesticides? 

values_of_additives_vector <- sapply(pesticide_users_list, 
                                     function(x){
  additives = x["Q_54"]
  additives <- case_when(
    additives == "Yes" ~ 1,
    additives == "No" ~ 0)
  return_val <- additives
})

#############################
#Q_55: What kind of organisms do you use pesticides against in your garden? 
#Multiple choices are allowed

values_of_pests_type_vector <- sapply(pesticide_users_list, function(x){
  pests <- unlist(strsplit(as.character(x["Q_55"]),';'))
  pests_value <- sum(pests_value_list[pests])
})

#############################
#Q_65 How important the listed aspects are for you when you choose a pesticide?

values_of_risk_on_bees_vector <- sapply(pesticide_users_list, function(x){
  risk_on_bees = x[["Q_65"]]
  value_risk_on_bees <- choice_value_list[risk_on_bees]
})

#############################
#Q_66: How important the listed aspects are for you when you choose a pesticide?
  
values_of_risk_on_human_vector <- sapply(pesticide_users_list, function(x){
  risk_on_human = x[["Q_66"]]
  value_risk_on_bees <- choice_value_list[risk_on_human]
})

#############################
#Q_68: Where do you usually buy the pesticides? 
#Multiple choices are allowed

values_of_shops_vector <- sapply(pesticide_users_list, function(x){
  shops_type <- unlist(strsplit(as.character(x["Q_68"]),';'))
  values <- sum(shops_value_list[shops_type])
})

#############################
#Q_69: Do you read the direction for use on the packaging? 

values_of_packaging_vector <- sapply(pesticide_users_list, function(x){
  packaging = x[["Q_69"]] 
  pest_types <- unlist(strsplit(as.character(x["Q_52"]),';'))
  if (all(pest_types == "Self-made pesticides, home practices") &
    (packaging == "Rarely or never"))
  {
    packaging_value = 0
  }
  else
  {
    packaging_value <- packaging_value_list[packaging]
  }
  packaging_value
})
  
#############################
#Q_70: How important is pesticide use in your garden? 

values_of_important_vector <- sapply(pesticide_users_list, function(x){
  important_value = x[["Q_70"]]
})

#############################
#############################
str(values_of_fertilizer_type_vector)
pesticide_index_df <- data.frame(Fertilizer = values_of_fertilizer_type_vector,
                                 Fleas = values_of_fleas_vector,
                                 Pesticide_type = values_of_pesticide_type_vector,
                                 Pest_products = values_of_products_vector,
                                 Additives = values_of_additives_vector,
                                 Pest = values_of_pests_type_vector,
                                 Risk_bees = values_of_risk_on_bees_vector,
                                 Risk_human = values_of_risk_on_human_vector,
                                 Shops = values_of_shops_vector,
                                 Packaging = values_of_packaging_vector,
                                 Importance = values_of_important_vector,
                                 ID = pesticide_users$ID)
sapply(pesticide_index_df, range)

# Rescale #

rescaled_pesticide_index_df <- pesticide_index_df

rescaled_pesticide_index_df$Fertilizer <- 
  rescale(c(rescaled_pesticide_index_df$Fertilizer, 0, 4), 
          to = c(0, 6))[1:length(rescaled_pesticide_index_df$Fertilizer)]

rescaled_pesticide_index_df$Fleas <- 
  rescale(c(rescaled_pesticide_index_df$Fleas, 0, 4),  
          to = c(0, 5)) [1:length(rescaled_pesticide_index_df$Fleas)]

rescaled_pesticide_index_df$Pesticide_type <- 
  rescale(c(rescaled_pesticide_index_df$Pesticide_type, 1, 7),
          to = c(1, 18)) [1:length(rescaled_pesticide_index_df$Pesticide_type)]

rescaled_pesticide_index_df$Pest_products <- 
  rescale(c(rescaled_pesticide_index_df$Pest_products, 1, 4),
          to = c(1, 14)) [1:length(rescaled_pesticide_index_df$Pest_products)]

rescaled_pesticide_index_df$Additives <- 
  rescale(c(rescaled_pesticide_index_df$Additives, 0, 1),
          to = c(0, 5)) [1:length(rescaled_pesticide_index_df$Additives)]

rescaled_pesticide_index_df$Pest <- 
  rescale(c(rescaled_pesticide_index_df$Pest, 1, 20), #elmeleti maximum, amugy 19-et ertek maximum el
          to = c(1, 14)) [1:length(rescaled_pesticide_index_df$Pest)]

rescaled_pesticide_index_df$Risk_bees <- 
  rescale(c(rescaled_pesticide_index_df$Risk_bees, 0, 4),
          to = c(0, 6)) [1:length(rescaled_pesticide_index_df$Risk_bees)]

rescaled_pesticide_index_df$Risk_human <- 
  rescale(c(rescaled_pesticide_index_df$Risk_human, 0, 4),
          to = c(0, 6))[1:length(rescaled_pesticide_index_df$Risk_human)]

rescaled_pesticide_index_df$Shops <- 
  rescale(c(rescaled_pesticide_index_df$Shops, 1, 15),
          to = c(1, 6)) [1:length(rescaled_pesticide_index_df$Shops)]

rescaled_pesticide_index_df$Packaging <- 
  rescale(c(rescaled_pesticide_index_df$Packaging, 0, 5),
          to = c(0, 6))[1:length(rescaled_pesticide_index_df$Packaging)]

rescaled_pesticide_index_df$Importance <- 
  rescale(c(rescaled_pesticide_index_df$Importance, 1, 5),
          to = c(0, 14))[1:length(rescaled_pesticide_index_df$Importance)]

# SUM #
rescaled_pesticide_index_df$SUM <- 
  (rowSums(rescaled_pesticide_index_df[, 1:11])) 

windows()
histogram(rescaled_pesticide_index_df$SUM)

#pdf("Histograms_pesticide_only_users_index.pdf", width = 12, height = 8)
#par(mfrow = c(3, 4))
#sapply(rescaled_pesticide_index_df[,c(1:11, 13)], function(x){
  #print(hist(x))
#})
#dev.off()

###############################

# non pesticide users #
non_pesticide_users <- full_df[full_df$Q_51 == "No", ]
number_of_non_users = length(non_pesticide_users$ID)

# Create a multiple list from the full dataframe
non_users_list <- lapply(1:nrow(non_pesticide_users), function(x){
  non_pesticide_users[x, ]
})

#############################
#Q_42: Do you regularly fertilizer your garden?

values_of_fertilizer_type_vector_2 <- sapply(non_users_list, function(x){
  fertilizer_types <- unlist(strsplit(as.character(x["Q_42"]),';'))
  return_val <- sum(fertilizer_value_list[fertilizer_types])
})

#############################
#Q_45 and Q_46: Do you have a dog(g) and/or cat(s) in your garden? 
#If so, do you regularly treat it/them against fleas in the form 
#of preventive collars or drops?

values_of_fleas_vector_2 <- sapply(non_users_list, function(x){
  vector <- as.character(x[c("Q_45", "Q_46")])
  value <- sum(fleas_value_list[vector])
})

#############################
non_users_index_df <- data.frame(Fertilizer = values_of_fertilizer_type_vector_2,
                                 Fleas = values_of_fleas_vector_2,
                                 Pesticide_type = rep(0, number_of_non_users),
                                 Pest_products = rep(0, number_of_non_users),
                                 Additives = rep(0, number_of_non_users),
                                 Pest = rep(0, number_of_non_users),
                                 Risk_bees = rep(0, number_of_non_users),
                                 Risk_human = rep(0, number_of_non_users),
                                 Shops = rep(0, number_of_non_users),
                                 Packaging = rep(0, number_of_non_users),
                                 Importance = rep(0, number_of_non_users),
                                 ID = non_pesticide_users$ID)

#############################
# all respondets #

all_respondets_df <- rbind(pesticide_index_df, non_users_index_df)

# Rescale #

rescaled_all_respondets_df <- all_respondets_df

rescaled_all_respondets_df$Fertilizer <- 
  rescale(c(rescaled_all_respondets_df$Fertilizer, 0, 4), 
          to = c(0, 6))[1:length(rescaled_all_respondets_df$Fertilizer)]

rescaled_all_respondets_df$Fleas <- 
  rescale(c(rescaled_all_respondets_df$Fleas, 0, 4),  
          to = c(0, 5)) [1:length(rescaled_all_respondets_df$Fleas)]

rescaled_all_respondets_df$Pesticide_type <- 
  rescale(c(rescaled_all_respondets_df$Pesticide_type, 0, 7),
          to = c(0, 18)) [1:length(rescaled_all_respondets_df$Pesticide_type)]

rescaled_all_respondets_df$Pest_products <- 
  rescale(c(rescaled_all_respondets_df$Pest_products, 0, 4),
          to = c(0, 14)) [1:length(rescaled_all_respondets_df$Pest_products)]

rescaled_all_respondets_df$Additives <- 
  rescale(c(rescaled_all_respondets_df$Additives, 0, 1),
          to = c(0, 5)) [1:length(rescaled_all_respondets_df$Additives)]

rescaled_all_respondets_df$Pest <- 
  rescale(c(rescaled_all_respondets_df$Pest, 0, 20),
          to = c(0, 14)) [1:length(rescaled_all_respondets_df$Pest)]

rescaled_all_respondets_df$Risk_bees <- 
  rescale(c(rescaled_all_respondets_df$Risk_bees, 0, 4),
          to = c(0, 6)) [1:length(rescaled_all_respondets_df$Risk_bees)]

rescaled_all_respondets_df$Risk_human <- 
  rescale(c(rescaled_all_respondets_df$Risk_human, 0, 4),
          to = c(0, 6))[1:length(rescaled_all_respondets_df$Risk_human)]

rescaled_all_respondets_df$Shops <- 
  rescale(c(rescaled_all_respondets_df$Shops, 0, 15),
          to = c(0, 6)) [1:length(rescaled_all_respondets_df$Shops)]

rescaled_all_respondets_df$Packaging <- 
  rescale(c(rescaled_all_respondets_df$Packaging, 0, 5),
          to = c(0, 6))[1:length(rescaled_all_respondets_df$Packaging)]

rescaled_all_respondets_df$Importance <- 
  rescale(c(rescaled_all_respondets_df$Importance, 0, 5),
          to = c(0, 14))[1:length(rescaled_all_respondets_df$Importance)]

# SUM #
rescaled_all_respondets_df$SUM <- 
  (rowSums(rescaled_all_respondets_df[, 1:11])) 

sapply(rescaled_all_respondets_df, range)

PES_index_df <- rescaled_all_respondets_df

#save(PES_index_df, file = "PES_index_df.RDA")

###################################################
library(lattice)
windows()
histogram(PES_index_df$SUM, nint = 200)

pdf("Histograms_pesticide_all_respondents_index.pdf", width = 12, height = 8)
par(mfrow = c(3, 4))
sapply(PES_index_df[,c(1:11, 13)], function(x){
  print(hist(x))
})
dev.off()

histogram(PES_index_df$SUM)
