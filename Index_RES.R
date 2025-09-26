rescale <- scales::rescale

# create a multiple list from the full dataframe

full_df_list <- lapply(1:nrow(full_df), function(x){
  full_df[x, ]
})

##############value lists and max values##############
# Q_32 and Q_33 and Q_34 #
knowledge_value_list <- c(3, 4, 2, 1, 0)
names(knowledge_value_list) <-  c("I know.",
                                  "I know, and I explore them consciously.",
                                  "I do not know them well but I am trying to get to know it better.",
                                  "I do not know, but I want to know.", 
                                  "I do not know.")

#knowledge_max_value = 12

# for Q_92 #
synthetic_pest_value_list <- c(3, 3, 2, 1, 0)
names(synthetic_pest_value_list) <-  c("Highly", "Significantly", 
                                       "Moderately", "Negligibly",
                                       "Not et all")

#############################
#Q_27 To what extent do the "Nature conservation" influence your gardening habit?

values_of_conservation_habit_vector <- sapply(full_df_list, function(x){
  conservation_habit = x[["Q_27"]]
})

#############################
#"Q_32": plants, "Q_33": birds, "Q_34": insects of respondents' garden

values_of_knowledge_vector <- sapply(full_df_list, function(x){
  vector <- as.character(x[c("Q_32", "Q_33", "Q_34")])
  value <- knowledge_value_list[vector]
  sum(value)
  #sum(value)/knowledge_max_value
})

#############################
#"Q_72: How do you support wild pollinators? 
#Multiple choices are allowed

values_of_poll_support_type_vector <- sapply(full_df_list, function(x){
  support_type <- unlist(strsplit(as.character(x["Q_72"]),';'))
  
  if (any(support_type %in% "I do not support them actively"))
  {
    support_value = 0
  }
  else
  {
    if (length(support_type) == 1)
    {support_value = 1}
    else
    {support_value = 2}
  }
  support_value
})

#############################
#"Q_73": Do you think your garden is pollinator-friendly?

values_of_poll_friendly_vector <- sapply(full_df_list, function(x){
  poll_friendly = x["Q_73"]
  case_when(
    poll_friendly == "Yes" ~ 1,
    poll_friendly == "No" ~ 0
  )
})

#############################
#"Q_81": Can you imagine your garden being part of a garden network that helps maintain biodiversity?

values_of_biodiv_network_vector <- sapply(full_df_list, function(x){
  biodiv_network = x["Q_81"]
  case_when(
    biodiv_network == "Yes" ~ 2,
    biodiv_network == "Maybe" ~ 1,
    biodiv_network == "No" ~ 0
  )
})

#############################
#"Q_82": Do you usually submit data on arthropods in your garden to citizen science-based biodiversity recording platforms?

values_of_citizen_science_vector <- sapply(full_df_list, function(x){
  citizen_science = x["Q_82"]
  case_when(
    citizen_science == "1" ~ 0, #No
    citizen_science == "2" ~ 1,
    citizen_science == "3" ~ 2,
    citizen_science == "4" ~ 3,
    citizen_science == "5" ~ 4 #Yes, several times a week
  )
})

#############################
#"Q_83": #No mow may

values_of_nomowmay_vector <- sapply(full_df_list, function(x){
  nomowmay = x["Q_83"]
  case_when(
    nomowmay == "Yes, and I did not mow in May" ~ 1, 
    nomowmay == "Yes, but I did mow in May" ~ -1,
    nomowmay == "I have not heard about this campaign" ~ 0
  )
})

#############################
#Q_92: To what level do you think synthetic pesticides can treaten the listed organisms?

values_of_synthetic_pest_vector <- sapply(full_df_list, function(x){
  documenting = x[["Q_92"]]
  values <- synthetic_pest_value_list[documenting]
})

#############################
#Q_94: How do you learn/gather knowledge/information about gardening?
# Multiple choices are allowed.

values_of_infos_vector <- sapply(full_df_list, function(x){
  infos_type <- unlist(strsplit(as.character(x["Q_94"]),';'))
  if (length(infos_type) == 1)
    {infos_value = 1}
  else
    if (length(infos_type) == 2 | length(infos_type) == 3 | 
        length(infos_type) == 4)
    {infos_value = 2}
    else {
      infos_value = 3
    }
  infos_value
})

#############################
#Q_95: Do you document the development/changes of your garden with pictures, videos, and/or notes?

values_of_documenting_vector <- sapply(full_df_list, function(x){
  documenting = x["Q_95"]
  case_when(
    documenting == "Yes" ~ 1,
    documenting == "No" ~ 0
  )
})

#############################

respondents_index_df <- data.frame(Conserv_habit = values_of_conservation_habit_vector, 
                 Knowledge = values_of_knowledge_vector,
                 Poll_support = values_of_poll_support_type_vector,
                 Poll_friendly = values_of_poll_friendly_vector,
                 Biodiv_network = values_of_biodiv_network_vector,
                 Citizen_sci = values_of_citizen_science_vector,
                 Nomowmay = values_of_nomowmay_vector,
                 Synthetic_pest = values_of_synthetic_pest_vector,
                 Infos = values_of_infos_vector,
                 Documenting = values_of_documenting_vector,
                 ID = full_df$ID)

sapply(respondents_index_df, range)

# Rescale #

rescaled_respondents_index_df <- respondents_index_df

rescaled_respondents_index_df$Conserv_habit <- 
  rescale(c(rescaled_respondents_index_df$Conserv_habit, 1, 5), 
          to = c(0, 15))[1:length(rescaled_respondents_index_df$Conserv_habit)]

rescaled_respondents_index_df$Knowledge <- 
  rescale(c(rescaled_respondents_index_df$Knowledge, 0, 12), 
          to = c(0, 13))[1:length(rescaled_respondents_index_df$Knowledge)]

rescaled_respondents_index_df$Poll_support <- 
  rescale(c(rescaled_respondents_index_df$Poll_support, 0, 2), 
          to = c(0, 12))[1:length(rescaled_respondents_index_df$Poll_support)]

rescaled_respondents_index_df$Poll_friendly <- 
  rescale(c(rescaled_respondents_index_df$Poll_friendly, 0, 1), 
          to = c(0, 5))[1:length(rescaled_respondents_index_df$Poll_friendly)]

rescaled_respondents_index_df$Biodiv_network <- 
  rescale(c(rescaled_respondents_index_df$Biodiv_network, 0, 2), 
          to = c(0, 15))[1:length(rescaled_respondents_index_df$Biodiv_network)]

rescaled_respondents_index_df$Citizen_sci <- 
  rescale(c(rescaled_respondents_index_df$Citizen_sci, 0, 4), 
          to = c(0, 10))[1:length(rescaled_respondents_index_df$Citizen_sci)]

rescaled_respondents_index_df$Nomowmay <- 
  rescale(c(rescaled_respondents_index_df$Nomowmay, -1, 1), 
          to = c(-1, 5))[1:length(rescaled_respondents_index_df$Nomowmay)]

rescaled_respondents_index_df$Synthetic_pest <- 
  rescale(c(rescaled_respondents_index_df$Synthetic_pest, 0, 3), 
          to = c(0, 3))[1:length(rescaled_respondents_index_df$Synthetic_pest)]

rescaled_respondents_index_df$Infos <- 
  rescale(c(rescaled_respondents_index_df$Infos, 1, 3), 
          to = c(1, 12))[1:length(rescaled_respondents_index_df$Infos)]

rescaled_respondents_index_df$Documenting <- 
  rescale(c(rescaled_respondents_index_df$Documenting, 0, 1), 
          to = c(0, 10))[1:length(rescaled_respondents_index_df$Documenting)]

# SUM #
rescaled_respondents_index_df$SUM <- 
  (rowSums(rescaled_respondents_index_df[, 1:10])) 

RES_index_df <- rescaled_respondents_index_df
#save(RES_index_df, file = "RES_index_df.RDA")

############################################################
library(lattice)
windows()
histogram(rescaled_respondents_index_df$SUM, nint = 200)

pdf("Histograms_respondents_index.pdf", width = 12, height = 8)
par(mfrow = c(3, 4))
sapply(rescaled_respondents_index_df[,c(1:10, 12)], function(x){
  print(hist(x))
  })
dev.off()
