#This model will first sample the PAT intercept and the use that information
#to set a prior on the intercept

Sys.setLanguage("en")
set.seed(33)

#Load packages
library(tidyverse)
library(ggrepel)
library(nflreadr)
library(nflplotR)
require(rstan)

#Load R Support functions for the project
source("C:/Users/jonas/Desktop/Dokumente/STAT447_Project/All Project Files R/support_functions.R")

#Load both Stan models
#Model to sample intercept
stan_model_pat <- stan_model(file = "C:/Users/jonas/Desktop/Dokumente/STAT447_Project/All Project Files R/pat.stan")

#Model to sample slope
second_stage_model <- stan_model(file = "C:/Users/jonas/Desktop/Dokumente/STAT447_Project/All Project Files R/second_stage.stan")

#-------------------------------------------------

#Create the sampler
mc_intercept_slope_sampler <- function(full_season_data, n_iter) {
  #Initialize vectors
  intercept <- c()
  #intercept_var <- c()
  slope <- c()
  #slope_mean <- c()
  
  #Loop for each player
  for (i in 1:length(full_season_data$player_name)) {
    data <- player_season(full_season_data, i)
    
    fit <- sampling(stan_model_pat,
                    data = list(n= data[1,1], k= data[2,1]),
                    iter = n_iter,
                    chains = 1,
                    set.seed(33))
    fit_extract <- extract(fit)
    fit_extract_mean <- as.numeric(mean(fit_extract$p))
    mean <- qlogis(fit_extract_mean)
    
    print(mean)
    intercept <- c(intercept, mean) #Adjust intercept 
    #intercept_var <- c(intercept_var, var(fit_extract$p))
    
    # Sample the slope
    
    fit <- sampling(second_stage_model,
                    data = list(mean = intercept[i] ,
                                N = length(data[1,])-1,
                                distance = data[1, -1],
                                indicator = data[2, -1] ),
                    iter = n_iter,
                    chains = 1,
                    set.seed(33))
    
    fit_extract <- extract(fit)
    
    slope <- c(slope, mean(fit_extract$slope))
    #slope_var <- c(slope_var, var(fit_extract$slope))
    
    
  }
  return(data.frame(full_season_data$player_name, intercept,
                    slope))
}

#--------------------------------

#Start the analysis on Multiple seasons

complete_analysis <- function(years, n_iter) {
    
    #Initialize Vectors
    current_data <- c()
    results <- list()
    parameters <- data.frame()
    
    #Loop over Seasons
    for (i in seq_along(years)){
      #Get data and clean it
      current_data <- load_pbp(years[i])
      current_data <- calculate_player_stats_kicking(current_data, weekly = FALSE)
      current_data <- season_data(current_data)
      
      #Sample
      parameters <- mc_intercept_slope_sampler(current_data, n_iter)
      
      #Add to the return DF
      results[[paste0(years[i], "_Season")]] <- parameters
    }
    
    return(results)
  }

years <- c(2023, 2022, 2021, 2020, 2019, 2018, 2017, 2016)

result_data <- complete_analysis(years, 10000)
write.csv(result_data, "result_data", row.names = FALSE)

