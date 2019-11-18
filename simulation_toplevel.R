#Top level file for running the peer-grading graph simulation
#Author: drew waters (email: aew2@rice.edu)
#Date Created: 17 Feb 2018
#Date Modified: 17 Feb 2018

source('utils.R')
require(itertools)
require(tidyverse)

###USER PARAMETERS -- ONLY CHANGE THESE###
datapath <- './data/processed'
resultspath <- './results/'
T <- 1000
RandomSeed <- c(0)
g_factor <- c(100)
Distrib <- c('tn', 'ln')
sigma_1 <- c(15)
sigma_2 <- c(5, 15)
a <- c(100, 1000, 36000)
b <- c(15)
c <- c(2)


#Get all the files from all directories in datapath
all_files <- get_all_files(datapath)

#Run the simulation, looping over all parameters and files
results_df <- data.frame()
iterator <- ihasNext(product(g_factor=g_factor, Distrib=Distrib, sigma_1=sigma_1, 
                             sigma_2=sigma_2, a=a, b=b, c=c, RandomSeed=RandomSeed))

counter<-0
while (hasNext(iterator)) {
  
  cat(counter)
  counter <- counter + 1
  
  #Get parameter set
  params <- nextElem(iterator)
  
  #Run T trials for each file for the given parameter set
  for (file in all_files) {
    results_temp <- run_simulation(file, T, params)
    results_df <- rbind(results_df, results_temp)
  }
  
}

#Final munge for output . . . aggregate results over trials, format for the master sheet
df_melt <- results_df %>% gather(key=metric_agg, value=value, kt_mean, kt_median, 
                                 MAE_mean, MAE_median, pearson_mean, pearson_median) %>%
                          separate(metric_agg, c("CorrStat", "Aggregat"), "_") 
df_final_summary <- df_melt %>% group_by(g_factor, Distrib, sigma_1, sigma_2, a, b, c, RandomSeed, GraphName_, N, K, CorrStat, Aggregat) %>% 
                    summarise(mean=mean(value), std=sd(value), min=min(value), max=max(value))
df_final_summary$Scale <- df_final_summary$CorrStat=='kt'
df_final_summary$Scale[df_final_summary$Scale==TRUE] <- 'ranking'
df_final_summary$Scale[df_final_summary$Scale==FALSE] <- 'rating'
df_final_summary <- df_final_summary[, c('Distrib', 'g_factor', 'sigma_1', 'sigma_2', 'a', 'b', 'c', 'N', 'K', 'Scale', 'Aggregat', 'CorrStat', 'RandomSeed', 'GraphName_', 'mean', 'std', 'min', 'max')]

#Write the results to disk
current_time = gsub(':', '', gsub('-', '', gsub(' ', 'T', Sys.time())))
out_name <- paste0(current_time, '.csv', sep='')
out_name_summary <- paste0(current_time, '_summary.csv', sep='')
write.csv(results_df, paste0(resultspath, out_name))
write.csv(df_final_summary, paste0(resultspath, out_name_summary))
