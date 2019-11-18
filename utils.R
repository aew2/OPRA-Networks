#Some utility/helper functions for the peer grading simulations
#Author: drew waters (email: aew2@rice.edu)
#Date Created: 17 Feb 2018
#Date Modified: 17 Feb 2018

require(tidyverse)
require(msm)
require(miscTools)

repmat = function(X, m, n) {
  X <- as.matrix(X)
  mx = dim(X)[1]
  nx = dim(X)[2]
  matrix(t(matrix(X, mx, nx * n)), mx * m, nx * n, byrow = T)
}

get_all_files <- function(datapath) {
  subfolders <- list.dirs(datapath)
  files = c()
  for (sub in subfolders) {
    files <- c(files, paste(sub, list.files(sub, pattern='.csv'), sep='/'))
  }
  return (files[grepl('.csv', files)])
}


run_simulation <- function(file, trials, params) {
  
  #Get the data, parse out simple file name, K, and N
  data <- as.matrix(read.csv(file, header=FALSE))
  if (dim(data)[1] != dim(data)[2]) {  #Sanity check on bad file format
    return(data.frame())
  }
  N <- dim(data)[1]
  split_name <- str_split(sub('.csv', '', file), '/')
  GraphName_ <- split_name[[1]][length(split_name[[1]])]
  cat(GraphName_)
  cat('\n')
  K <- as.numeric(str_split(split_name[[1]][length(split_name[[1]])-1], 'K')[[1]][2])
  
  results_sim <- data.frame()
  
  set.seed(params$RandomSeed)
  
  for (tt in 1:trials) {

    #Randomly generate the rater/item parameters
    g <- generate_user_intellegence(N, params$g_factor, params$sigma_1, params$Distrib)
    q <- generate_item_quality(g, params$sigma_2)
    P <- generate_item_scores(g, q, data, params$a, params$b, params$c)
    R <- generate_item_ranks(P)
    
    #Evaluate the ranking fidelity via kendall's tau
    rank_mean_agg <- colMeans(R, na.rm=TRUE)
    rank_median_agg <- colMedians(R, na.rm=TRUE)
    kt_mean <- cor(q, rank_mean_agg, method='kendall')
    kt_median <- cor(q, rank_median_agg, method='kendall')
    
    #Evaluate the rating fidelity (Pearson and MAE)
    score_mean_est <- colMeans(P, na.rm=TRUE)
    score_median_est <- colMedians(P, na.rm=TRUE)
    MAE_mean <- mean(abs(q-score_mean_est))
    MAE_median <- mean(abs(q-score_median_est))
    pearson_mean <- cor(q, score_mean_est)
    pearson_median <- cor(q, score_median_est)
    
    result = data.frame(params)
    result$GraphName_ <- GraphName_
    result$trial <- tt
    result$N <- N
    result$K <- K
    result$kt_mean <- kt_mean
    result$kt_median <- kt_mean
    result$MAE_mean <- MAE_mean
    result$MAE_median <- MAE_mean
    result$pearson_mean <- pearson_mean
    result$pearson_median <- pearson_mean
    results_sim <- rbind(results_sim, result)
    }
  return(results_sim)
}


generate_user_intellegence <- function(N, mu_g, sigma_g, type) {
  if (type == 'tn') {
    g <- rtnorm(N, mu_g, sigma_g, lower=0)
  } else { #lognormal if not truncated for now!
    g <- rlnorm(N, log(mu_g), log(sigma_g))
  }
  return(g)
}

generate_item_quality <- function(g, sigma) {
  sigma_q <- rtnorm(length(g), sigma, 1, lower=0)
  q <- rtnorm(length(g), g, sigma_q, lower=0)
  return(q)
}

generate_item_scores <- function(g, q, A, a, b, c) {
  N <- length(g)
  sigma_scores <- matrix(( a / (g - b )) ^ c, nrow=N)
  Sigma <- repmat(sigma_scores, 1, N);
  Q <- repmat(matrix(q, nrow=1), N, 1)
  Qv <- matrix(Q, nrow=N^2)
  Sv <- matrix(Sigma, nrow=N^2) 
  Pv <- rtnorm(N^2, Qv, Sv, lower=0)
  P <- matrix(Pv, nrow=N)
  P <- P * A
  P[P==0] = NA
  return(P)
}

generate_item_ranks <- function(S) {
  #Matrix R is of shape (Users x Items)
  #So iterate by rows, arg sort, and return
  
  N <- dim(S)[1]
  R <- S
  
  for (nn in 1:N) {
    R[nn, !is.na(R[nn,])] <- order(order(R[nn, !is.na(R[nn,])])) #Double order is faster than rank!
  }
  
  return(R)
}
