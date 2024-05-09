###
# Function to analyze output
###

NetworkSummary <- function(networklist){
  netwobjects <- lapply(networklist, function(x) 
    network(as.matrix(x), directed = T, matrix.type = "adjacency"))
  
  netwstats <- data.frame(
    "Network size" = sapply(netwobjects, network.size),
    "Density" = sapply(netwobjects, gden),
    "Components" = sapply(netwobjects, components),
    "Transitivity" = sapply(netwobjects, function(x) gtrans(x, mode = "graph")),
    "Reciprocity" = sapply(netwobjects, function(x) grecip (x, measure = "edgewise"))
  )
  
  return(netwstats)
}

SummarizeParameters <- function(theta, se, error_combo, realmodel){
  # Select data belonging to a certain error combination
  theta_to_use <- theta[theta$Error_Neg == error_combo[1] & theta$Error_Pos == error_combo[2], 1:9]
  se_to_use <- se[se$Error_Neg == error_combo[1] & se$Error_Pos == error_combo[2], 1:9] 
  
  # True model results
  realtheta <- c(realmodel$rate, realmodel$theta)
  realtheta_se <- c(realmodel$vrate, realmodel$se)
  
  # Calculate the statistics on data belonging to a error combination
  stat <- data.frame(
    Error_Neg = rep(error_combo[1], 9),
    Error_Pos = rep(error_combo[2], 9),
    Real_Theta = realtheta,
    Real_SE = realtheta_se,
    Theta = sapply(theta_to_use, mean),
    SE = sapply(se_to_use, mean),
    Bias = colMeans(theta_to_use) - realtheta,
    Variance = sapply(theta_to_use, var),
    MSE = colMeans((theta_to_use - realtheta)^2),
    Precision_Change = apply(theta_to_use, 2, sd)/realtheta_se,
    Coverage = sapply(1:9, function(i) {
      low <- theta_to_use[,i] - qnorm(0.975)*se_to_use[,i]
      high <- theta_to_use[,i] + qnorm(0.975)*se_to_use[,i]
      mean(realtheta[i] >= low & realtheta[i] <= high)
    }),
    Power = sapply(1:9, function(j){
      wald <- theta_to_use[,j]/se_to_use[,j]
      mean(wald <= qnorm(0.025) | wald >= qnorm(0.975))
    })
  )
  
  return(stat)
}

AnalyzeOutput <- function(output, realmodel, n, error_combo){
  stats_per_network <- vector("list", nrow(error_combo))
  
  for (i in 1:nrow(error_combo)){
    indices <- ((i-1)*(n)+1):(n*i) # indices belonging to the error-combination
    
    # Calculate network statistics for each network
    stats_error_combo <- list(
      obs_1 = NetworkSummary(output$original_networks_1[indices]),
      obs_2 = NetworkSummary(output$original_networks_2[indices]),
      an_1 = NetworkSummary(lapply(output$analyzed_networks[indices], function(arr) arr[,,1])),
      an_2 = NetworkSummary(lapply(output$analyzed_networks[indices], function(arr) arr[,,2]))
    )
    
    stats_per_network[[i]] <- setNames(stats_error_combo, c("obs1", "obs2", "analyzed1", "analyzed2"))
    names(stats_per_network)[i] <- paste0(error_combo[i,1], "_", error_combo[i,2])
  }
  
  # Summarize network statistics per error combination & network type
  summary_networkstats_mean <- lapply(c(1:nrow(error_combo)), function(x) {
    stats <- data.frame(
      Error_Neg = rep(error_combo[x,1], 4),
      Error_Pos = rep(error_combo[x,2], 4),
      Density = unlist(lapply(stats_per_network[[x]], function(y) mean(y$Density))),
      Components = unlist(lapply(stats_per_network[[x]], function(y) mean(y$Components))),
      Transitivity = unlist(lapply(stats_per_network[[x]], function(y) mean(y$Transitivity))),
      Reciprocity = unlist(lapply(stats_per_network[[x]], function(y) mean(y$Reciprocity)))
    )
  }) %>% do.call(rbind, .)
  
  # Summarize SAOM results per error combination
  summary_theta_mean <- apply(error_combo, 1, function(x) 
    SummarizeParameters(output$theta, output$se, x, realmodel)) %>%
    do.call(rbind, .)
  
  # Combine results
  results <- list(stats_per_network, summary_networkstats_mean, summary_theta_mean)
  names(results) <- c("stats_per_network", "summary_networkstats", "summary_theta")
  return(results)
}
