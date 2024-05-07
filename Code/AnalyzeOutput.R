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

AnalyzeOutput <- function(output, realmodel, n, error_combo){
  stats_per_network <- vector("list", nrow(error_combo))
  
  for (i in 1:nrow(error_combo)){
    print(error_combo[i,])
    indices <- ((i-1)*(n)+1):(n*i)
    
    stats_error_combo <- list(
      obs_1 = NetworkSummary(output$original_networks_1[indices]),
      obs_2 = NetworkSummary(output$original_networks_2[indices]),
      an_1 = NetworkSummary(lapply(output$analyzed_networks[indices], function(arr) arr[,,1])),
      an_2 = NetworkSummary(lapply(output$analyzed_networks[indices], function(arr) arr[,,2]))
    )
    
    stats_per_network[[i]] <- setNames(stats_error_combo, paste0("Error_", error_combo[i,1], "_", error_combo[i,2]))
  }
}