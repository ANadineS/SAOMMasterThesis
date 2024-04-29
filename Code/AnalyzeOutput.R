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
  
  for (i in 1:nrow(error_combo)){
    indices <- ((i-1)*(n)+1):n*i
    obs_1 <- lapply(output$original_networks[indices], function(arr) arr[,,1])
    obs_2 <- lapply(output$original_networks[indices], function(arr) arr[,,2])
    an_1 <- lapply(output$analyzed_networks[indices], function(arr) arr[,,1])
    an_2 <- lapply(output$analyzed_networks[indices], function(arr) arr[,,2])
    
    View(NetworkSummary(obs_2))
  }
}