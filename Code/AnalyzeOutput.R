###
# Function to analyze output
###

NetworkSummary <- function(networklist){
  numerics <- lapply(networklist, function(x) matrix(as.numeric(x), nrow = nrow(x)))
  netwobjects <- lapply(numerics, function(x) 
    network(x, directed = T, matrix.type = "adjacency"))
  
  netwstats <- data.frame(
    "Network size" = mean(sapply(netwobjects, network.size)),
    "Density" = mean(sapply(netwobjects, gden)),
    "Components" = mean(sapply(netwobjects, components)),
    "Transitivity" = mean(sapply(netwobjects, function(x) gtrans(x, mode = "graph"))),
    "Reciprocity" = mean(sapply(netwobjects, function(x) grecip (x, measure = "edgewise")))
  )
  
  return(netwstats)
}

AnalyzeOutput <- function(output, realmodel, n, error_combo){
  
  for (i in 1:ncol(error_combo)){
    indices2 <- ((i-1)*(n)+1):n*i
    indices <- 1:75
    obs_1 <- lapply(output$original_networks[indices], function(arr) arr[,,1])
    obs_2 <- lapply(output$original_networks[indices], function(arr) arr[,,2])
    an_1 <- lapply(output$analyzed_networks[indices], function(arr) arr[,,1])
    an_2 <- lapply(output$analyzed_networks[indices], function(arr) arr[,,2])
    
    print(NetworkSummary(obs_1))
  }
}