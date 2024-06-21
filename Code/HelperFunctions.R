###
# Various helper functions, used in different parts of the simulation & data formatting process
###

### Error introduction functions
# Random error introduction
IntroduceError_Random <- function(matr, error_neg, error_pos) {
  matr <- apply(matr, c(1, 2), function(cell) {
    if (cell == 1) {
      # Introduce a false negative with probability error_neg
      rbinom(1, 1, 1 - error_neg)
    } else {
      # Introduce a false positive with probability error_pos
      rbinom(1, 1, error_pos)
    }
  })
  
  # Remove any self-loops that may have been introduced
  diag(matr) <- 0
  
  return(matr)
}

### Coding of changes between two observations of the same network
ChangeCoding <- function(matrix1, matrix2){
  changematrix <- matrix(NA, nrow(matrix1), nrow(matrix1))
  
  # Check for each cell in matrix 1 and 2 what kind of change occurred
  for(i in 1:nrow(matrix1)){
    for (j in 1:ncol(matrix1)){
      if (matrix1[i,j] == 0){
        if (matrix2[i,j] == 0){
          
          # Tie does not exist in both matrices
          changematrix[i,j] <- 0
        } else {
          
          # A new tie was created in matrix 2 that did not exist in matrix 1
          changematrix[i,j] <- 1
        } 
      } else {
        if (matrix2[i,j] == 1){
          
          # Tie exists in both matrices
          changematrix[i,j] <- 2
        } else {
          
          # A tie was removed in matrix 2 that existed in matrix 1
          changematrix[i,j] <- 3
        }
      }
    }
  }
  
  return(changematrix)
}

### Hamming distance
hamming <- function(net1,net2) {
  tbl <- table(c(0,0,1,1,net1),c(0,1,0,1,net2))-1
  return(tbl[1,2]+tbl[2,1])
}


### Jaccard index
jaccard <- function(net1,net2) {
  tbl <- table(c(0,0,1,1,net1),c(0,1,0,1,net2))-1
  return(tbl[2,2]/(tbl[1,2]+tbl[2,1]+tbl[2,2]))
}


### Function to transform edgelists to adjacency matrices
EdgelistToMatrix <- function(edgelist, n = 133){
  
  # Add self-loops to edgelist to make sure isolated actors are also included
  self_loops <- matrix(c(1:133, 1:133, rep(1, 133)), ncol = 3, nrow = 133, byrow = F)
  final_edgelist <- rbind(self_loops, edgelist)
  
  # Transform edgelist to adjacency matrix
  graph <- graph_from_edgelist(as.matrix(final_edgelist[,-3]), directed = T)
  adj_matrix <- as.matrix(as_adjacency_matrix(graph))
  diag(adj_matrix) <- 0 # Remove self-loops
  
  return(adj_matrix)
}

