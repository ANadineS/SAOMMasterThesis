###
# Various helper functions, used in different parts of the simulation & data formatting process
###

### Error introduction functions
# Random error introduction
IntroduceError_Random <- function(matrix, error_neg, error_pos){
  matrix <- apply(matrix, c(1,2), 
                  function(x) ifelse(x == 1, rbinom(1, 1, 1 - error_neg), rbinom(1, 1, error_pos)))
  return(matrix)
}

### Coding of changes between two observations of the same network
ChangeCoding <- function(matrix1, matrix2){
  changematrix <- matrix(NA, nrow(matrix1), nrow(matrix1))
  
  for(i in 1:nrow(matrix1)){
    for (j in 1:ncol(matrix1)){
      if (matrix1[i,j] == 0){
        if (matrix2[i,j] == 0){
          changematrix[i,j] <- 0
        } else {
          changematrix[i,j] <- 1
        } 
      } else {
        if (matrix2[i,j] == 1){
          changematrix[i,j] <- 2
        } else {
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

EdgelistToMatrix <- function(edgelist, n = 133){
  graph <- graph_from_edgelist(as.matrix(edgelist[,-3]), directed = T)
  adj_matrix <- as.matrix(get.adjacency(graph, sparse = F))
  #colnames(adj_matrix) <- rownames(adj_matrix) <- 1:n
  
  adj_matrix_all <- matrix(0, nrow = n, ncol = n, dimnames = list(1:n, 1:n))
  adj_matrix_all[row.names(adj_matrix), colnames(adj_matrix)] <- adj_matrix
  
  return(adj_matrix)
}
