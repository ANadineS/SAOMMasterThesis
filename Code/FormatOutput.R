### 
# Function to format the output
###

FormatOutput <- function(output){
  results <- list()
  results$original_networks_1 <- lapply(output, "[[", "original_networks_1")
  results$original_networks_2 <- lapply(output, "[[", "original_networks_2")
  results$analyzed_networks <- lapply(output, "[[", "analyzed_networks")
  results$actualchange <- lapply(output, "[[", "actualchange")
  results$analyzedchange <- lapply(output, "[[", "original_networks")
  results$error_neg <- sapply(output, "[[", "error_neg")
  results$error_pos <- sapply(output, "[[", "error_pos")
  results$error_in_change <- sapply(output, "[[", "error_in_change")
  results$tconv.max <- sapply(output, "[[", "tconv.max")
  
  results$results <- lapply(output, "[[", "results")
  
  results$tconv <- as.data.frame(do.call(rbind, lapply(output, "[[", "tconv"))) %>%
    cbind(., sapply(output, "[[", "error_neg"), sapply(output, "[[", "error_pos"), 
          sapply(output, "[[", "error_in_change"))
  
  colnames(results$tconv) <-  
    c("Density", "Reciprocity", "Transitivity-Reciprocity", "3-cycles", "GWESP", 
      "Indegree Popularity", "Indegree Activity", "Homophily-Sex", "Error_Neg", 
      "Error_Pos", "Error_Total")
  
  results$theta <- as.data.frame(do.call(rbind, lapply(output, "[[", "theta"))) %>%
    cbind(., sapply(output, "[[", "error_neg"), sapply(output, "[[", "error_pos"), 
          sapply(output, "[[", "error_in_change"))
  
  results$se <- as.data.frame(do.call(rbind, lapply(output, "[[", "se"))) %>%
    cbind(., sapply(output, "[[", "error_neg"), sapply(output, "[[", "error_pos"), 
          sapply(output, "[[", "error_in_change"))
  
  colnames(results$theta) <- colnames(results$se) <-  
    c("Rate", "Density", "Reciprocity", "Transitivity-Reciprocity", "3-cycles", "GWESP", 
      "Indegree Popularity", "Indegree Activity", "Homophily-Sex", "Error_Neg", 
      "Error_Pos", "Error_Total")
  
  results$wald <- results$theta[,-c(10:12)]/results$se[,-c(10:12)]
  results$p <- apply(as.matrix(results$wald), c(1,2), function(t) pnorm(abs(t), lower.tail = F)) %>%
    cbind(., sapply(output, "[[", "error_neg"), sapply(output, "[[", "error_pos"), 
          sapply(output, "[[", "error_in_change")) %>%
    as.data.frame()
  
  colnames(results$p)[10:12] <- c("Error_Neg", "Error_Pos", "Error_Total")
  
  return(results)
}

