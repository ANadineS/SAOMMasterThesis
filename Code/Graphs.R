###
# Create graphs to analyse parameter outcomes per error combination
###

### Graph creation
CreateGraph <- function(datasummary, data = NULL, variable, error_neg = 0.1, error_pos = 0){
  # Select relevant data for selected variable and error combinations
  data <- subset(data, Error_Neg %in% error_neg & Error_Pos == error_pos)
  realvalue <- datasummary[datasummary$Parameter == variable, "Real_Theta"]
  realse <- datasummary[datasummary$Parameter == variable, "Real_SE"]
  
  # Create a density pot for the relevant variable for the relevant error values
  plot <- ggplot(data, aes(x = !!sym(variable), group = Error_Neg, fill = as.factor(Error_Neg))) +
    geom_density(alpha = 0.5, size = 0) +
    geom_vline(xintercept = realvalue) +
    labs(y = "Density",
         fill = "False negative \n probability") +
    theme_minimal()
    
  return(plot)
}

### Save created graphs
SaveGraph <- function(variable, error_pos, directory){
  # Create graph and safe to given directory
  directory <- paste0(directory, variable, "_", error_pos, ".jpg")
  
  CreateGraph(datasummary = random_results$summary_theta, data = random_results$filtered_output$theta, 
              variable = c(variable), error_neg = c(0, 0.05, 0.1, 0.2, 0.3), error_pos = error_pos)
  
  ggsave(directory, width = 2400, height = 1900, units = "px", dpi = 300)
}
