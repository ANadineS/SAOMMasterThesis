###
# Create grapsh for analysis
###

CreateGraph <- function(datasummary, data = NULL, variable, type, error_neg = 0.1, error_pos = 0){

  if (type == "density"){
    data <- subset(data, Error_Neg %in% error_neg & Error_Pos == error_pos)
    realvalue <- datasummary[datasummary$Parameter == variable, "Real_Theta"]
    
    plot <- ggplot(data, aes(x = !!sym(variable), group = Error_Neg, fill = as.factor(Error_Neg)))+
      geom_density(alpha = 0.5, size = 0)+
      geom_vline(xintercept = realvalue)+
      labs(title = paste("Simulated", variable, "parameters"), ylab = "Density",
           fill = "False negative probability")+
      theme_minimal()
  } else if (type == "lineplot"){
    datasummary <- subset(datasummary, Parameter %in% variable & Error_Pos == error_pos)
    print(datasummary)
    
    plot <- ggplot(datasummary, aes(x = Error_Neg)) +
      theme_minimal() + 
      lims(y = c(-4, 4))

      for (i in 1:length(variable)){
      plot <- plot + 
        geom_line(data = datasummary[datasummary$Parameter == variable[i],], 
                  aes(y = Theta, colour = variable[i])) +
        geom_point(data = datasummary[datasummary$Parameter == variable[i],], 
                   aes(y = Theta, colour = variable[i])) +
        geom_errorbar(data = datasummary[datasummary$Parameter == variable[i],], 
                      aes(ymin = Theta - qnorm(0.975)*SE, ymax = Theta + qnorm(0.975)*SE, 
                          width = 0.01, colour = variable[i]))
    }
  }
  
  return(plot)
}

CreateGraph(datasummary = random_results$summary_theta, data = random_formatted$theta, 
            variable = c("GWESP"), type = "density", error_neg = c(0.1, 0.4))
