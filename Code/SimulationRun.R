###
# Script for one simulation run
###

SimulationRun <- function(IntroduceError_X, obs1, obs2, sex, controls, error_pos, error_neg, simulation_element){
  netw_1 <- IntroduceError_X(obs1, error_neg, error_pos)
  netw_2 <- IntroduceError_X(obs2, error_neg, error_pos)
  
  actualchange <- ChangeCoding(obs1, obs2)
  analyzedchange <- ChangeCoding(netw_1, netw_2)
  error_in_change <- sum(actualchange != analyzedchange)/(nrow(actualchange)^2-length(diag(actualchange)))
  
  friendship <- sienaDependent(array(c(netw_1, netw_2), dim = c(133,133,2)))
  glasgowdata <- sienaDataCreate(friendship, sex2)
  
  effects <- getEffects(glasgowdata)
  effects <- includeEffects(effects, name = "friendship", density, recip, cycle3, gwespFF, transRecTrip, inPop, inAct)
  effects <- includeEffects(effects, name = "friendship", sameX, interaction1 = "sex2")
  
  results <- siena07(controls, data = glasgowdata, effects = effects)
  
  output <- list()
  output$original_networks_1 <- obs1
  output$original_networks_2 <- obs2
  output$analyzed_networks <- array(c(netw_1, netw_2), dim = c(133,133,2))
  output$actualchange <- actualchange
  output$analyzedchange <- analyzedchange
  output$error_neg <- error_neg
  output$error_pos <- error_pos
  output$error_in_change <- error_in_change
  
  output$results <- results
  output$theta <- c(results$rate, results$theta)
  output$se <- c(results$vrate, results$se)
  output$el <- simulation_element
  output$tconv_max <- results$tconv.max
  output$tconv <- results$tconv
  
  return(output)
}
