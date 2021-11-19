iterate_LDA_k <- function(dtm, n_k = 10, iterations = 200, path = "models/", ...){
  # Generate a series of models with 2:k topics
  
  # Create list of 2:k; having only 1 topic is not very meaningful
  k_list <-  seq(1,n_k, by = 1)
  
  # Generate a subdirectory that is unique to each corpus
  # Create directory, if it does not exist already
  if(!dir.exists(path)) dir.create(path)
  # Setup a function for iterating LDA over k = k_list
  model_list <- TmParallelApply(X = k_list, FUN = function(k., ...){
    # Save each k as a file for viewing later
    filename = file.path(path, paste0(k., "_topics.rda"))
    
    # Create iterations if they do not exist already
    if (!file.exists(filename)){
      # Use textmineR to create an LDA model
      model <- FitLdaModel(dtm = dtm,
                           k = k.,
                           iterations = iterations,
                           alpha = .01,
                           beta = .1)
      
      # Save value of k within model
      model$k <- n_k
      
      # Calculate coherence for each model
      model$coherence <- CalcProbCoherence(phi = model$phi,
                                           dtm = dtm,
                                           M = 5)
      # Write each model to file
      save(model, file = filename)
    } else {
      # If model exists already, load from file
      load(filename)
    }
    # Return model
    model
    
  }, export = c("dtm", "path"))
  model_list <- saveRDS(model_list, paste0(path,"model_list.rds"))
}

find_top_coherence <- function(modelList, path = "models/", ...) {
  
  # Generate a coherence matrix from iterations
  coherence_matrix <- data.frame(
    # Create co_mat from models
    k = sapply(modelList, function(model)
      nrow(model$phi)),
    # Calculate mean coherence
    coherence = sapply(modelList,
                       function(x)
                         mean(x$coherence)),
    stringsAsFactors = F
  )
  coherence_matrix %>% saveRDS(paste0(path, "coherenceMat.rds"))
  
  coherence_matrix

}