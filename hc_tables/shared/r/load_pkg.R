# Install and load packages
  package_names <- c('survey','dplyr','foreign')
  lapply(package_names, function(x) if(!x %in% installed.packages()) install.packages(x))
  lapply(package_names, require, character.only=T)
  
  options(survey.lonely.psu='adjust') 
