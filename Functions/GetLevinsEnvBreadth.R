### AE Melton, 2020
# Function to get environmental breadths per ENMTools env.breadth function

#
GetLevinsEnvBreadth <- function(ENMEval.Outputs.folder, Species.Layers.folder, max.reps = 1000, tolerance = 1e-05){
  
setwd(ENMEval.Outputs.folder)
output.folder.list <- list.files(pattern = "*", full.names = TRUE) # Make a list of all species output folders
output.folder.list
  
setwd(Species.Layers.folder)
layers.folder.list <- list.files(pattern = "*", full.names = TRUE) # Make a list of all species output folders
layers.folder.list
species.list <- gsub(pattern = "./", replacement = '', x = layers.folder.list)
species.list
  
df <- data.frame(species = character(), env.B2 = character()) # Generate empty data frame to put results in
tmp <- data.frame(species = character(), env.B2 = character()) # Generate empty data frame to put results in
for(i in 92:length(species.list)){
  #
  species.name <- species.list[i]
  print(species.name)
  setwd(Species.Layers.folder)
  setwd(layers.folder.list[i])
  env.files <- list.files(pattern = ".tif", full.names = TRUE)
  envStack <- stack(env.files)
  envStack <- setMinMax(envStack)
  red.envStack <- ReduceEnvPred(usr.env = envStack, stat = "pearson", corr.cutoff = 0.8)
  #
  
  #
  setwd(ENMEval.Outputs.folder)
  setwd(output.folder.list[i])
  
  model.object <- paste0(species.name, "_ENMEval.RDA")
  load(model.object)
  csv <- paste0(species.name, "CandidateModelResults.csv")
  mod.num <- as.numeric(read.csv(file = csv)[1,1])
  #
  
  #
  tmp.breadth <- rbind(env.breadth(model = modeval@models[[mod.num]], env = red.envStack, max.reps = 1000, tolerance = 1e-05))
  species <- species.name
  tmp <- cbind(Species = species, env.B2 = tmp.breadth[1,1])
  #
  
  #
  df <- rbind(df, tmp)
  }
return(df)
}
