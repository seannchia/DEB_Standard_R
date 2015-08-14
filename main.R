#---------------------------------------------------------------
# Compute model predictions for one individual,
#   with standard DEB equations (Kooijman 2010)
# 
# Parameter estimates for Torpedo marmorata
#
# from the egg to adult stage
# one reproduction event every year
#(shrinking of structural volume is possible in starvation condition)
# 
# simu : structure with individual features and model predictions
#
# calls :   init.m
#           
#           food.m, temp.m
#           get_obs.m, get_plots
#
# 2013/03/15 - Laure Pecquerie
#---------------------------------------------------------------  

rm(list=ls()) # Clear the workspace of everything
scriptdir = dirname(sys.frame(1)$ofile) # Get script's working directory
source(file.path(scriptdir,"sourceDir.R"), chdir = TRUE) # Load the script to load all the R functions in the directory
sourceDir(scriptdir, trace=FALSE) # Run the function to load all the functions in the directory
 

# Remove all variables and parameters from the workspace, leaving functions
rm(list = setdiff(ls(), lsf.str()))

# 1 - initialize time, parameters, etc
simu<-init()

# 2 - calculate state variables
simu$tEVHR<-indiv(simu)

# 3 - calculate observable quantities
simu$obs<-get_obs(simu)

## 4 - make plots
get_plots(simu)

