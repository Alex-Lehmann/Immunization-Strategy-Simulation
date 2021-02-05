# Original code written by Dr. Dave Campbell, Carleton University

library(tools)

loadPackages = function(packages){
  i = 1
  dependencies = NULL
  for (pkg in packages){
    # Check each passed package for required dependencies and add to list of packages to load
    dependencies = c(dependencies, unlist(package_dependencies(packages[i], recursive=FALSE, which=c("Depends", "Imports", "LinkingTo"))))
  }
  
  # while(i <= length(packages)){
  #   
  #   # Check each passed package for required dependencies and add to list of packages to load
  #   dependencies = c(dependencies, unlist(package_dependencies(packages[i], recursive=FALSE, which=c("Depends", "Imports", "LinkingTo"))))
  #   i = i + 1
  # }
  toLoad = rev(unique(c(packages, dependencies)))
  print(toLoad)
  # Attach required packages
  for (pkg in toLoad){ # Try to load from global library
    
    if (pkg %in% installed.packages()){
      library(pkg, character.only=TRUE)
      print(paste0("Package '", pkg, "' loaded from global library"))
    
    } else { # If not in global library, use local directory
      if (!(pkg %in% installed.packages(lib=getwd()))){ # Install to local directory if not present
        print(paste0("Package '", pkg, "' not found; installing to local directory"))
        install.packages(pkg, lib=getwd())
      }
      
      library(pkg, lib.loc=getwd(), character.only=TRUE) # Load from local directory
      print(paste0("Package '", pkg, "' loaded from local directory"))
    }
  }
}
