# Original code written by Dr. Dave Campbell, Carleton University

library(tools)

loadPackages = function(packages){
  
  # Check each passed package for required dependencies and add to list of packages to load
  dependencies = NULL
  for (pkg in packages){
    dependencies = c(dependencies, unlist(package_dependencies(pkg, recursive=FALSE, which=c("Depends", "Imports", "LinkingTo"))))
  }
  toLoad = rev(unique(c(packages, dependencies)))
  
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
      
      # Load from local directory
      library(pkg, lib.loc=getwd(), character.only=TRUE, logical.return=TRUE)
      print(paste0("Package '", pkg, "' loaded from local directory"))
    }
  }
}
