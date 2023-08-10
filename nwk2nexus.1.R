### Transfer newick to nexus
### nwk2nexus.1.R
### v1 2023/02/13
### transfer newick to the right nexus format (tip translated to token)
### @author:Chen Yu Chi

library(phytools)

script_path <- normalizePath("nwk2nexus.1.R")
work_path <- substr(script_path,1,nchar(script_path)-13)
setwd(work_path)
tree <- read.tree(file = "phy.nwk")
writeNexus(tree, "phy.txt")

