### Plot ancestral reconstruction result
### anc_plt.1.R
### v1 2023/02/13
### Plot ancetral reconstruction result
### @author:Chen Yu Chi

# install.packages(c("phytools", "dplyr", "tidyverse", "ggplot2", "tidytree"))

library(phytools)
library(ggtree)
library(tidyverse)
library(ggplot2)
library(tidytree)
library(dplyr)

script_path <- normalizePath("anc_plt.1.R")
work_path <- substr(script_path,1,nchar(script_path)-11)
setwd(work_path)

sphy <- ape::read.tree("phy.nwk")
dat <- read.csv("dat.txt", sep = "\t", header = F)
res <- read.table("results.txt", sep = "\t", header=T)
res <- res[-1,]

sphy$edge.length <- res$Original.BL * res$Mean.Scalar

x <- dat$V2
names(x) <- dat$V1

#sanc <- fastAnc(sphy, x)
sanc <- ace(x, sphy, type="continuous", method="pic")

val_node <- sanc$ace
val_node[is.nan(val_node)] <- 0

# pdf("test.pdf", width = 7, height = 7)
# contMap(sphy, log(x+1), method="user", anc.states=log(val_node+1), type="fan", fsize = c(0.2,1)) # counts transformed here only to facilitate plotting
# dev.off()

# Create data frame for traits on tip and node
tip_data <- data.frame(node = nodeid(sphy, names(x)), trait = x)
node_data <- data.frame(node = names(val_node), trait= val_node)

all_data <- rbind(tip_data, node_data)
all_data$node <- as.numeric(all_data$node)
all_data$trait <- as.numeric(all_data$trait)
tre_color <- full_join(sphy, all_data, by = 'node')

# rename based on table
tip_label <- read.csv("../name_replace.lst", sep = "\t", header = F)
names(tip_label) <- c("common_name","abbre_name")
tre_color@phylo$tip.label[match(tip_label$abbre_name, tre_color@phylo$tip.label)] <- tip_label$common_name

# Phylogenetic tree with numt number color gradient
my_breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)
tre_color_plt <- ggtree(tre_color, 
                        aes(color = trait),
                        layout = 'circular',
                        ladderize = FALSE,
                        continuous = 'colour',
                        size = 0.4,
                        branch.length = "none") + 
  scale_color_gradientn(colours = c('dodgerblue2',
                                    'deepskyblue',
                                    'aquamarine',
                                    'indianred1',
                                    'tomato',
                                    'firebrick'),
                        #trans = "pseudo_log",
                        #breaks = my_breaks, 
                        #labels = my_breaks, 
                        guide = "colourbar",
                        limits = range(0, 0.5)
  ) + 
  geom_tiplab(align = TRUE,
              linesize = 0.2,
              size = 2,
              color = "black") + 
  labs(color = "values") +
  theme(legend.title.align = 0.5, 
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        #legend.position = "c(0.8, 0.5)",
        legend.key.size = unit(0.8, "line"),
        aspect.ratio = 1,
        plot.margin = margin(0,0,0,0)) +
  guides(colour = guide_colourbar(title.position = 'top',
                                  barwidth = 0.8)) 

ggsave("phy_plt_0to05.pdf", 
       plot = tre_color_plt,
       height = 220, 
       width = 220,
       units = c("mm"), 
       dpi = 600)

