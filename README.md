# VisuNet: an interactive tool for rule network visualization in R

VisuNet is implemented as an R package and using Shiny Gadget properties for rule network visualization of rule-based models. 

## Example

```` 
devtools::install_github("komorowskilab/VisuNet"")

require(VisuNet)

#Sample rule set for a classifier of autisic and non-autiscic young males
#'Line by line' data type
data(autcon_ruleset)

#Run VisuNet
#Remember to click DONE once you finished your work with the network
vis_out = visunet(autcon_ruleset, type = 'L)

#############################
#Network object customization

#Nodes customisation
#Mark autism-related genes as stars
#genes reported in databases of autism associations
aut_genes <- c('TSPOAP1', 'COX2','NCS1','RHPN1','FLRT2',
              'BAHD1','NCKAP5L','PPOX', 'NGR2',
              'ATXN8OS','DEPDC1')

#create the new vector of variables: shape
nodes_RNO <- vis_out$all$nodes
nodes_RNO$shape <- rep('dot', length(nodes_RNO$label))
nodes_RNO$shape[which(as.character(nodes_RNO$label) %in% aut_genes)] <- 'star'
nodes_RNO$shape
#create the node object list
nodesL <- list(nodes = nodes_RNO,CustCol =  c('shape'))

#rerun VisuNet with a new shape of nodes
vis_out2 <- visunet(rules, CustObjectNodes = nodesL)

#Edges customisation
#mark the interaction between COX2 and MAP7 genes
edges_RNO <- vis_out2$all$edges
edges_RNO$label2
edges_RNO$arrows <- rep('enabled', length(edges_RNO$label2))
edges_RNO$arrows[which(edges_RNO$label2 == 'COX2=3-MAP7=2')] <- 'to'

#create the edge object list
edgesL <- list(edges = edges_RNO,CustCol =   c('arrows'))

#rerun VisuNet with a new variable
vis_out3 <- visunet(rules, CustObjectNodes = nodesL, CustObjectEdges = edgesL)
````
