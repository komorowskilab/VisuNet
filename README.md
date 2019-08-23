# VisuNet: an interactive tool for network visualization of rule-based models in R

VisuNet is implemented as an R package and using Shiny Gadget properties for rule network visualization of rule-based models. 

## Example

```` 
devtools::install_github("komorowskilab/VisuNet")

require(VisuNet)

#Sample rule set for a classifier of autisic and non-autiscic young males
#'Line by line' data type
data(autcon_ruleset)

#Run VisuNet
#Remember to click DONE once you finished your work with the network
vis_out = visunet(autcon_ruleset, type = 'L')

#############################
#Rule network object customization

#Nodes customization
#Mark autism-related genes as stars
aut_genes <- c('TSPOAP1', 'COX2','NCS1','RHPN1','FLRT2',
              'BAHD1','NCKAP5L','PPOX', 'NGR2',
              'ATXN8OS','DEPDC1')

#Extract rule network object for nodes
nodes_RNO <- vis_out$all$nodes

#Create the new vector of variables: shape
nodes_RNO$shape <- rep('dot', length(nodes_RNO$label))

#Find autism-related genes and mark them as starts 
nodes_RNO$shape[which(as.character(nodes_RNO$label) %in% aut_genes)] <- 'star'

#Create the node object list
#first variable nodes: customized rule network object for nodes
#second variable CustCol: vector of modified/added variables
nodesL <- list(nodes = nodes_RNO,CustCol =  c('shape'))

#Rerun VisuNet with a new shape of nodes
vis_out2 <- visunet(rules, CustObjectNodes = nodesL)
````
