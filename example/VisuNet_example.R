#VisuNet installation
devtools::install_github('komorowskilab/VisuNet')

#load package
library(VisuNet)

#sample autism-control decision table
View(head(autcon))

#creating the rule-based classifier using ROSETTA from the R.ROSETTA package
autcon_rules <- rosetta(autcon)

#rule-based model quality
View(autcon_rules$quality)

#input rule data
rules <- autcon_rules$main

#required data variables
rules = rules[,c("FEATURES","DISC_CLASSES","DECISION",
                 "SUPP_RHS","ACC_RHS","PVAL")]
View(head(rules))

#Run  VisuNet
vis_out <- visunet(rules)
#input types: R.ROSETTA data.frame ("RDF"),
#ROSETTA GUI output ("RGUI") or Line by line format ("L").
#Node color type: discretization levels ("DL")
#or accuracy value ("A")


View(head(vis_out$all$nodes))

#Nodes customisation
#mark autism-related genes as stars
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

