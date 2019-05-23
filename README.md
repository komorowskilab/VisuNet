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

````
