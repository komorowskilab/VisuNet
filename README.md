# VisuNet: an interactive tool for rule network visualization in R

VisuNet is implemented as an R package and using Shiny Gadget properties for rule network visualization of rule-based models. 

## Example

```` 
devtools::install_github("komorowskilab/VisuNet"")

require(VisuNet)

#Create a rule-based classifier using sample autism-control dataset



nodes <- data.frame(id = 1:3)
edges <- data.frame(from = c(1,2), to = c(1,3))
visNetwork(nodes, edges)

# vignette
vignette("Introduction-to-visNetwork")

# full javascript documentation
visDocumentation()

# shiny example
shiny::runApp(system.file("shiny", package = "visNetwork"))
````
