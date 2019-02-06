NodeColor = function(nodes, NodeColor){
  if(NodeColor == 'A'){
    names(nodes)[names(nodes) == 'color.backgroundAcc'] <- 'color.background'
  }  else if(NodeColor == 'GE') {
    names(nodes)[names(nodes) == 'color.backgroundGE'] <- 'color.background'
  }
  return(nodes)
}
