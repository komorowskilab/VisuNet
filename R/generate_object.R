generate_object = function(decs, rules,type,  TopNodes,  NodeColorType,  NewDataNodes, NewDataEdges){
  AllNets = NULL
  Net = NULL
  for (i in decs){
    #i = "control"
    recRulesDecs = NULL
    #rules for decision
    RulesDec = rules[which(rules$decision ==i),]
    #generate nodes and edges for decision
    RulesSetSize=dim(RulesDec)[1]
    #print(dim(RulesDec))
    if(RulesSetSize != 0){
      Net = generateNet(i, RulesDec, type, RulesSetSize, TopNodes, NodeColorType = NodeColorType, NewDataNodes, NewDataEdges)
    }else{
      Net = list(nodes = NULL, edges = NULL, NodeRulesSetPerNode = NULL)
    }

    AllNets[[i]] = Net
    Net =  NULL
  }
  if(dim(rules)[1] != 0){
    AllNets[['all']] = generateNet('all', rules, type, dim(recRulesFiltr)[1], TopNodes,  NodeColorType = NodeColorType,
                                   NewDataNodes, NewDataEdges)
  }else{
    AllNets[['all']] = list(nodes = NULL, edges = NULL, NodeRulesSetPerNode = NULL)
  }
  return(AllNets)
}
