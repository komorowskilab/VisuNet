generate_object = function(decs, rules,type,  TopNodes,  NodeColorType,  NewData, NewDataValues){

  #print(dim(recRulesFiltr))
  AllNets = NULL
  Net = NULL
  for (i in decs){
    #i = "control"
    recRulesDecs = NULL
    #rules for decision
    RulesDec = rules[which(rules$DECISION ==i),]
    #rules = recRulesDecs
    #rules filtration
    #.......#
    #generate nodes and edges for decision
    #print(i)
   # print((dim(RulesDec)))
    # print(RulesDec)
    RulesSetSize=dim(RulesDec)[1]
    if(RulesSetSize != 0){
      Net = generateNet(i, RulesDec, type, RulesSetSize, TopNodes, NodeColorType = NodeColorType, NewData, NewDataValues )
    }else{
      Net = list(nodes = NULL, edges = NULL, NodeRulesSetPerNode = NULL)
    }

    AllNets[[i]] = Net
    Net =  NULL
    #print(nodes)
  }
  if(dim(rules)[1] != 0){
    AllNets[['all']] = generateNet('all', rules, type, dim(recRulesFiltr)[1], TopNodes,  NodeColorType = NodeColorType,
                                   NewData, NewDataValues)
  }else{
    AllNets[['all']] = list(nodes = NULL, edges = NULL, NodeRulesSetPerNode = NULL)
  }
  #AllNets[['Rules']] = rules
  return(AllNets)
}
