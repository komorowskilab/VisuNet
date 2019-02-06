generate_object = function(decs, rules,type,  NodeColorType,  NewData, NewDataValues){

  #print(dim(recRulesFiltr))
  AllNets = NULL
  Net = NULL
  RulesDec = NULL
  for (i in decs){
    #i = "control"
    RulesDec = NULL
    #rules for decision
    RulesDec = rules[which(rules$DECISION == i),]
    #rules = recRulesDecs
    #rules filtration
    #.......#
    #generate nodes and edges for decision
    print(i)
    print((dim(RulesDec)))
    # print(RulesDec)
    RulesSetSize=dim(RulesDec)[1]
    if(RulesSetSize != 0){
      print(dim(RulesDec))
      Net = generateNet(i, RulesDec, type, RulesSetSize, NodeColorType = NodeColorType, NewData, NewDataValues)
    }else{
      Net = list(nodes = NULL, edges = NULL, NodeRulesSetPerNode = NULL)
    }

    #nodes = list(id =  nodesMatrix[,'id'], label =  nodesMatrix[,"label"])
    AllNets[[i]] = Net
    Net =  NULL
    #print(nodes)
  }

  AllNets[['all']] = generateNet('all', rules, type, dim(rules)[1], NodeColorType = NodeColorType ,NewData, NewDataValues)
  AllNets[['Rules']] = rules
  return(AllNets)
}
