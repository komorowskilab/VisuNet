generate_object = function(decs, rules,type, minAcc, minSupp, PercSupp, TopNodes,  NodeColorType,  NewData, NewDataValues){

  if(length(rules$PERC_SUPP_RHS ) >0){
    recRulesFiltr = rules[which(rules$ACC_RHS >= minAcc & rules$PERC_SUPP_RHS >= PercSupp),]
  }else{
    recRulesFiltr = rules[which(rules$ACC_RHS >= minAcc & rules$SUPP_RHS >= minSupp),]
  }
  #print(dim(recRulesFiltr))
  AllNets = NULL
  Net = NULL
  for (i in decs){
    #i = "control"
    recRulesDecs = NULL
    #rules for decision
    RulesDec = recRulesFiltr[which(recRulesFiltr$DECISION ==i),]
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
  if(dim(recRulesFiltr)[1] != 0){
    AllNets[['all']] = generateNet('all', recRulesFiltr, type, dim(recRulesFiltr)[1], TopNodes,  NodeColorType = NodeColorType,
                                   NewData, NewDataValues)
  }else{
    AllNets[['all']] = list(nodes = NULL, edges = NULL, NodeRulesSetPerNode = NULL)
  }
  AllNets[['Rules']] = rules
  return(AllNets)
}
