generate_object = function(decs, rules,type, minAcc, minSupp, PercSupp, NodeColorType,  NewData, NewDataValues){

  if(length(rules$PERC_SUPP_RHS ) >0){
    recRulesFiltr = rules[which(rules$ACC_RHS >= minAcc & rules$SUPP_RHS >= minSupp & rules$PERC_SUPP_RHS >= PercSupp),]
  }else{
    recRulesFiltr = rules[which(rules$ACC_RHS >= minAcc & rules$SUPP_RHS >= minSupp),]
  }


  AllNets = NULL
  for (i in decs){
    #i = "control"
    recRulesDecs = NULL
    #rules for decision
    RulesDec = recRulesFiltr[which(recRulesFiltr$DECISION ==i),]
    #rules = recRulesDecs
    #rules filtration
    #.......#
    #generate nodes and edges for decision
    Net = generateNet(i, RulesDec, type, NodeColorType = NodeColorType, NewData, NewDataValues )
    #nodes = list(id =  nodesMatrix[,'id'], label =  nodesMatrix[,"label"])
    AllNets[[i]] = Net
    Net =  NULL
    #print(nodes)
  }

  AllNets[['all']] = generateNet('all', recRulesFiltr, type, NodeColorType = NodeColorType ,NewData, NewDataValues)
  AllNets[['Rules']] = rules
  return(AllNets)
}
