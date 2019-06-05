filtration_rules = function(rules, minAcc, minSupp){
    recRulesFiltr = rules[which(rules$accuracyRHS >= minAcc & rules$supportRHS >= minSupp),]

  return(recRulesFiltr)
}
