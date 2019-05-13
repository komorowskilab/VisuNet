filtration_rules = function(rules, minAcc, minSupp){
    recRulesFiltr = rules[which(rules$ACC_RHS >= minAcc & rules$SUPP_RHS >= minSupp),]

  return(recRulesFiltr)
}
