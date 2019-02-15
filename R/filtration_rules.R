filtration_rules = function(rules, minAcc, minSupp, PercSupp){

  if(length(rules$PERC_SUPP_RHS ) >0){
    recRulesFiltr = rules[which(rules$ACC_RHS >= minAcc & rules$PERC_SUPP_RHS >= PercSupp),]
  }else{
    recRulesFiltr = rules[which(rules$ACC_RHS >= minAcc & rules$SUPP_RHS >= minSupp),]
  }

  return(recRulesFiltr)
}
