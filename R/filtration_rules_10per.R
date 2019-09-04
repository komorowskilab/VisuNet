filtration_rules_10per = function(rules){
  fraction=0.1
  rules$CONNECTION = rules$supportRHS * rules$accuracyRHS
  rules_order = rules[order(rules$CONNECTION, decreasing = TRUE),]
  decisions = unique(rules_order$decision)
  n_10per = (round(dim(rules_order)[1]*fraction, digits = 0))
  rules_order_10perc = rules_order[1:n_10per,]
  diff_decisions = (setdiff(decisions, unique(rules_order_10perc$decision) ))
  if(length(diff_decisions) != 0){
    ind_min_dec = NULL
    for(i in diff_decisions){
      ind_min_dec = c(ind_min_dec,min(which(rules_order$decision == i)))
    }
    ind_all_decs = max(ind_min_dec)
    rules_order_10perc = rules_order[1:ind_all_decs,]
  }

  minAcc = min(rules_order_10perc$accuracyRHS)
  minSupp = min(rules_order_10perc$supportRHS)
  if("DecisionCoverage" %in% colnames(rules_order_10perc)) minDecisionCoverage = min(rules_order_10perc$minDecisionCoverage) else minDecisionCoverage = 0

  out_rules_order_10perc = list(rules = rules_order_10perc, minAcc = minAcc, minSupp = minSupp, minDecisionCoverage = minDecisionCoverage)

  return(out_rules_order_10perc )
}
