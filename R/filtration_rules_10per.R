filtration_rules_10per = function(rules){
  rules$CONNECTION = rules$SUPP_RHS * rules$ACC_RHS
  rules_order = rules[order(rules$CONNECTION, decreasing = TRUE),]
  decisions = unique(rules_order$DECISION)
  n_10per = (round(dim(rules_order)[1]*0.01, digits = 0))
  rules_order_10perc = rules_order[1:n_10per,]
  diff_decisions = (setdiff(decisions, unique(rules_order_10perc$DECISION) ))
  if(length(diff_decisions) != 0){
    ind_min_dec = NULL
    for(i in diff_decisions){
      ind_min_dec = c(ind_min_dec,min(which(rules_order$DECISION == i)))
    }
    ind_all_decs = max(ind_min_dec)
    rules_order_10perc = rules_order[1:ind_all_decs,]
  }

  minAcc = min(rules_order_10perc$ACC_RHS)
  minSupp = min(rules_order_10perc$SUPP_RHS)
  if("PERC_SUPP_RHS" %in% colnames(rules_order_10perc)) minPrecSupp = min(rules_order_10perc$PERC_SUPP_RHS) else minPrecSupp = 0

  out_rules_order_10perc = list(rules = rules_order_10perc, minAcc = minAcc, minSupp = minSupp, minPrecSupp = minPrecSupp )

  return(out_rules_order_10perc )
}
