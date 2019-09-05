filtration_rules <- function(rules, minAcc, Param, minValue){
    recRulesFiltr <- rules[which(rules$accuracyRHS >= minAcc),]
    if(Param == 'Min Support'){
      recRulesFiltr_f <-recRulesFiltr[which(recRulesFiltr$supportRHS >= minValue),]
    }else{
      recRulesFiltr_f <-recRulesFiltr[which(recRulesFiltr$decisionCoverage >= minValue),]
      }


  return(recRulesFiltr_f)
}
