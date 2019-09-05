filter_rules = function(rules, minAcc, minSupp, Param, minValue){
  if(is.null(Param)){
    Param <- 'Min Support'
    minValue <- 0
  }
     if(Param == 'Min Support'){
      if(max(rules$supportRHS) < minValue){
        'Please change min Support. Your value is too high!'
      }
    }else if(Param == 'Min Decision Coverage'){
      if(max(rules$decisionCoverage) < minValue){
        'Please change min Decision Coverage. Your value is too high!'
      }
    }else if(max(rules$accuracyRHS) < minAcc){
      'Please change min Accucary. Your value is too high!'
    }else{
      NULL
    }

}
