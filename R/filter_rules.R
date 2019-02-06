filter_rules = function(rules, minAcc, minSupp, PercSupp){
  if(length(rules$PERC_SUPP_RHS ) > 0){
    if(max(rules$PERC_SUPP_RHS) < PercSupp){
      'Please change min % Support value. Your value is too high!'
    }else if(max(rules$ACC_RHS) < minAcc){
      #print('YES')
      'Please change min Accucary value. Your value is too high!'
    }else{
      NULL
    }
  }else{
    if(max(rules$SUPP_RHS) < minSupp){
      'Please change min Support value. Your value is too high!'
    }else if(max(rules$ACC_RHS) < minAcc){
      'Please change min Accucary value. Your value is too high!'
    }else{
      NULL
    }
  }
}
