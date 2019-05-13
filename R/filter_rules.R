filter_rules = function(rules, minAcc, minSupp){


    if(max(rules$SUPP_RHS) < minSupp){
      'Please change min Support value. Your value is too high!'
    }else if(max(rules$ACC_RHS) < minAcc){
      'Please change min Accucary value. Your value is too high!'
    }else{
      NULL
    }

}
