filter_rules = function(rules, minAcc, minSupp){


    if(max(rules$supportRHS) < minSupp){
      'Please change min Support value. Your value is too high!'
    }else if(max(rules$accuracyRHS) < minAcc){
      'Please change min Accucary value. Your value is too high!'
    }else{
      NULL
    }

}
