filterRulesTest = function(RNobj, minAcc, minSupp, PercSupp){
  #print('validation')
  #print(minAcc)
  #max(rules$ACC_RHS)
  # print(rules)
  if(length(RNobj$meanPERC_SUPP ) > 0){
   # print(RNobj$meanPERC_SUPP)
    if(max(RNobj$meanPERC_SUPP) < PercSupp){
      'Please change min % Support value. Your value is too high!'
    }else if(max(RNobj$meanAcc) < minAcc){
      #print('YES')
     # print((RNobj$meanAcc))
      'Please change min Accucary value. Your value is too high!'
    }else{
      NULL
    }
  }else{
    if(max(RNobj$meanSupp) < minSupp){
      'Please change min Support value. Your value is too high!'
    }else if(max(RNobj$meanAcc) < minAcc){
      'Please change min Accucary value. Your value is too high!'
    }else{
      NULL
    }
  }
}
