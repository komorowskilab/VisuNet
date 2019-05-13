filterRules = function(RNobj, minAcc, minSupp, PercSupp){
  if(length(RNobj$meanPERC_SUPP ) >0){
    RNobjFiltr = RNobj[which(RNobj$meanAcc >= minAcc & RNobj$meanPERC_SUPP >= PercSupp),]
  }else{
    RNobjFiltr = RNobj[which(RNobj$meanAcc >= minAcc & RNobj$meanSupp >= minSupp),]
  }
  #print(head(RNobjFiltr))
  return(RNobjFiltr)
}
