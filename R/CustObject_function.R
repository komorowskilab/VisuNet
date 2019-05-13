CustObject_function = function(DataInfoDF, NewCustDF, CustCol, id_col){
  #NewCustDF = NewDataNodes$nodes
  #CustCol = NewDataNodes$CustCol
  #DataInfoDF = NodeInfoDF
  print(head(DataInfoDF))
  print(NewCustDF)
  ind_Col = which(CustCol %in% colnames(DataInfoDF))
  print(colnames(DataInfoDF))
  NewCustDF[id_col,] = as.character(unlist(NewCustDF[id_col,]))
  DataInfoDF[id_col,] = as.character(unlist(DataInfoDF[id_col,]))
  int_id = intersect((NewCustDF[id_col,]), (DataInfoDF[id_col,]))
  NewCustDF_int =  NewCustDF[(which(as.character(NewCustDF[id_col,]) %in% int_id)),]
  if(length(ind_Col)>0){
    for(i in 1:length(ind_Col)){
      DataInfoDF[CustCol[ind_Col[i]]] = as.character(unlist(DataInfoDF[CustCol[ind_Col[i]]]))
      NewCustDF[CustCol[ind_Col[i]]] = as.character(unlist(NewCustDF[CustCol[ind_Col[i]]]))
      DataInfoDF[match(  NewCustDF_int[id_col,], as.character(DataInfoDF[id_col,])),CustCol[ind_Col[i]]] = NewCustDF_int[CustCol[ind_Col[i]]]
    }

    ind_Col_diff = CustCol[setdiff(seq(1, length(CustCol)),ind_Col)]
  }else{ind_Col_diff = CustCol}
  print(ind_Col_diff)
  for(i in 1:length(ind_Col_diff)){
    print(length(DataInfoDF[id_col,]))
    DataInfoDF$newcolumn = rep(NA, length(DataInfoDF[id_col,]))
    colnames(DataInfoDF)[which(colnames(DataInfoDF) == 'newcolumn')] = ind_Col_diff[i]
    NewCustDF[ind_Col_diff[i]] = as.character(unlist(NewCustDF[ind_Col_diff[i]]))

    DataInfoDF[match(  NewCustDF_int[id_col,], as.character(DataInfoDF[id_col,])),ind_Col_diff[i]] = NewCustDF_int[ind_Col_diff[i]]

  }


  return(DataInfoDF)
}
