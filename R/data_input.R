data_input = function(data1, type){
  #R.Rosetta output
  if(type == 'RDF'){
    df = data1
  }else if(type == 'L'){
    df = data1
  }else if(type == 'RGUI'){
    rl = dataset_merged[-lapply(dataset_merged, function(x) grep('%', x))[[1]],]
    j = 0
    all_rules=NULL
    condition = list()
    features = list()
    decision = list()
    accuracy = list()
    support = list()
    id =list()
    for (line in rl){
      line = as.character(line)
      if (length(grep( '=>', line))==1){
        j = j + 1
        ele = strsplit(line,'=>', fixed = TRUE)[[1]]
        LHS = ele[1]
        tRHS = ele[2]
        features = gsub(' ','',strsplit(LHS, 'AND')[[1]])
        condition[[j]] = paste0(features,collapse=',')
        id[[j]] = j
        RHS = gsub(' ','',strsplit(tRHS, 'OR')[[1]])
        decision[[j]] = RHS
      }else {
        param  = strsplit(line,'=')[[1]]
        if(length(grep("Acc.  (RHS)", param[1], fixed = TRUE))==1){ accuracy[[j]] = as.numeric(strsplit(gsub('\\[|\\]','', param[2]),',')[[1]])}
        if(length(grep("Supp. (RHS)", param[1], fixed = TRUE))==1){ support[[j]] = as.numeric(strsplit(gsub('object(s)', '',gsub('\\[|\\]','', param[2]), fixed = TRUE),',')[[1]])}
      }
    }

    sup_max = lapply(support, which.max)
    s = unlist(sup_max)
    #lapply(accuracy, function(x) x[s])



    accuracy_max = mapply(function(X,Y) {
      sapply(1, function(row) X[[Y]])
    }, X=accuracy, Y=sup_max)

    decision_max = mapply(function(X,Y) {
      sapply(1, function(row) X[[Y]])
    }, X=decision, Y=sup_max)

    support_max = mapply(function(X,Y) {
      sapply(1, function(row) X[[Y]])
    }, X=support, Y=sup_max)

    rules2 = as.data.frame(cbind(unlist(condition), decision_max, as.character(accuracy_max), as.character(support_max)))
    colnames(rules2) = c('FEATURES', 'DECISION', 'ACC_RHS', 'SUPP_RHS')
    rules2$ACC_RHS = as.numeric(as.character(rules2$ACC_RHS))
    rules2$SUPP_RHS = as.numeric(as.character(rules2$SUPP_RHS))
    rules2$PVAL = 0.05
    df = rules2
  }else{
    print('Invalid data type!')
  }
  return(df)
}

