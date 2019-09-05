data_input = function(data1, type){
  #R.Rosetta output
  text_out = 'Incorrect data structure. Please check help.'
  if(type == 'RDF' | type == 'L'){
    validate(
      need("supportRHS" %in% colnames(data1), text_out),
      need("accuracyRHS" %in% colnames(data1), text_out),
      need("decision" %in% colnames(data1), text_out),
      need("features" %in% colnames(data1), text_out),
      need(is.numeric(data1$accuracyRHS), 'Accuracy is not numeric'),
      need(is.numeric(data1$supportRHS), 'Support is not numeric')
    )
    if("pValue" %in% colnames(data1) == FALSE){ data1$pValue = 0.05}
    if("decisionCoverage" %in% colnames(data1) | "coverageRHS" %in% colnames(data1)){
      data1 <-
        data1 %>%
        plyr::rename(., replace      = c("coverageRHS" = "decisionCoverage" ), warn_missing = FALSE)
      #data1 <- data1 %>% rename(decisionCoverage = coverageRHS)
      validate(
        need(is.numeric(data1$decisionCoverage), 'Decision coverage is not numeric')
      )
    }

    if(type == 'RDF'){validate(RDF_columns_test(colnames(data1),text_out))}
    if(!isTRUE("levels" %in% colnames(data1))){
      data1$levels = data1$CUTS_COND}
      df = data1
 # }else if(type == 'L'){
  #  validate(
  #    need("supportRHS" %in% colnames(data1), text_out),
 #     need("accuracyRHS" %in% colnames(data1), text_out),
  #    need("decision" %in% colnames(data1), text_out),
 #     need("features" %in% colnames(data1), text_out),
 #     need(is.numeric(data1$accuracyRHS), 'Accuracy is not numeric'),
 #     need(is.numeric(data1$supportRHS), 'Support is not numeric')
#    )
#    if("pValue" %in% colnames(data1) == FALSE){ data1$pValue = 0.05}

#    df = data1
  }else if(type == 'RGUI'){
    dataset_merged = data1
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
    colnames(rules2) = c('features', 'decision', 'accuracyRHS', 'supportRHS')
    rules2$accuracyRHS = as.numeric(as.character(rules2$accuracyRHS))
    rules2$supportRHS = as.numeric(as.character(rules2$supportRHS))
    rules2$pValue = 0.05
    df = rules2
  }else{
    print('Invalid data type!')
  }
  return(df)
}

