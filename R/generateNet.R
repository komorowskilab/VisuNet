generateNet=function(decs, rules, type, RulesSetSite, TopNodes, NodeColorType, NewData, NewDataValues){
  # rules = recRulesFiltr
  # print(rules)
  if(type == 'RDF'){
    vec = as.character(as.matrix(rules["FEATURES"]))
    lst1 = sapply(vec, function(x) strsplit(x, ","))
    vec2 = as.character(as.matrix(rules["DISC_CLASSES"]))
    lst2 = sapply(vec2, function(x) strsplit(x, ","))
    #print(lst1)
    #print(lst2)
    newLst = mapply(paste, collapse = ",", sep = "=", lst1,
                    lst2)
    NodeID = as.character(unname(newLst))
    rules$id=NodeID
  }else{
    rules$id = as.matrix(rules$FEATURES)
  }

  #nodeLabel = unique(cbind(unlist(strsplit(as.matrix(label),',')),unlist(strsplit(as.matrix(vec),','))))
  # Rule connection value
  rules$CONNECTION = rules$SUPP_RHS * rules$ACC_RHS
  #Node information
  Nodes_vec=sapply(rules$id, function(x) strsplit(x, ","))
  NodeUniq=unique(unlist(Nodes_vec))

  NodeInfoDF = NULL
  NodeState = NULL
  meanAcc = NULL
  meanSupp = NULL
  meanPrecSupp = NULL
  NRules = NULL
  PrecRules = NULL
  NodeConnection = NULL
  NodeRulesSet = NULL
  DecisionSet = NULL
  for (nod in NodeUniq){
    #nod = 'MAP7=3'
    #nod = "MXRA7_Activated_4=3"
    node_id = (which(lapply(Nodes_vec, function(x)  length(which(x == nod) )) !=0))
    # print(nod)
    #discrete state
    NodeState = c(NodeState,strsplit(nod, '=')[[1]][2])

    #mean accuracy
    meanAcc = c(meanAcc,mean(rules[node_id,"ACC_RHS"]))
    #mean support
    meanSupp = c(meanSupp, mean(rules[node_id,"SUPP_RHS"]))
    #mean % support
    if("PERC_SUPP_RHS" %in% colnames(rules[node_id,])) meanPrecSupp = c(meanPrecSupp, mean(rules[node_id,"PERC_SUPP_RHS"])) else meanPrecSupp = c(meanPrecSupp, NA)
    # number of rules
    NRules = c(NRules, dim(rules[node_id,])[1])
    # % from rules in decision
    PrecRules = c(PrecRules, dim(rules[node_id,])[1] / dim(rules)[1] )
    # Connection value
    NodeConnection = c(NodeConnection, sum(rules[node_id,]$CONNECTION * (unlist((lapply(Nodes_vec[node_id], length)))-1)))
    #Set of rules per Node
    NodeRulesSet[[nod]] = viewRules(rules[node_id,])
    DecisionSet = c(DecisionSet, names(sort(table(rules[node_id, "DECISION"]),decreasing=TRUE)[1]))

  }


  NodeColor = NULL

  if(NodeColorType == 'DL'){
    #color according to the discrete state - GENE EXPRESSION:
    NodeColor = rep('#999999', length(NodeUniq))
    NodeColor[which(NodeState == min(NodeState))] = '#56B4E9'
    NodeColor[which(NodeState == max(NodeState))] = '#E69F00'

    #Nodes label
    NodeLabel = unlist(lapply(NodeUniq, function(x) strsplit(x, '=')[[1]][1]))
  }else if(NodeColorType == 'A'){
    #color according to the discrete state:
    #colFunc = colorRampPalette(c("#F5DBC6", "#D55E00"), interpolate = "spline", bias = 4)
    #colorVec = colFunc(10)
    #plot(rep(1,10),col=colorVec,pch=19,cex=3)
    #NodeColor = colorVec[round(meanAcc,2)*100]

    # #color according to the accuracy value
    #matrix of colors
    breaks=seq(0.7, 1, by=0.001)
    colFunc60_100 = colorRampPalette(c("#EDBF9A", "#D55E00"))
    colFunc60_100 = colorRampPalette(c("#F1CDB0", "#D55E00"))
    colVec = colFunc60_100(length(breaks))
    breaks2 = seq(0,0.69,by=0.001)
    #colFunc0_60 = colorRampPalette(c("#F5DBC6", "#EDBF9A"))
    #colFunc0_60 = colorRampPalette(c("white", "#EDBF9A"))
    colFunc0_60 = colorRampPalette(c("white", "#F1CDB0"))
    colVec2 = colFunc0_60(length(breaks2))
    ColorMat = cbind(as.numeric(c(breaks2, breaks)),c(colVec2, colVec))

    NodeColor = ColorMat[match(round(meanAcc,2),ColorMat[,1]),2]

    #Nodes label
    NodeLabel = NodeUniq
  }else{
    print('The color schema value is wrong!')
  }



  if (is.na(meanPrecSupp)[1] == FALSE){
    #print('YES')
    NodeTitle = paste0('Name: <b>', NodeUniq, '</b><br/>Edges: <b>', NRules, '</b><br/>Connection: <b>',  round(NodeConnection,2),
                       '</b><br/>Mean accuracy: <b>', round(meanAcc,2), '</b><br/>Mean % support: <b>', round(meanPrecSupp,2))
    #Node Info data frame
    NodeInfoDF = data.frame(id = NodeUniq,  label =  NodeLabel, DiscState = NodeState, color.background = NodeColor, value = meanPrecSupp,
                            borderWidth = (PrecRules*20), color.border = c("#0072B2"),
                            meanAcc = meanAcc, meanSupp = meanSupp, meanPERC_SUPP = meanPrecSupp, NRules = NRules,
                            PrecRules = PrecRules, NodeConnection = NodeConnection, title = NodeTitle)
  }else{
    #print('NO')
    NodeTitle = paste0('Name: <b>', NodeUniq, '</b><br/>Edges: <b>', NRules, '</b><br/>Connection: <b>',  round(NodeConnection,2),
                       '</b><br/>Mean accuracy: <b>', round(meanAcc,2), '</b><br/>Mean support: <b>', round(meanSupp,2))
    #Node Info data frame
    NodeInfoDF = data.frame(id = NodeUniq,  label =  NodeLabel, DiscState = NodeState, color.background = NodeColor, value = meanSupp,
                            borderWidth = (PrecRules*20), color.border = c("#0072B2"),
                            meanAcc = meanAcc, meanSupp = meanSupp,  NRules = NRules,
                            PrecRules = PrecRules, NodeConnection = NodeConnection, title = NodeTitle)
  }

  if(decs == 'all'){
    NodeInfoDF$group = DecisionSet
  }

  #NodeInfoDF$font.size = 12

  # NodeTitle = paste0('Name: <b>', NodeUniq, '</b><br/>Edges: <b>', NRules, '</b><br/>Connection: <b>',  round(NodeConnection,2),
  #                   '</b><br/>Mean accuracy: <b>', round(meanAcc,2), '</b><br/>Mean % support: <b>', round(meanPrecSupp,2))
  #Node Info data frame
  # NodeInfoDF = data.frame(id = NodeUniq,  label =  NodeLabel, DiscState = NodeState, color.background = NodeColor, value = meanPrecSupp,
  #                         borderWidth = (PrecRules*100), color.border = c("#0072B2"),
  #                         meanAcc = meanAcc, meanSupp = meanSupp, meanPrecSupp = meanPrecSupp, NRules = NRules,
  #                         PrecRules = PrecRules, NodeConnection = NodeConnection, title = NodeTitle)


  NodeInfoDF = NodeInfoDF[order(NodeInfoDF$NodeConnection, decreasing = TRUE),]
  #Node Info data frame
  #NodeInfoDF = data.frame('NodeUniq' = NodeUniq,  'NodeLabel' = NodeUniq, 'DiscState' = NodeState, 'NodeColor' = NodeColor,
  #                        'meanAcc' = meanAcc, 'meanSupp' = meanSupp, 'meanPrecSupp' = meanPrecSupp, 'NRules' = NRules,
  #                        'PrecRules' = PrecRules, 'NodeConnection' = NodeConnection, 'title' = NodeTitle)

  #print(dim(NodeInfoDF)[1])


  if(TopNodes != 0 & TopNodes <= dim(NodeInfoDF)[1]){
    NodeInfoDF = NodeInfoDF[1:TopNodes,]

  }else if(TopNodes > dim(NodeInfoDF)[1]){
    NodeInfoDF =  NodeInfoDF
  }

  #edges
  AllRuleLen = (lapply(Nodes_vec, length))
  EdgesInfo = NULL
  if(length(which(AllRuleLen !=1)) != 0){

    #print(AllRuleLen)
    rules2elem = which(AllRuleLen == 2)
    EdgesInfo2Ele=cbind(do.call(rbind,Nodes_vec[rules2elem]), rules[rules2elem,c("CONNECTION")])

    rules3AndMoreElem = which(AllRuleLen > 2)
    if(is.null(dim(rules3AndMoreElem)) == FALSE){
      rules3AndMoreElemList = lapply(Nodes_vec[rules3AndMoreElem], function(x) matrix(x[combn(1:length(x), 2)],ncol = 2, byrow = TRUE))
      EdgesInfo3Ele = do.call(rbind,mapply('cbind',  rules3AndMoreElemList,
                                           (rules[rules3AndMoreElem,"CONNECTION"]), SIMPLIFY=FALSE))
      EdgesInfoAll=rbind(EdgesInfo2Ele, EdgesInfo3Ele)
    }else{
      EdgesInfoAll=EdgesInfo2Ele
    }

    EdgesInfoTemp = as.data.frame(EdgesInfoAll)
    colnames(EdgesInfoTemp) = c('from' , 'to' , 'conn')
    EdgesInfoAllSort=t(apply(subset(EdgesInfoTemp, select=c("from", "to")), 1, sort))
    colnames(EdgesInfoAllSort) = c('from' , 'to')

    EdgesInfoAllSort2=data.frame(EdgesInfoAllSort,'conn' = EdgesInfoTemp$conn )
    EdgesInfo = aggregate(EdgesInfoAllSort2$conn~EdgesInfoAllSort2$from+EdgesInfoAllSort2$to, FUN= function(x) sum(as.numeric(levels(x))[x]))
    colnames(EdgesInfo) = c('from' , 'to' , 'conn')
    #Normalized connection value
    if(dim(EdgesInfo)[1] == 1 )  EdgesInfo$connNorm = 1 else EdgesInfo$connNorm = ((EdgesInfo$conn-min(EdgesInfo$conn))/(max(EdgesInfo$conn)-min(EdgesInfo$conn)))
    EdgesInfo$label2 = paste0(EdgesInfo$from, '-', EdgesInfo$to )
    EdgesInfo$color = rep('#e5e5e2', length(EdgesInfo$connNorm))
    EdgesInfo$color[which(EdgesInfo$connNorm >= 0.85)] = '#ea1d1d'
    EdgesInfo$color[which(EdgesInfo$connNorm < 0.85 & EdgesInfo$connNorm >= 0.7)] = '#d86431'
    EdgesInfo$color[which(EdgesInfo$connNorm < 0.7 & EdgesInfo$connNorm >= 0.55)] = '#dbcb33'
    EdgesTile = paste0('From:  <b>', EdgesInfo$from, '</b><br/>To: <b>', EdgesInfo$to,
                       '</b><br/>Connection: <b>', round(EdgesInfo$conn,2), '</b>')
    EdgesInfo$title = EdgesTile
    EdgesInfo$width  = (EdgesInfo$connNorm *5)

  }

  if(NewData == TRUE){
    if(NewDataValues$type == 'nodes'){

      new_columns =   (NewDataValues$df_values[(match(NodeInfoDF$id, NewDataValues$df_values$id )), 2:dim(NewDataValues$df_values)[2]])
      NodeInfoDF = cbind(NodeInfoDF[ , !(names(  NodeInfoDF) %in% colnames(new_columns))], new_columns)
      #View(head(cbind(NodeInfoDF, new_columns)))
      # NodeInfoDF[colnames(new_columns)]
    }else if(NewDataValues$type == 'edges'){
      new_columns =   (NewDataValues$df_values[(match(EdgesInfo$label, NewDataValues$df_values$label )), 2:dim(NewDataValues$df_values)[2]])
      EdgesInfo = cbind(EdgesInfo[ , !(names(  EdgesInfo) %in% colnames(new_columns))], new_columns)
    }else{
      print('Wrong data type!')
    }

  }


  Net = list(nodes = NodeInfoDF, edges = EdgesInfo, NodeRulesSetPerNode = NodeRulesSet)
  return(Net)
}
