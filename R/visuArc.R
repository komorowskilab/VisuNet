#### arch plot
visuArc <- function(net, df, icol, dec, mainTitle){
  gene <- rownames(df[which.max(df[,icol]),])
  discLev <- substr(gsub("[\\(\\)]", "", regmatches(gene, gregexpr("\\(.*?\\)", gene))[[1]]), 1, 1)
  gene2 <- gsub("\\s*\\([^\\)]+\\)","",as.character(gene))
  edges <- net[[dec]]$edges[,1:4]
  edges_from <- as.character(edges$from)
  edges_to <- as.character(edges$to) ## gsub("\\=.*","",as.character(net$short$edges$to))
  indx <- unique(c(which(edges_from == paste0(gene2,"=",discLev)),which(edges_to == paste0(gene2,"=",discLev))))
  edges2 <- edges[indx,]
  M <- as.matrix(edges2[,1:2])

  #colors
  valsCols <- round(edges2$connNorm, digits = 1)*10
  edgesCon <- as.numeric(as.factor(as.character(valsCols)))
  colors <- colorRampPalette(c("gainsboro","lavender","darkorchid3"))(length(unique(valsCols)))[edgesCon]
  labelsNodes <- unique(c(t(M)))
  colNodes <- c("#56B4E9","#999999","#E69F00")[as.numeric(sub(".*=","",labelsNodes))]

  #nodes values
  nodesDlev <- substr(gsub("[\\(\\)]", "", regmatches(rownames(df), gregexpr("\\(.*?\\)", rownames(df)))),1,1)
  nodesNams <- gsub("\\s*\\([^\\)]+\\)","",as.character(rownames(df)))
  nodes2 <- paste0(nodesNams,"=",nodesDlev)
  nodeSize <- round(df[match(labelsNodes, nodes2),icol],digits = 1)*10

  ordV <- c(1,order(edgesCon, decreasing = T)+1)

  colsLabs <- rep("black",length(labelsNodes))
  colsLabs[which(labelsNodes == paste0(gene2,"=",discLev))] <- "red"
  arcplot(M, lwd.arcs=edgesCon, col.arcs = colors, col.nodes = colNodes, labels=sub("=.*","",labelsNodes),
          ordering=ordV, col.labels=colsLabs, cex.labels=1, font=1, lwd.nodes = nodeSize, main=mainTitle)
}
