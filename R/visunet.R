#' Rule Network Visualistion gadget
#'
#' This is a mostly empty package to learn roxygen documentation.
#'
#' Hello allows me to learn how to write documentation in comment blocks
#' co-located with code.
"_PACKAGE"

#' visunet
#' @import visNetwork shiny shinythemes R.ROSETTA
#' @param rules data frame with rules information. Required at least four columns: "features",
#' "accuracyRHS", "supportRHS" and "decision" and each row corresponds to one rule.\cr
#' See the output of \code{\link[R.ROSETTA]{rosetta}} for the information about the rules data frame structure.
#'#'
#' @param type  character string specifying the type of the input data.  Three types implemented
#' are "RDF" - the R.ROSETTA output (see \code{\link[R.ROSETTA]{rosetta}}), "L" - "Line by line" file format (see \code{\link[R.ROSETTA]{saveLineByLine}})
#'The default is "RDF".
#'
#' @param NodeColorType character string specifying the color of nodes.
#' \itemize{
#'   \item "DL" - feature discretization levels
#'   \item "A" - feature distratization value
#' }
#'The default is "DL".
#'
#' @param CustObjectNodes a list that contains customised VisuNet output for nodes. List need
#' to contain two variables:
#'\itemize{
#'   \item nodes - customized VisuNet output for nodes
#'   \item CustCol - names of variables added/changed in the VisuNet output for nodes.
#'   See \code{\link[visNetwork]{visNodes}} for the full list of available variables.
#' }
#' Example use in the example section
#'
#' @param CustObjectEdges a list that contains customised VisuNet output for edges. List need
#' to contain two variables:
#'\itemize{
#'   \item edges - customized VisuNet output for nodes
#'   \item CustCol - names of variables added/changed in the VisuNet output for nodes.
#'   See \code{\link[visNetwork]{visEdges}} for the full list of available variables.
#' }
#' Example use in the example section
#'
#' @return Rule Network Object.
#' @keywords misc
#' @export
#' @examples
#'
#' #R.ROSETTA output
#' out = rosetta(autcon)
#' rules = out$main
#' vis_out = visunet(rules)
#'------------
#' #"Line by line" file format
#' rules = (read.csv2('LbL.txt', sep='\t', header = FALSE, col.names = c('features', 'decision', 'accuracyRHS', 'supportRHS'),stringsAsFactors=FALSE))
#' rules$accuracyRHS = as.numeric(rules$accuracyRHS)
#' rules$supportRHS = as.numeric(rules$supportRHS)
#' rules$pValue = 0.05
#' vis_out = visunet(rules, 'L')
#'
#'------------
#'#customisation of the VisuNet output for nodes
#'nodes_RNO <- vis_out$all$nodes
#'#Changing the nodes shape to stars
#'nodes_RNO$shape <- rep('star', length(nodes_RNO$label))
#'# a customized nodes list
#'nodesL <- list(nodes = nodes_RNO,CustCol =  c('shape'))
#'Rerun VisuNet with a customized nodes list
#'vis_out2 <- visunet(rules, CustObjectNodes = nodesL)
#'
#'------------
#'customisation of VisuNet output for edges
#'Adding arrows to edges
#'edges_RNO <- vis_out$all$edges
#'edges_RNO$arrows <- rep('to', length(edges_RNO$label2))
#'# a customized edges list
#'edgesL <- list(edges = edges_RNO,CustCol =   c('arrows'))
#'Rerun VisuNet with a customized edges list
#'vis_out3 <- visunet(rules, CustObjectEdges = edgesL)



visunet = function(rules, type ='RDF', NodeColorType = 'DL',  CustObjectNodes=list(), CustObjectEdges=list()){
  rules = data_input(rules, type)
  rules_10per_param = filtration_rules_10per(rules)
  minAcc = rules_10per_param$minAcc
  minSupp = rules_10per_param$minSupp


  ui <- dashboardPage(
    header <- dashboardHeader(title = "VisuNet", tags$li(class = "dropdown", actionButton("done", "Done"))),
    sidebar <- dashboardSidebar(
      sidebarMenu(
        uiOutput("decisions"),
        hr(),
        sliderInput("accuracy", ("Min Accuracy"),
                    min = 0, max = 1, value = minAcc, step = 0.01),
        uiOutput("support"),
        numericInput("TopNodes", label = ("Show top n nodes"), value = 0),
        selectInput("NodeColor",label = ("Color of nodes"), choices =  c('Accuracy value' = 'A','Discretization Levels' = 'DL'), selected = NodeColorType),

        actionButton("run", "Run"),
        downloadButton('saveHTML', 'Save network as .html'),
        menuItem("Network", icon = icon("project-diagram"), tabName = "network") ,
        menuItem("Legend", icon = icon("sliders"), tabName = "legend")#,
        #menuItem("About", icon = icon("user-astronaut"), tabName = "about")
      )
    ),
    body <- dashboardBody(
     # tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
      tabItems(
        tabItem(tabName = 'network',title = 'Network',


                fluidRow(
                  #adding network
                  box(width=12, height = 700,
                      status = "primary",
                     # tags$head(tags$style(HTML(".tab-pane { height: 70vh; overflow-y: auto; }" ))),
                      solidHeader = TRUE,
                      collapsible = FALSE,
                      visNetworkOutput("network", height = "600px"))

                ),
                #,
                fluidRow(
                  #adding rules table for nodes
                  box(status = "warning", title ='Rules',
                      width=12,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      dataTableOutput("nodes_data_from_shiny"),
                      uiOutput('dt_UI'))
                )),
        tabItem(tabName = "legend",
                fluidPage(
                  h2("Legend"),
                  tags$img(src = 'https://i.ibb.co/ZGznD9Y/legend-shony.png', height="500"))),
        tabItem(tabName = "about",
                h2("About"))
      )
    )

  )




  server <- function(input, output) {

    decs = unique(as.matrix(rules$decision))
    decs_f = c('all', decs )
    lnodes <- data.frame(label = c("Under-Expressed", "No change","Over-Expressed"),
                         shape = c( "dot", "dot", "dot"),
                         color = list(background=c("#999999") ,border=c("#0072B2")),
                         borderWidth = c(3),font.size = 18, size =c(25),
                         id = 1:3)


    data <- eventReactive( input$run, {
      validate(
        filter_rules(rules, input$accuracy, input$support)
      )
      RulesFiltr =  filtration_rules(rules, input$accuracy, input$support)
      data_input=generate_object(decs, RulesFiltr,type, input$TopNodes, input$NodeColor,  CustObjectNodes, CustObjectEdges)
      return(data_input)
    })

    net <- reactive({
      data = data()

      decisionName = input$decisions
      nodes = data[[decisionName]]$nodes
      edges = data[[decisionName]]$edges

      validate(
        need(is.null(nodes) == FALSE, "No rules for the current decision. Change the settings")
      )
      graph = visNetwork::visNetwork(nodes, edges, main = paste('Decision: ', decisionName), height = "800px",
                                     width = "100%") %>%
        visLayout(randomSeed = 123) %>%
        visPhysics(enabled = TRUE) %>%
        visInteraction(hover = TRUE, navigationButtons = TRUE) %>%
        visEdges(smooth = TRUE) %>%
        visNodes(font = list(size='1500px'))%>%
        visEvents(select = "function(nodes) {
                  Shiny.onInputChange('current_node_id', nodes.nodes);
                  ;}")
      if( length(nodes$group) >0){
        visNetwork::visOptions(graph = graph, selectedBy = list(variable = "group" , multiple = TRUE, main = "Select by decision", style = 'width: 200px; height: 30px;
                                                                padding-left: 80px;
                                                              font-size: 15px;
                                                                color: black;
                                                                border:none;
                                                                outline:none;'))

      }else{
        graph
      }

  })

    output$network <- renderVisNetwork({
      net()
  })


    observe({
      visNetworkProxy("network") %>%
        visOptions(selectedBy = list(variable = "group", selected = input$Select) )
    })

   # observe({

    #  visNetworkProxy("network") %>%
    #    visOptions(highlightNearest = list(enabled = TRUE,
     #                                   degree = input$deg), nodesIdSelection = input$highlightNearestSelect)
    #})

    output$decisions <- renderUI({
      selectInput("decisions",label = ("Choose decision"), choices =  as.character(decs_f), selected = decs_f[1])
    })


    #output$Select <- renderUI({
   #   selectInput("Select",label = ("Select by decision"), choices =  as.character(decs_f), selected = decs_f[1])
   # })

    output$support <- renderUI({
      sliderInput("support", ("Min Support"),
                  min = 0, max = max(rules$supportRHS), value = minSupp, step = 1)
    })

    output$NodeColor <- renderUI({
      if(input$ColorNode == 'DL'){
        colorNodeValue = 'Discretization Levels'
      }else{
        colorNodeValue = 'A'
      }
      selectInput("NodeColor",label = h4("Color of nodes"), choices =  c('Accuracy value','Discretization Levels'), selected = NodeColorType)
    })


    nodeInfo <- reactiveValues(selected = '')

    observeEvent(input$current_node_id, {
      nodeInfo$selected <<- input$current_node_id
    })

    output$table <- renderDataTable({
      data =  data()
      decisionName = input$decisions
      nodes = data[[decisionName]]$nodes
      data[[decisionName]]$NodeRulesSetPerNode[[nodeInfo$selected]]
    })

    output$dt_UI <- renderUI({
      data =  data()
      decisionName = input$decisions
      nodes = data[[decisionName]]$nodes
      validate(
        need(is.null(nodes) == FALSE, "")
      )
      if(nrow(nodes[which(nodeInfo$selected == nodes$id),])!=0){
        dataTableOutput('table')
      } else{}

    })

    output$saveHTML <- downloadHandler(
      filename = function() {
        paste('network-', Sys.Date(), '.html', sep='')
      },
      content = function(con) {
        net() %>%
          visSave(con)

      }
    )

    observeEvent(input$done, {
      stopApp(data())
    })
  }

  runGadget(ui, server, viewer = browserViewer())
}
