#' VisuNet: an interactive tool for network visualization of rule-based models in R
#'

#' VisuNet is an interactive tool for network visualization of complex rule-based classifiers. See the \href{https://komorowskilab.github.io/VisuNet/}{documentation}.
#' @import visNetwork shiny shinythemes R.ROSETTA
#' @param ruleSet the appropriately formatted set of rules:
#'\itemize{
#' \item R.ROSETTA data frame - the rules data frame that is the output of R.ROSETTA can be directly imported in VisuNet.
#' See \code{\link[R.ROSETTA]{rosetta}} for details.
#' \item "Line by line" file format - input data should be in a data frame format that contains the following columns:
#'\itemize{
#'\item features - the left-hand side of the rule corresponding to comma-separated attributes and their values, of type, type "factor"
#'\item decision - the right-hand side of the rule corresponding to the decision value, of type "factor"
#'\item accuracyRHS - the rule accuracy, of type "numeric"
#'\item supportRHS - the rule support, of type "numeric"
#'\item decisionCoverage or coverageRHS - the fraction of correcly classfied objects, of type "numeric"
#'}
#'}
#'
#' @param type  a character string specifying the type of the input data:
#' \itemize{
#' \item "RDF" - the R.ROSETTA output (see \code{\link[R.ROSETTA]{rosetta}})
#' \item "L" - the "Line by line" file format (see \code{\link[R.ROSETTA]{saveLineByLine}})
#'}
#'
#' @param NodeColorType a character string specifying the color of nodes:
#' \itemize{
#'   \item "DL" - feature discretization levels, option is available for data discretized into three levels: 1, 2 and 3.
#'   In the case of gene expression, data discretization levels correspond to: 1 - under-expressed gene, 2 - no change gene expression and 3 - over-expressed gene.
#'   \item "A" - color of nodes defined by the mean accuracy value for the node.
#' }
#'The default is "DL".
#'
#' @param NodeSize a character string specifying the size of nodes:
#' \itemize{
#'   \item "DC" - the mean decision coverage for the feature
#'   \item "S" - the mean support for the feature
#' }
#' If the decision coverage value is unavailable, the support is taken by default.
#'
#' @param CustObjectNodes a list that contains the customized VisuNet output for nodes. The list needs to contain two variables:
#'\itemize{
#'   \item nodes - a customized VisuNet output for nodes
#'   \item CustCol - the names of variables added/changed in the VisuNet output for nodes.
#'   See \code{\link[visNetwork]{visNodes}} for details.
#' }
#'
#' @param CustObjectEdges  a list that contains customized VisuNet output for edges.
#' The list needs to contain two variables:
#'\itemize{
#'   \item edges - a customized VisuNet output for edges
#'   \item CustCol - the names of variables added/changed in the VisuNet output for edges.
#'   See \code{\link[visNetwork]{visEdges}} for details.
#' }
#'
#'@references
#' See the \href{https://komorowskilab.github.io/VisuNet/}{documentation} for more details and examples.
#'
#' @return Rule Network Object - a collection of lists corresponding to decision variables and an additional list for the combined decision ‘all’.
#' The lists contain information required to reproduce the rule network, i.e. data frames for nodes, edges
#' and RulesSetPerNode - a list that shows rules for each node.
#' \cr
#' \cr
#' Structure of the data frame for nodes:
#' \itemize{
#' \item id - a unique node id, based on attribute value and left-hand side value of the rule set
#' \item label - the attribute variable without the ‘=value’ part from the left-hand side of the rule set
#' \item DiscState - the attribute value
#' \item color.background - the node color
#' \item value - the node size
#' \item color.border - the color of the node border
#' \item meanAcc - the mean accuracy value of all rules that contain the node
#' \item meanSupp - the mean support value of all rules that contain the node
#' \item NRules - the number of rules that contain the node
#' \item PrecRules - fraction of rules that contain the node
#' \item NodeConnection - the total connection value obtained from the rules that contain the node
#' \item title - information visible on the tooltip
#' \item group - the decision value that occurs most frequently (>50%) in rules associated with the node;
#' otherwise group contains all comma-separated decision values corresponding to rules associated with the node.
#' group defines the content of the ‘Select by decision’ drop-down box.
#' }
#'
#' \cr
#' Structure of the data frame for edges:
#' \itemize{
#' \item from, to - the pair of nodes that create the edge
#' \item conn - the connection variable obtained from the edge-associated rules.
#' \item connNorm - the connection variable normalized according to the maximum connection variable in the rule network
#' \item label2 - the edge id
#' \item color - the edge color
#' \item title - information visible on the tooltip
#' \item width - the edge width, defined according to the normalized connection value

#'
#' }
#' @keywords misc
#' @export
#' @examples
#'
#' #The R.ROSETTA output format
#' #the rule-based model construction using R.ROSETTA
#' resultsRos <- rosetta(autcon)
#' vis_out <- visunet(resultsRos$main, type = "RDF")
#'------------
#'
#' #"Line by line" file format
#' rules <- data(autcon_ruleset)
#' vis_out <- visunet(rules, type = "L")
#'




visunet = function(ruleSet, type ="RDF",  NodeColorType = "DL", NodeSize = "DC",  CustObjectNodes=list(), CustObjectEdges=list()){
  rules <- ruleSet
  rules <-  data_input(rules, type)
  rules_10per_param <-  filtration_rules_10per(rules)
  minAcc <-  rules_10per_param$minAcc
  minSupp <-  rules_10per_param$minSupp
  minDecisionCoverage <- rules_10per_param$minDecisionCoverage

  if(minDecisionCoverage == 0){
    NodeSize = 'S'
    choices_v <- 'Min Support'
    names(choices_v) <- 'S'
    choices_values <- minSupp
    names(choices_values) <- 'S'
  }else{
      choices_v <- c('Min Decision Coverage', 'Min Support')
      names(choices_v) <- c('DC', 'S' )
      choices_values <- c(minDecisionCoverage, minSupp )
      names(choices_values) <- c('DC', 'S')
      }


  ui <- dashboardPage(
    header <- dashboardHeader(title = "VisuNet", tags$li(class = "dropdown", actionButton("done", "Done"))),

    sidebar <- dashboardSidebar(
      sidebarMenu(
        tags$style(".skin-blue .sidebar a { color: #444; }"),
        uiOutput("decisions"),
        hr(),
        sliderInput("accuracy", ("Min Accuracy"),
                    min = 0, max = 1, value = minAcc, step = 0.01),

        uiOutput("FiltrParam"),
        uiOutput("value_slider"),

        numericInput("TopNodes", label = ("Show top n nodes"), value = 0),
        selectInput("NodeColor",label = ("Color of nodes"), choices =  c('Accuracy value' = 'A','Discretization Levels' = 'DL'), selected = NodeColorType),

        actionButton("run", "Run"),
       # downloadButton('saveHTML', 'Save network as .html'),
        uiOutput("download", class =  "butt1"),
        menuItem("Network", icon = icon("project-diagram"), tabName = "network") ,
        menuItem("Legend", icon = icon("sliders"), tabName = "legend")
      )
    ),
    body <- dashboardBody(
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
                      #dataTableOutput("nodes_data_from_shiny"),
                      uiOutput('dt_UI'))
                )),
        tabItem(tabName = "legend",
                fluidPage(
                  h2("Legend"),
                  tags$img(src = 'https://i.ibb.co/rGyG16p/Visu-Net-legend.png', height="500"))),
        tabItem(tabName = "about",
                h2("About"))
      )
    )

  )




  server <- function(input, output) {

    decs = unique(as.matrix(rules$decision))
    decs_f = c('all', decs )

    data <- eventReactive( input$run, {
      validate(
        filter_rules(rules, input$accuracy, input$support, input$FiltrParam, input$value_slider)
      )
      RulesFiltr =  filtration_rules(rules, input$accuracy, input$FiltrParam, input$value_slider)
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



    output$decisions <- renderUI({
      selectInput("decisions",label = ("Choose decision"),



                  choices =  as.character(decs_f), selected = decs_f[1])
    })


    output$FiltrParam = renderUI({


      selectInput(
        inputId = "FiltrParam",
        label = "",
        choices = as.character(choices_v),
        selected = NodeSize)
      })

    data_available <- eventReactive( input$FiltrParam, {
    data_available <- choices_v[choices_v == input$FiltrParam]
    })
    output$value_slider = renderUI({
     # data_available <- choices_v[choices_v == input$FiltrParam]
      data_available = data_available()
      value_available <- choices_values[names(choices_values) == names(data_available)]
      if(names(data_available) == 'S'){
        value_available_max <- max(rules$supportRHS)
        step = 1
      }else{
        value_available_max <- max(rules$decisionCoverage)
        step = 0.01
        }
      sliderInput(inputId = "value_slider",
                  label = '',
                  min = 0,
                  max = value_available_max,
                  value = value_available,
                  step = step)
    })

    output$support <- renderUI({
      sliderInput("support", ("Min Support"),
                  min = 0, max = max(rules$supportRHS), value = minSupp, step = 0.01)
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
      nodeInfo$selected <- input$current_node_id
    })

    output$table <- renderDataTable({
      data =  data()
      decisionName = input$decisions
      nodes = data[[decisionName]]$nodes
      data[[decisionName]]$RulesSetPerNode[[nodeInfo$selected]]
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

    output$download <- renderUI({
      if(input$run !=0) {
        downloadButton('saveHTML', 'Download network as .html')
      }
    })


    observeEvent(input$done, {
      stopApp(data())
    })
  }

  runGadget(ui, server, viewer = browserViewer())
}
