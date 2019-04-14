#' Rule Network Visualistion gadget
#'
#' This is a mostly empty package to learn roxygen documentation.
#'
#' Hello allows me to learn how to write documentation in comment blocks
#' co-located with code.
"_PACKAGE"

#' visunet
#' @import visNetwork shiny shinythemes R.ROSETTA
#' @param rules data frame or path to the rules file, depending on the values of the type parameter.\cr
#' In the case of data frame, rule is the output of \code{\link[R.ROSETTA]{rosetta}}. The rules file
#' is the output of \code{\link[R.ROSETTA]{rosetta}} saved using \code{\link[R.ROSETTA]{saveLineByLine}}
#' or the output of the ROSETTA GUI version (see \url{http://bioinf.icm.uu.se/rosetta/}).
#'
#' @param type  character string specifying the type of the input data.  Three types implemented
#' are "RDF" (the R.ROSETTA data frame), "L" ("Line by line" file format) and "RGUI" (ROSETTA GUI plan format). \cr
#' The "Line by line" is the tab-separated file, each row corresponds to one rule. File contains 4 or 5 columns:
#' condition list, decision, accuracy and support of the rule, the optional 5th column contains a fractional support value\cr
#'The default is "RDF".
#'
#' @param Cust logical flag: if TRUE the CustObject is customised Rule Network Object, otherwise the
#' CustObject parameter is ignored.
#' @param CustObject customised Rule Network Object corresponding to the output of the visunet function,
#' that can be modified according
#' to the visualisation aim. See more in the details section.
#' @return Rule Network Object.
#' @keywords misc
#' @export
#' @examples
#' R.ROSETTA output
#' out = rosetta(autcon)
#' rules = out$main
#' vis_out = visunet(rules)
#'------------
#' Line by line file
#' rules = (read.csv2('dataset_ethnicity_all_100set.txt', sep='\t', header = FALSE, col.names = c('FEATURES', 'DECISION', 'ACC_RHS', 'SUPP_RHS'),stringsAsFactors=FALSE))
#' rules$ACC_RHS = as.numeric(rules$ACC_RHS)
#' rules$SUPP_RHS = as.numeric(rules$SUPP_RHS)
#' rules$PVAL = 0.05
#' vis_out = visunet(rules, 'L')
#'

visunet = function(rules, type ='RDF', NodeColorType = 'DL', Cust=FALSE, CustObject){

  rules = data_input(rules, type)
  rules_10per_param = filtration_rules_10per(rules)

  minAcc = rules_10per_param$minAcc
  minSupp = rules_10per_param$minSupp
  minPrecSupp = rules_10per_param$minPrecSupp
  #NodeColorType = 'DL'

  ui <- dashboardPage(
    header <- dashboardHeader(title = "VisuNet", tags$li(class = "dropdown", actionButton("done", "Done"))),
    sidebar <- dashboardSidebar(
      sidebarMenu(
        uiOutput("decisions"),
        hr(),
        sliderInput("accuracy", ("Min Accuracy"),
                    min = 0, max = 1, value = minAcc, step = 0.01),
        uiOutput("support"),
        sliderInput("PrecSupport", ("Min % Support"),
                    min = 0, max = 100, value = minPrecSupp, step = 1),
        numericInput("TopNodes", label = ("Show top n nodes"), value = 0),
        selectInput("NodeColor",label = ("Color of nodes"), choices =  c('Accuracy value' = 'A','Discretization Levels' = 'DL'), selected = NodeColorType),

      #  sliderInput("deg", "Degree :", min = 0, max = 10, value = 0),
       # checkboxInput("highlightNearestSelect", "Select node ?", FALSE),
        actionButton("run", "Run"),
        downloadButton('saveHTML', 'Save network as .html'),
        menuItem("Network", icon = icon("project-diagram"), tabName = "network"),
        menuItem("Legend", icon = icon("sliders"), tabName = "legend"),
        menuItem("About", icon = icon("user-astronaut"), tabName = "about")
      )
    ),
    body <- dashboardBody(
     # tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
      tabItems(
        tabItem(tabName = 'network',title = 'Network',
             #   fluidRow(
                  #box( title = 'Options', background = "aqua", collapsible = TRUE,
                  #           #status = 'success',
                  #           solidHeader = TRUE,

                  #           uiOutput("Select")

               # ),


             #   box( title = 'Options', collapsible = TRUE,
              #               status = 'success',
              #               solidHeader = TRUE

                             #downloadButton('saveHTML', 'Save network as .html')
             #                )),


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
                h2("legend")),
        tabItem(tabName = "about",
                h2("About"))
      )
    )

  )




  server <- function(input, output) {



    decs = unique(as.matrix(rules$DECISION))
    decs_f = c('all', decs )
    data <- eventReactive( input$run, {
      validate(
        filter_rules(rules, input$accuracy, input$support, input$PrecSupport)
      )
      RulesFiltr =  filtration_rules(rules, input$accuracy, input$support, input$PrecSupport)
      data_input=generate_object(decs, RulesFiltr,type, input$TopNodes, input$NodeColor, Cust, CustObject)
      data_input[['Rules']] = rules
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
        visInteraction(hover = TRUE) %>%
        visEdges(smooth = TRUE) %>%
        visEvents(select = "function(nodes) {
                  Shiny.onInputChange('current_node_id', nodes.nodes);
                  ;}")

      if( length(nodes$group) >0){
        visNetwork::visOptions(graph = graph, selectedBy = list(variable = "group" ,  style = 'width: 200px; height: 30px;
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
        visOptions(selectedBy = list(variable = "group", selected = input$Select))
      # visRemoveNodes(id = input$Focus)
      # visFit(nodes = input$Focus)
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
                  min = 0, max = max(rules$SUPP_RHS), value = minSupp, step = 1)
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
      if(nrow(nodes[which(nodeInfo$selected == nodes$id),])!=0){
        dataTableOutput('table')
      } else{}

    })

    output$saveHTML <- downloadHandler(
      filename = function() {
        paste('network-', Sys.Date(), '.html', sep='')
      },
      content = function(con) {
        net() %>% visSave(con)
      }
    )

    observeEvent(input$done, {
      stopApp(data())
    })
  }

  runGadget(ui, server, viewer = browserViewer())
}
