#' Rule Network Visualistion gadget
#' @import visNetwork shiny shinythemes R.ROSETTA
#' @param rule table
#' @return Rule Network Object.
#' @keywords misc
#' @export
#' @examples
#' R.ROSETTA output
#' out=rosetta(autcon)
#' rules=out$main
#' visunet(rules)
#'------------
#' Line by line file
#' rules2 = (read.csv2('dataset_ethnicity_all_100set.txt', sep='\t', header = FALSE, col.names = c('FEATURES', 'DECISION', 'ACC_RHS', 'SUPP_RHS'),stringsAsFactors=FALSE))
#' rules2$ACC_RHS = as.numeric(rules2$ACC_RHS)
#' rules2$SUPP_RHS = as.numeric(rules2$SUPP_RHS)
#' rules2$PVAL = 0.05
#' d2 = visunet(rules2, 'L')
#'

visunet = function(data1, type ='RDF',  NewData=FALSE, NewDataValues){



  rules = data_input(data1, type)
  rules_10per_param = filtration_rules_10per(rules)

  minAcc = rules_10per_param$minAcc
  minSupp = rules_10per_param$minSupp
  minPrecSupp = rules_10per_param$minPrecSupp
  NodeColorType = 'DL'

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
        actionButton("run", "Run"),
        menuItem("Network", icon = icon("project-diagram"), tabName = "network"),
        menuItem("Legend", icon = icon("sliders"), tabName = "legend"),
        menuItem("About", icon = icon("user-astronaut"), tabName = "about")
      )
    ),
    body <- dashboardBody(
     # tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
      tabItems(
        tabItem(tabName = 'network',title = 'Network',
                fluidRow(box(width=12, title = 'Options', collapsible = TRUE,
                             status = 'success',
                             solidHeader = TRUE,
                             downloadButton('saveHTML', 'Save network as .html')
                             )),
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
      #print(dim(RulesFiltr))
      data_input=generate_object(decs, RulesFiltr,type, input$TopNodes, input$NodeColor, NewData, NewDataValues)
      data_input[['Rules']] = rules
      return(data_input)
      #return(list(data_input = data_input, TopNodes = TopNodes))
    })

    #output$network <- renderVisNetwork({
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
        #visExport(name = "export-network",
        #          float = "right", label = "Save network",  style= "")  %>%
        visEvents(select = "function(nodes) {
                  Shiny.onInputChange('current_node_id', nodes.nodes);
                  ;}")

      if( length(nodes$group) >0){
        visNetwork::visOptions(graph = graph, selectedBy = list(variable = "group",  style = 'width: 200px; height: 30px;
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
    output$decisions <- renderUI({
      selectInput("decisions",label = ("Choose decision"), choices =  as.character(decs_f), selected = decs_f[1])
    })

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


    myNode <- reactiveValues(selected = '')

    observeEvent(input$current_node_id, {
      myNode$selected <<- input$current_node_id
    })

    output$table <- renderDataTable({
      data =  data()
      decisionName = input$decisions
      nodes = data[[decisionName]]$nodes
      data[[decisionName]]$NodeRulesSetPerNode[[myNode$selected]]
    })

    output$dt_UI <- renderUI({
      data =  data()
      decisionName = input$decisions
      nodes = data[[decisionName]]$nodes
      if(nrow(nodes[which(myNode$selected == nodes$id),])!=0){
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
