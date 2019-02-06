#' Rule Network Visualistion gadget
#' @import visNetwork shiny shinythemes miniUI R.ROSETTA
#' @param rule table
#' @return Rule Network Object.
#' @keywords misc
#' @export
#' @examples
#'
#' R.ROSETTA output
#' out=rosetta(autcon)
#' rules=out$main
#' visunet(rules)
#'------------
#' 'Line by line' format
#' rules = (read.csv2('dataset_ethnicity_all_100set.txt', sep='\t', header = FALSE, col.names = c('FEATURES', 'DECISION', 'ACC_RHS', 'SUPP_RHS'),stringsAsFactors=FALSE))
#' rules$ACC_RHS = as.numeric(rules$ACC_RHS)
#' rules$SUPP_RHS = as.numeric(rules$SUPP_RHS)
#' rules$PVAL = 0.05
#' d2 = visunet(rules, 'L')
#'
visunet = function(data1, type ='RDF',  NewData=FALSE, NewDataValues){
  minAcc = 0.7
  minSupp = 1
  minPrecSupp = 10
  NodeColorType = 'GE'

  data_input = function(data1, type){
    #R.Rosetta output
    if(type == 'RDF'){
      df = data1

    }else if(type == 'L'){
      df = data1
    }else if(type == 'RF'){
      #Rosetta output format

    }else{
      print('Invalid data type!')
    }
    return(df)
  }

  ui <- miniPage(
    theme = shinytheme("cerulean"),
    gadgetTitleBar(h1("VisuNet")),
    miniTabstripPanel(
      miniTabPanel("Visualize", icon = icon('project-diagram'),

                   fillPage(fillRow(padding =15, flex = c(1, 3),
                                    fillCol(flex = c(NA),
                                            uiOutput("decisions"),
                                            sliderInput("accuracy", h4("Min Accuracy"),
                                                        min = 0, max = 1, value = minAcc, step = 0.01),
                                            uiOutput("support"),
                                            #sliderInput("support", h3("Min Support"),
                                            #             min = 0, max = 100, value = minSupp, step = 1),

                                            # uiOutput("PrecSupport"),

                                            sliderInput("PrecSupport", h3("Min % Support"),
                                                        min = 0, max = 100, value = minPrecSupp, step = 1),
                                            selectInput("NodeColor",label = h4("Color of nodes"), choices =  c('Accuracy value' = 'A','Gene Expression' = 'GE'), selected = NodeColorType),
                                            actionButton("run", "Run")#,
                                            #downloadButton("downloadData", "Download Plot")

                                    ),
                                    #visNetworkOutput("network", height = "90%"),

                                    fillCol(
                                      flex = c(5,NA),
                                      visNetworkOutput("network", height = "90%"),
                                      verbatimTextOutput("shiny_return")
                                      #  dataTableOutput("nodes_data_from_shiny")

                                    )


                                    #plotOutput("plotTopRight", height = "100%"),
                   )
                   )
      ),
      miniTabPanel("Legend", icon = icon("sliders"),
                   miniContentPanel(
                     h4("Legend")
                     #sliderInput("year", "Year", 1978, 2010, c(2000, 2010), sep = "")
                   )
      ),
      miniTabPanel("About", icon = icon("user-astronaut"),
                   miniContentPanel(
                     h4("About")
                   )
      )
    )
  )



  server <- function(input, output, session) {

    rules = data_input(data1, type)
    decs = unique(as.matrix(rules$DECISION))
    decs_f = c(decs, 'all')
    data_input=generate_object(decs_f, rules,type, NodeColorType, NewData, NewDataValues)


    #data <- eventReactive( c(input$decisions, input$accuracy, input$support, input$NodeColor) , {
    # Create a Progress object
    #   progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    #    on.exit(progress$close())
    #    progress$set(message = "Making plot", value = 0)
    #     progress$inc( detail = ("Doing part"))
    #    data_input=generate_object(decs, rules, input$accuracy, input$support, input$PrecSupport, input$NodeColor, NewData, NewDataValues)
    #  })

    nodesFiltr <- eventReactive( input$run, {
      validate(
        filterRulesTest(data_input$all$nodes, input$accuracy, input$support, input$PrecSupport)
      )
      decisionName = input$decisions
      nodes = data_input[[decisionName]]$nodes
      #print(nodes)
      edges = data_input[[decisionName]]$edges
      nodesFiltr = filterRules(nodes, input$accuracy, input$support, input$PrecSupport)

      nodesFiltrColor = NodeColor(nodesFiltr, input$NodeColor)
      output = list(nodes = nodesFiltrColor, edges = data_input[[decisionName]]$edges, decisionName = decisionName)
      #print(dim(nodes))
      #print(dim(nodesFiltr))
      return(output)
    })

    output$network <- renderVisNetwork({

      net =  nodesFiltr()
      print(net$nodes)
      nodes = net$nodes
      edges = net$edges
      decisionName = net$decisionName
      validate(
        need(dim(nodes)[1] != 0, "No rules for the current decision. Change the settings")
      )
      print(head(nodes$color.backgroundAcc))
      graph = visNetwork::visNetwork(nodes, edges, main = paste('Decision: ', decisionName), height = "800px", width = "100%") %>%
        visLayout(randomSeed = 123) %>%
        visPhysics(enabled = TRUE) %>%
        visInteraction(hover = TRUE) %>%
        visExport(type = "pdf" , name = "export-network",
                  float = "right", label = "Save network",  style= "")  %>%
        visEvents(hoverNode = "function(nodes) {
                  Shiny.onInputChange('current_node_id', nodes);
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


    output$decisions <- renderUI({
      selectInput("decisions",label = h4("Choose decision"), choices =  as.character(decs_f), selected = decs_f[1])
    })

    output$support <- renderUI({
      #selectInput("support",label = h4("Min Support"), choices =  as.character(decs_f), selected = decs_f[1])
      sliderInput("support", h4("Min Support"),
                  min = 0, max = max(rules$SUPP_RHS), value = minSupp, step = 1)
    })

    # output$PrecSupport <- renderUI({
    #     data =  data()
    #  conditionalPanel(
    #      condition = "length(data$meanPERC_SUPP) > 0",
    #     sliderInput("PrecSupport", h3("Min % Support"),
    #               min = 0, max = 100, value = minPrecSupp, step = 1)

    #    )
    #  })
    #
    output$NodeColor <- renderUI({
      if(input$ColorNode == 'GE'){
        colorNodeValue = 'Gene Expression'
      }else{
        colorNodeValue = 'A'
      }
      selectInput("NodeColor",label = h4("Color of nodes"), choices =  c('Accuracy value','Gene Expression'), selected = NodeColorType)
    })

    output$shiny_return <- renderPrint({
      input$current_node_id
    })
    # output$nodes_data_from_shiny <- renderDataTable( {
    #  if (!is.null(input$current_node_id) && !is.null(input$network_nodes)) {

    #     info <- data.frame(matrix(unlist(input$network_nodes),
    #                               ncol = dim(nodes)[1], byrow = T),
    #                        stringsAsFactors = FALSE)
    #     colnames(info) <- colnames(nodes)
    #    info[info$id == input$current_node_id, ]
    #  }
    # })

    # observeEvent(input$current_node_id, {
    #    visNetworkProxy("network") %>%
    #      visGetNodes()
    #   })

    # output$downloadData <- downloadHandler({
    #   data =  data()
    #    decisionName = input$decisions
    ##    nodes = data[[decisionName]]$nodes
    #    edges = data[[decisionName]]$edges
    #   g=graph_from_data_frame(edges, directed = FALSE, vertices = nodes)

    #    outfile <- tempfile(fileext = '1.png')
    #    svg(outfile, width = 400, height = 300)
    #    plot(g)
    #    dev.off()


    # })

    observeEvent(input$done, {
      stopApp(data_input)
    })


  }


  runGadget(ui, server, viewer = browserViewer())
}
