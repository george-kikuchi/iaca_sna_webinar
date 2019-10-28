
# to do
# legend; visNetworkProxy does not take visLegend
# use df adn group?


# max file size
options(shiny.maxRequestSize = 500*1024^2)

# check libraries again (just double checking)
list.of.packages <- c("shiny", "shinydashboard", 
                      "data.table", "tidyr", "dplyr", "plyr",
                      "visNetwork", "igraph", "sqldf", "readxl", "leaflet")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='http://cran.us.r-project.org')


# load libraries
library(shiny)
library(shinydashboard)
#library(DT)
library(data.table)
library(tidyr)
library(dplyr)
library(plyr)
library(visNetwork)
library(igraph)
library(sqldf)
library(readxl)
library(leaflet)

size.list <- c("Default", 
               "betweenness", "eigen_vector", 
               "closeness", "total_degree")

# user interface ---------
ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "Network AnalyzeR - Excel edition", titleWidth = 400),
  
  dashboardSidebar(

    selectizeInput('person_id', choices = NULL, 
                   label="Person ID",
                   multiple = TRUE, width = 350),
    
    br(),
    div(actionButton("updateButton", "Update the chart"), 
        style = "text-align:center"),
    br(),
    selectizeInput(
      'node_size', 'Change size by Centrality', choices = size.list, 
      multiple = FALSE, width = 350
    ),
    h5("note: closeness centrality is not suitable for disconnected graphs"),
    selectizeInput(
      'node_color', 'Change color by attributes',  choices = NULL, 
      multiple = FALSE, width = 350
    )
  ) ,
  dashboardBody(
    fluidRow(
      box(title = "network chart", collapsible = TRUE, collapsed = FALSE,
          status = "danger", solidHeader = TRUE, width= 12,
          visNetworkOutput("network", width = "100%", height = "600px")
      )
    ),
    box(title = "Raw Data", 
        div(style = 'overflow-x: scroll; overflow-y: scroll; height:400px', 
            DT::dataTableOutput("table", width = "auto", height = "100%") 
        ),
        collapsible = TRUE, collapsed = FALSE,
        status = "primary", solidHeader = TRUE, width= 12
    )
  )
)

# server-side function --------
server <- function(input, output, session){

  # modal for file selection
  query_modal <- modalDialog(
    title = "Select your Excel file that has inct_id and person_id",
    fileInput("file1", "Choose Excel File for network chart",
              multiple = FALSE,
              accept = c(".xls",".XLS", ".xlsx")),
    fileInput("file2", "Choose Excel File to color-code nodes (optional)",
              multiple = FALSE,
              accept = c(".xls",".XLS", ".xlsx")),
    easyClose = F,
    footer = tagList(
      actionButton("file_import", "Preview Data"),
      h5("click the preview data button only after 'upload complete' is shown")
    )
  )
  
  query_modal2 <- modalDialog(
    
    tags$head(tags$style(HTML(".shiny-split-layout > div {
                                overflow: visible;}"))),
    
    h4("select column names for incident_id, person_id, name, and type"),
    
    splitLayout(
        selectizeInput('inct_id_mapping', choices = NULL, 
                   label="incident id", multiple = FALSE),
        selectizeInput('person_id_mapping', choices = NULL, 
                   label="person id", multiple = FALSE),
        selectizeInput('name_mapping', choices = NULL, 
                   label="name", multiple = FALSE),
        selectizeInput('type_mapping', choices = NULL, 
                   label="type", multiple = FALSE)
    ),
  
      tableOutput('preview1'),
    
    hr(),
    h4("select a person_id column for the node attribute file 
       (blank if no file was selected)"),
    
      selectizeInput('person_id_mapping2', choices = NULL, 
                   label="person id", multiple = FALSE),
      
    #splitLayout(
      
    #  cellArgs = list(style = "padding: 40px"),
    
      tableOutput('preview2'),

      #)
    
    easyClose = F,
    
    footer = tagList(
      actionButton("file_import2", "Import Data"),
      
      h5("make sure column selections are not duplicated")
    ),
    size = "l"
  )
  
  # Show the model on start up ...
  showModal(query_modal)
  
  # reactiveVal to store the dataset
  my_dataset  <- reactiveVal()
  my_dataset2 <- reactiveVal()
  
  observeEvent(input$file_import, {
    removeModal()
    
    withProgress(message = 'Checking data...', value = 0.4, {  
    
    # Your query here
    my_data <- read_excel(input$file1$datapath)
    my_dataset(my_data)
    
    if(!is.null(input$file2$datapath)){
      my_data2 <- read_excel(input$file2$datapath)
      my_dataset2(my_data2)
    }
    
    })
    
    # show the data preview and column specification modal
    showModal(query_modal2)
    
  })
  
  # creating preview table
  output$preview1 <- renderTable(head(my_dataset(),  3))
  output$preview2 <- renderTable(head(my_dataset2(), 3))  
  
  # column mapping
  observeEvent(input$file_import, {
    updateSelectizeInput(session, 'inct_id_mapping', 
                         choices = names(my_dataset()), 
                         selected = ifelse(any(names(my_dataset()) %in% 'inct_id'), 'inct_id',
                                           names(my_dataset())[1]),  server = TRUE)
  })
  
  observeEvent(input$file_import, {
    updateSelectizeInput(session, 'person_id_mapping', 
                         choices = names(my_dataset()), 
                         selected = ifelse(any(names(my_dataset()) %in% 'person_id'), 'person_id',
                                           names(my_dataset())[2]), server = TRUE)
  })
  
  observeEvent(input$file_import, {
    updateSelectizeInput(session, 'name_mapping', 
                         choices = names(my_dataset()), 
                         selected = ifelse(any(names(my_dataset()) %in% 'name'), 'name',
                                           names(my_dataset())[3]), server = TRUE)
  })
  
  observeEvent(input$file_import, {
    updateSelectizeInput(session, 'type_mapping', 
                         choices = names(my_dataset()), 
                         selected = ifelse(any(names(my_dataset()) %in% 'type'), 'type',
                                           names(my_dataset())[4]), server = TRUE)
  })
  
  # secondary file's person_id
  observeEvent(input$file_import, {
    req(my_dataset2())
    updateSelectizeInput(session, 'person_id_mapping2', 
                         choices = names(my_dataset2()), 
                         selected = ifelse(any(names(my_dataset2()) %in% 'person_id'), 'person_id',
                                           names(my_dataset2())[1]), server = TRUE)
  })

  
  # load name list for search
  observeEvent(input$file_import2, {
    
    validate(
      need(length(unique(c(input$inct_id_mapping, input$person_id_mapping, 
             input$name_mapping, input$type_mapping)))==4,
           "column selection duplicated"
    ))
    
   # remove modal
    removeModal()
    
    # update the search box list
    label.df <- data.frame(
      person_id = my_dataset()[[input$person_id_mapping]],
      label = paste0(my_dataset()[[input$person_id_mapping]],
                                 ": ",
                                 my_dataset()[[input$name_mapping]]))
    
    #browser()
    updateSelectizeInput(session, 'person_id', 
                         choices = label.df, server = TRUE,
                         options = list(labelField='label', searchField='label', 
                                        valueField='label', render = I( 
                                          "{ 
                                          option: function(item, escape) { 
                                          return '<div>' + escape(item.label) + 
                                          '</div>'; 
                                          } }"
                                      )
                                        )
                                        )
      
    
    #updateSelectizeInput(session, 'person_id', 
    #                     choices = my_dataset()[[input$person_id_mapping]], server = TRUE)
    
    # update the attribute file
    if(!is.null(my_dataset2())){
      updateSelectizeInput(session, 'node_color', selected = 'Default',
                         choices = c("Default", names(my_dataset2())[!names(my_dataset2()) 
                                                        %in% input$person_id_mapping2])
      )
    } else {
      updateSelectizeInput(session, 'node_color', selected = 'Default',
                           choices = c("Default"))
    }
  })
  
  # read query string
  df.all <- eventReactive(input$updateButton, {
    
    withProgress(message = 'Checking initial data...', value = 0.2, {
      
      validate(
        need(!is.null(input$person_id), "Invalid Person ID")
      )
      
      # if condition to check column type (numeric or character)
      if(class(my_dataset()[[input$person_id_mapping]])=="numeric"){
        person_id_input <- as.numeric(gsub("(.*):.*", "\\1", input$person_id))
      } else {
        person_id_input <- gsub("(.*):.*", "\\1", input$person_id)
      }
      
      #person_id_input <- unlist(strsplit(input$person_id, ","))
      
      # check input person IDs (special characters, etc.)
      validate(
        need(!is.na(person_id_input), "Invalid Person ID")
      )
      
      # importing data
      sample <- my_dataset() 
      
      # rename columns
      colnames(sample)[colnames(sample) == input$inct_id_mapping] <- "inct_id"
      colnames(sample)[colnames(sample) == input$person_id_mapping] <- "person_id"
      colnames(sample)[colnames(sample) == input$name_mapping] <- "name"
      
      if(is.null(input$type_mapping)) {
        sample$type <- ""
      } else {
        colnames(sample)[colnames(sample) == input$type_mapping] <- "type"
      }
        
      # ensuring that person id is numeric
      #sample <- sample %>% 
      #  mutate(person_id = as.numeric(person_id))
      
      
      if(class(my_dataset()[[input$person_id_mapping]])=="numeric"){
        # check if person_id exists
         person_id.exists <- sqldf(
                              paste0("select person_id from sample 
                                      where person_id in (", 
                                     paste0(person_id_input, collapse=", "), ")"))
      
          validate(
            need(nrow(person_id.exists)>0, "Invalid or non-existent Offender ID")
          )
      
        # get data

        # one degree of separation
        
          temp1 <- sqldf(
            paste0("select * from sample where person_id in (", 
                 paste0(person_id_input, collapse=", "), ")"))
        
          temp1_incident <- temp1$inct_id
        
          temp2 <- sqldf(
            paste0("select * from sample where inct_id in (", 
                 paste0(temp1_incident, collapse=", "), ")"))
        
          # second degree of separation
        
         temp2_person <- temp2$person_id
        
          temp3 <- sqldf(
            paste0("select * from sample where person_id in (", 
                   paste0(temp2_person, collapse=", "), ")"))
        
          temp3_incident <- temp3$inct_id
        
          df.all <- sqldf(
            paste0("select * from sample where inct_id in (", 
                   paste0(temp3_incident, collapse=", "), ")"))
      } else {
        # character type
        # check if person_id exists
        person_id.exists <- sqldf(
          paste0("select person_id from sample 
                 where person_id in ('", 
                 paste0(person_id_input, collapse="', '"), "')"))
        
        validate(
          need(nrow(person_id.exists)>0, "Invalid or non-existent Offender ID")
        )
        
        # get data
        
        # one degree of separation
        
        temp1 <- sqldf(
          paste0("select * from sample where person_id in ('", 
                 paste0(person_id_input, collapse="', '"), "')"))
        
        temp1_incident <- temp1$inct_id
        
        temp2 <- sqldf(
          paste0("select * from sample where inct_id in ('", 
                 paste0(temp1_incident, collapse="', '"), "')"))
        
        # second degree of separation
        
        temp2_person <- temp2$person_id
        
        temp3 <- sqldf(
          paste0("select * from sample where person_id in ('", 
                 paste0(temp2_person, collapse="', '"), "')"))
        
        temp3_incident <- temp3$inct_id
        
        df.all <- sqldf(
          paste0("select * from sample where inct_id in ('", 
                 paste0(temp3_incident, collapse="', '"), "')"))
      }
      
       
      # shape
      df.all$shape <- "dot"
      df.all$shape[df.all$person_id %in% person_id_input] <- "square"
      
      
      if (nrow(df.all) ==0){
        return(NULL)
      }  else {
        df.all 
      }
    })    
  })   
  
  ### create linkage data ----
  
  link.df <- 
    
    eventReactive(input$updateButton, {
      
      req(df.all())  
      
      validate(
        need(length(unique(df.all()$person_id))>1, "No known associates")
      )
      
      withProgress(message = 'Checking network links...', value = 0.4, {   
        
        inct_id <- "inct_id"
        person_id <- "person_id"
        
        
      # two mode to one mode conversion
        g <- graph.data.frame(df.all(), directed=FALSE)  ## Convert to an igraph network
        
        V(g)$type <- bipartite_mapping(g)$type 
       
        g <- graph.data.frame(df.all()[,1:2], directed=FALSE)  ## Convert to an igraph network
        
        V(g)$type <- bipartite_mapping(g)$type 
        
        projected_g <- bipartite_projection(g, multiplicity = TRUE)
        
        link <- as.data.frame(as_edgelist(projected_g$proj2), 
                              stringsAsFactors = FALSE)
        
        link$value = E(projected_g$proj2)$weight
        
        names(link)[1:2] <- c("person_id1", "person_id2")
        
        link
        
      })
    })
  
  # create node data
  node.df <- 
    
    eventReactive(input$updateButton, {
      
      withProgress(message = 'Checking network nodes...', value = 0.8, {   
        
        req(df.all())  
       
        # create node data
        df.name <- df.all()[!duplicated(df.all()$person_id), 
                            c("person_id", "name", "shape")]
        
        # convert person_id to characters for graph data
        df.name$person_id <- as.character(df.name$person_id)
        
        df.name
        
      })
    })
  
  
  # render the network diagram
  output$network <- renderVisNetwork({
    
    validate(
      need(nrow(link.df())>0, "No known associates")
    )
    
    df.name <- node.df()
    link <- link.df()
    
    # color
    df.name$color <- "lightblue" 
    
   
    nodes <- data.frame(id = df.name$person_id, 
                        label = df.name$name, 
                        title = df.name$person_id,
                        color = df.name$color,
                        shape = df.name$shape,
                        value = 1,
                        stringsAsFactors=FALSE)
    
    edges <- data.frame(from = as.character(link$person_id1), 
                        to = as.character(link$person_id2), 
                        value = link$value,
                        title = link$value,
                        color = "pink",
                        stringsAsFactors=FALSE)
    
    visNetwork(nodes, edges, width = "100%") %>% 
      visOptions(highlightNearest = list(enabled = T, degree = 1), 
                 nodesIdSelection = TRUE, 
                 manipulation = TRUE) 
  })
  
  # output records table
  output$table <- DT::renderDataTable({
    
    req(df.all())
    
    table.df <- df.all() 
    
    #browser()
    
    # convert person_id and incident_id to characters for filtering
    table.df <- table.df %>% 
      dplyr::mutate(person_id = as.character(person_id),
             inct_id   = as.character(inct_id),
             name = as.factor(name),
             date = as.Date(date),
             type = as.factor(type)
      ) %>% 
      dplyr::select(-shape)
    
    DT::datatable(table.df, filter="top", selection = 'none', rownames = FALSE,

                  extensions = 'Buttons',
                  
                  options = list(
                    
                    dom = 'lBT<"clear">frtip',
                    buttons = c('excel', 'copy'),
                    
                    pageLength = 25,
                    lengthMenu = c(5, 10, 25, 100, 1000),
                    search = list(regex = TRUE, caseInsensitive = TRUE),
                    searchHighlight = TRUE
                  ))
  })
  
  # update size and color 
  observe({
    
    # get node data
    df.name <- node.df()
    link <- link.df()
    
    link <- distinct(link)
    
    # create centrality data
    if(input$node_size != "Default"){
      g <- graph_from_data_frame(link)
    
      centrality <- data.frame(
        betweenness = betweenness(g, v = V(g), directed = FALSE, normalized = TRUE),
        eigen_vector = eigen_centrality(g, directed = FALSE)$vector,
        closeness = suppressWarnings(closeness(g, v=V(g))),
        total_degree = degree(g, v=V(g), mode = "total", normalized = TRUE)          
      )
    
      centrality$person_id <- row.names(centrality)
    
      df.name <- left_join(df.name, centrality, by=c("person_id"="person_id"))
    }
    
    df.name$Default <- 1
    
    # color
    df.name$color <- "lightblue" 
    
    # if else for categorical vs numerical
    
    if(!is.null(my_dataset2()) & input$node_color != "Default"){
    
      #browser()
      
      # load the attribute table and select ensure that person_id is unique
      attributes.df <- my_dataset2() %>% 
        dplyr::distinct_(input$person_id_mapping2, .keep_all = TRUE)
        
      # rename column
      colnames(attributes.df)[colnames(attributes.df) == input$person_id_mapping2] <- "person_id"
      attributes.df$person_id <- as.character(attributes.df$person_id)
      
      # join attribute data
      df.name <- df.name %>% 
        left_join(attributes.df, by=("person_id" = "person_id"))
      
      # temp data for coloring based on user selection
      legend.df.temp <- df.name %>% 
        dplyr::select_("person_id", input$node_color) %>% 
        na.omit() 
        
      # coloring; if condition by column type
      if (nrow(legend.df.temp)>0){
        if(class(legend.df.temp[, input$node_color])=="numeric"){
          
          legend.df.temp  <- legend.df.temp %>% 
            dplyr::rename_(color = input$node_color) %>% 
            dplyr::mutate( color = ifelse(color == 0, "lightblue",
                                 ifelse(color == 1, "orange",
                                 ifelse(color >= 2, "red", "lightblue"))))
          
          pal <- legend.df.temp$pal
          
          df.name$color <- NULL
          
          df.name <- df.name %>% 
            left_join(legend.df.temp, by=c("person_id" = "person_id"))
          
          
        } else {
          pal <- colorFactor(topo.colors(nrow(legend.df.temp)), 
                             legend.df.temp[, input$node_color])
          df.name <- df.name %>%
            dplyr::mutate_(color = input$node_color) %>% 
            dplyr::mutate(color = pal(color)) 
        }

      } else {
        
        df.name <- df.name %>% 
          dplyr::mutate(color = "lightblue")
      }
      
      # cleaning data
      df.name <- df.name %>% 
        dplyr::mutate(borderWidth = ifelse(is.na(color), 1, 12),
                      color = ifelse(is.na(color), "lightblue", color))
      
    } else {
      
      df.name$color <- "lightblue"
      df.name$borderWidth <- 8
    }
      
    # legends
    
    #browser()
    
    if(FALSE){
    lnodes <- dplyr::filter_(df.name, !is.na(input$node_color)) %>% 
      dplyr::select_(input$node_color, "color") %>% 
      dplyr::mutate_(label = input$node_color) %>% 
      dplyr::mutate( shape = "ellipse") %>% 
      dplyr::select( label, color, shape) %>% 
      dplyr::distinct() %>% 
      dplyr::mutate(id = ifelse(n() > 0, 1:n(), 1))
    
    #browser() # ver 2 does not like id number
    lnodes <- dplyr::select(lnodes, -id)
    }
    
    #sizing
    df.name$value <- df.name[, input$node_size] * 10

    color_label <- input$node_color
    
    if(input$node_size != "Default" & input$node_color != "Default"){
      df.name <- df.name %>% 
        dplyr::mutate(title =  paste0(df.name$name, "; ", df.name$person_id, "; centrality ", 
                               round(df.name$value/10, 2))) %>% 
        dplyr::mutate_(title_temp = input$node_color) %>% 
        dplyr::mutate(title = paste0(title, "; ", color_label, " ", title_temp))
            
    } else if(input$node_size != "Default" & input$node_color == "Default"){
      df.name <- df.name %>% 
        dplyr::mutate(title =  paste0(df.name$name, "; ", 
                                      df.name$person_id, "; centrality ", 
                                      round(df.name$value/10, 2))) 
    } else if(input$node_size == "Default" & input$node_color != "Default"){
      df.name <- df.name %>% 
        dplyr::mutate(title =  paste0(df.name$name, "; ", df.name$person_id)) %>% 
        dplyr::mutate_(title_temp = input$node_color) %>% 
        dplyr::mutate(title = paste0(title, "; ", color_label, " ", title_temp))
      
    } else {
      df.name <- df.name %>%  
        dplyr::mutate(title =  paste0(df.name$name, "; ", df.name$person_id))
    }
    
    # update network
    visNetworkProxy("network") %>%
      visUpdateNodes(nodes = data.frame(id = df.name$person_id, 
                                        label = df.name$name, 
                                        title = df.name$title,
                                        shape = df.name$shape,
                                        color = df.name$color,
                                        value = df.name$value,
                            stringsAsFactors=FALSE,
                            legend = TRUE)
      ) #%>% 
      #visLegend(addNodes = lnodes, 
      #          useGroups = FALSE)
  })
}

shinyApp(ui, server, options = list(launch.browser = TRUE))
