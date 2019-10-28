
options(list(scipen=999, launch.browser = TRUE)) #, shiny.port = 5155, shiny.host = "0.0.0.0"))

# online version
list.of.packages <- c("shiny", "shinydashboard", 
                      "DT", "data.table", 
                      "DBI", "odbc", "RODBC",
                      "plyr", "dplyr", "tidyr", 
                      "visNetwork", "igraph")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# load libraries
library(shiny)
library(shinydashboard)
library(DT)
library(data.table)
library(tidyr)
library(dplyr)
library(plyr)
library(visNetwork)
library(igraph)

source("config.R")

size.list <- c("Default", 
               "betweenness", "eigen_vector", 
               "closeness", "total_degree")

# load name list
name.list <- dbGetQuery(con,
                        paste0("select distinct ", person_id, 
                               " from ", table
                        ))
names(name.list)[1] <- 'person_id'

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
    br(),
    br(),
    h5("note: \\n
        closeness centrality is not suitable for disconnected graphs")
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

  # load name list

    updateSelectizeInput(session, 'person_id', 
                         choices = name.list$person_id, server = TRUE)
    
  
  # read query string
  df.all <- eventReactive(input$updateButton, {
    
    withProgress(message = 'Checking initial data...', value = 0.2, {
      
      validate(
        need(!is.null(input$person_id), "Invalid Person ID")
      )
      person_id_input <- unlist(strsplit(input$person_id, ","))
      
      # check input person IDs (special characters, etc.)
      validate(
        need(!is.na(person_id_input), "Invalid person ID")
      )
      
      # check if person_id exists
      person_id.exists <- dbGetQuery(con,
                                       paste0("select ", person_id, " from ", table, 
                                              " where ", person_id, " in (", 
                                              paste0(person_id_input, collapse=", "), ")"))
      
      validate(
        need(nrow(person_id.exists)>0, "Invalid or non-existent person ID")
      )
      
      # get data
      
      # one degree of separation
      temp1 <-  dbGetQuery(con,
                           paste0("select * from ", table, 
                                  " where ", person_id, " in (", 
                                  paste0(person_id_input, collapse=", "), ")"))
      
      temp1_inct <- temp1[[inct_id]]
      
      temp2 <-  dbGetQuery(con,
                           paste0("select * from ", table, 
                                  " where ", inct_id, " in (", 
                                  paste0(temp1_inct, collapse=", "), ")"))
      
      # second degree of separation
      
      temp2_person <- temp2[[person_id]]
      
      temp3 <-  dbGetQuery(con,
                           paste0("select * from ", table, 
                                  " where ", person_id, " in (", 
                                  paste0(temp2_person, collapse=", "), ")"))
      
      temp3_inct <- temp3[[inct_id]]
      
      df.all <-  dbGetQuery(con,
                            paste0("select * from ", table, 
                                   " where ", inct_id, " in (", 
                                   paste0(temp3_inct, collapse=", "), ")"))
      
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
                            c("person_id", "name")]
        
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
    df.name$color[df.name$person_id %in% unlist(strsplit(input$person_id, ","))] <- "red"
    
    nodes <- data.frame(id = df.name$person_id, 
                        label = df.name$name, 
                        title = df.name$person_id,
                        color = df.name$color,
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
    
    # convert person_id and inct_id to characters for filtering
    table.df <- table.df %>% 
      mutate(person_id = as.character(person_id),
             inct_id   = as.character(inct_id),
             name = as.factor(name),
             date = as.Date(date),
             type = as.factor(type)
      )
    
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
  
  # update size
  observe({
    
    # get node data
    df.name <- node.df()
    link <- link.df()
    
    link <- distinct(link)
    
    # create centrality data
    g <- graph_from_data_frame(link)
    
    centrality <- data.frame(
      betweenness = betweenness(g, v = V(g), directed = FALSE, normalized = TRUE),
      eigen_vector = eigen_centrality(g, directed = FALSE)$vector,
      closeness = suppressWarnings(closeness(g, v=V(g))),
      total_degree = degree(g, v=V(g), mode = "total", normalized = TRUE)          
    )
    
    centrality$person_id <- row.names(centrality)
    
    df.name <- left_join(df.name, centrality, by=c("person_id"="person_id"))
    
    df.name$Default <- 1
    
    # color
    df.name$color <- "lightblue" 
    df.name$color[df.name$person_id %in% unlist(strsplit(input$person_id, ","))] <- "red"
    
    df.name$value <- df.name[, input$node_size] * 10
    
    # update network
    visNetworkProxy("network") %>%
      visUpdateNodes(nodes = data.frame(id = df.name$person_id, 
                                        label = df.name$name, 
                                        title = paste0(df.name$person_id, "; ", round(df.name$value/10, 2)),
                                        color = df.name$color,
                                        value = df.name$value,
                            stringsAsFactors=FALSE)
      )
  })
}

shinyApp(ui, server, options = list(launch.browser = TRUE))
