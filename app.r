require("shiny")
require("data.table")
require("visNetwork")
require("igraph")


comingOutGraph <- graph_from_data_frame(fread("data/comingOutGraph.txt"), 
                                        directed = F, vertices =  readLines("data/COverticesNames.txt"))
comingOutGraph <- simplify(comingOutGraph, remove.multiple = T, remove.loops = T)
comingOutGraph.wc <- cluster_walktrap(comingOutGraph)
comingOutGraph.members <- membership(comingOutGraph.wc)
V(comingOutGraph)$group <- comingOutGraph.members
comingOut.visNetwork <- toVisNetworkData(comingOutGraph)

server <- function(input, output) {
  
  output$network <- renderVisNetwork({
    visNetwork(node = comingOut.visNetwork$nodes, edges = comingOut.visNetwork$edges,
               #width = "100%", height = "100%",
               # main = "<p style=\"font-family:Century Gothic;font-size:54px;text-align:center\">#NationalComingOutDay</p>",
               # submain = "<p style=\"font-size:36px;font-family:Century Gothic;font-size:26px;text-align:center\">Topic Modeling and LGBTQ+ Self-Identification</p>
               # <p style=\"font-size:36px;font-family:Century Gothic;font-size:26px;text-align:center\">Made by <a href = \"http://bit.ly/FrankWebb\">Frank Webb</a></p>
               # </p>",
               background = "#CAFAD3") %>%
      visPhysics(timestep = 0.1, adaptiveTimestep = TRUE,
                 barnesHut = list(springLength = 75,
                                  avoidOverlap = 0.2)) %>%
      visEdges(smooth = FALSE, width = 0.5
               ,color = list(inherit = "both", opacity = 1)
      ) %>%
      visNodes(shape = "text",font = list(face = "Century Gothic", size = "25"), borderWidth = 0, group = comingOutGraph.members) %>% 
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, algorithm = "all", hideColor = "rgba(0,0,0,0)"))
  })
}

ui <- fillPage(
  tags$header(HTML(
                   "<p style=\"font-family:Century Gothic;font-size:54px;text-align:center\">#NationalComingOutDay</p>
                   <p style=\"font-size:36px;font-family:Century Gothic;font-size:26px;text-align:center\">Topic Modeling and LGBTQ+ Self-Identification</p>
                   <p style=\"font-size:36px;font-family:Century Gothic;font-size:26px;text-align:center\">Made by <a href = \"http://bit.ly/FrankWebb\">Frank Webb</a></p>
                   </p>")),
  visNetworkOutput("network", height = "70%"),
  tags$footer(HTML("<body style = background-color:#CAFAD3><p style=\"font-family:Century Gothic;font-size:14px;text-align:center\">Thanks to <a href = \"https://twitter.com/karami_amir?lang=en\">Dr. Amir Karami</a>,
                         <a href = \"https://twitter.com/slisSS\">University of South Carolina School of Library and Information Science</a>, and 
                 <a href = \"https://twitter.com/HRC\">The Human Rights Campaign</a></p></body>"))
  
)

shinyApp(ui = ui, server = server)