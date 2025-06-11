# app.R ─────────────────────────────────────────────────────────────────
# 0. Packages -----------------------------------------------------------
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse, jsonlite, igraph, tidygraph, ggraph, widyr,
  visNetwork, shiny, shinydashboard, DT, ggforce
)

# 1. Import & clean data ------------------------------------------------
MC3         <- fromJSON("data/MC3_graph.json")
MC3_schema  <- fromJSON("data/MC3_schema.json")

mc3_nodes <- as_tibble(MC3$nodes)
mc3_edges <- as_tibble(MC3$edges)

mc3_nodes_cleaned <- mc3_nodes %>%               # ⇢ Section 4.1 in your notes
  mutate(id = as.character(id)) %>%
  filter(!is.na(id)) %>%
  distinct(id, .keep_all = TRUE) %>%
  select(-thing_collected)

mc3_edges_cleaned <- mc3_edges %>%               # ⇢ Section 4.2
  rename(from_id = source, to_id = target) %>%
  mutate(across(c(from_id, to_id), as.character)) %>%
  filter(from_id %in% mc3_nodes_cleaned$id,
         to_id   %in% mc3_nodes_cleaned$id) %>%
  filter(!is.na(from_id) & !is.na(to_id))

node_index_lookup <- mc3_nodes_cleaned %>%
  mutate(.row_id = row_number()) %>%
  select(id, .row_id)

mc3_edges_indexed <- mc3_edges_cleaned %>%
  left_join(node_index_lookup, by = c("from_id" = "id")) %>%
  rename(from = .row_id) %>%
  left_join(node_index_lookup, by = c("to_id" = "id")) %>%
  rename(to = .row_id) %>%
  select(from, to, is_inferred, type) %>%
  filter(!is.na(from) & !is.na(to))

used_node_indices <- sort(unique(c(mc3_edges_indexed$from,
                                   mc3_edges_indexed$to)))

mc3_nodes_final <- mc3_nodes_cleaned %>%
  slice(used_node_indices) %>%
  mutate(new_index = row_number())

old_to_new_index <- tibble(
  old_index = used_node_indices,
  new_index = seq_along(used_node_indices)
)

mc3_edges_final <- mc3_edges_indexed %>%
  left_join(old_to_new_index, by = c("from" = "old_index")) %>%
  rename(from_new = new_index) %>%
  left_join(old_to_new_index, by = c("to" = "old_index")) %>%
  rename(to_new = new_index) %>%
  select(from = from_new, to = to_new, is_inferred, type)

# 2. Helper functions ---------------------------------------------------
# 2.1 Relationship graph – keep Entity & Relationship nodes -------------
build_rel_graph <- function(nodes_tbl, edges_tbl) {
  g <- tbl_graph(nodes = nodes_tbl,
                 edges = edges_tbl,
                 directed = TRUE)
  
  # handle Friends / Colleagues as undirected
  undirected_ids <- g %>%
    activate(nodes) %>%
    filter(type == "Relationship",
           sub_type %in% c("Friends", "Colleagues")) %>%
    pull(id)
  
  g %>% activate(edges) %>%
    mutate(direction = if_else(to %in% undirected_ids | from %in% undirected_ids,
                               "undirected", "directed"))
}

# 2.2 Communication projection (Person ↔ Communication events) ----------
build_comm_proj <- function(nodes_tbl, edges_tbl, min_weight = 0) {
  
  persons <- nodes_tbl$id[nodes_tbl$type == "Entity" &
                            nodes_tbl$sub_type == "Person"]
  comms   <- nodes_tbl$id[nodes_tbl$type == "Event"]
  
  e_bip <- edges_tbl %>%
    filter((from %in% persons & to %in% comms) |
             (to %in% persons & from %in% comms)) %>%
    mutate(person = if_else(from %in% persons, from, to),
           comm   = if_else(from %in% comms,  from, to)) %>%
    select(person, comm) %>% distinct()
  
  proj <- e_bip %>%
    widyr::pairwise_count(person, comm, sort = FALSE, upper = FALSE) %>%
    filter(n >= min_weight)
  
  tbl_graph(nodes = nodes_tbl %>% filter(id %in% persons),
            edges = proj %>%
              transmute(from = item1, to = item2, weight = n),
            directed = FALSE)
}

# 2.3 VisNetwork & ggraph wrappers --------------------------------------
vis_from_graph <- function(g, clusters_tbl) {
  nodes_df <- g %>% activate(nodes) %>% as_tibble() %>%
    mutate(label = name %||% id,
           id    = row_number()) %>%    # visNetwork needs 1…N ids
    left_join(clusters_tbl, by = c("id" = "node")) %>%
    mutate(group = cluster)
  
  edges_df <- g %>% activate(edges) %>% as_tibble() %>%
    mutate(value = weight %||% 1,
           arrows = if_else(direction == "directed", "to", ""))
  
  visNetwork(nodes_df, edges_df, height = "350px") %>%
    visNodes(shape = "dot", size = 15) %>%
    visEdges(smooth = FALSE) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visLegend()
}

plot_cluster_graph <- function(g, clusters_tbl) {
  if (gorder(g) == 0) return(NULL)
  
  g <- g %>% activate(nodes) %>%
    left_join(clusters_tbl, by = c("id" = "node")) %>%
    mutate(cluster = factor(cluster))
  
  ggraph(g, layout = "fr") +
    geom_edge_link(aes(width = weight %||% 1), alpha = .3) +
    geom_node_point(aes(colour = cluster, size = centrality_degree()), show.legend = FALSE) +
    geom_node_text(aes(label = name %||% id, colour = cluster),
                   repel = TRUE, size = 3) +
    theme_void()
}

`%||%` <- function(a, b) if (!is.null(a)) a else b   # little helper

# 3. Build the master graphs once --------------------------------------
g_relationship_master <- build_rel_graph(mc3_nodes_final, mc3_edges_final)

# 4. UI -----------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "MC3 Exploration"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Explorer", tabName = "explore", icon = icon("project-diagram"))
    ),
    hr(style = "border-top: 1px solid #3c8dbc"),
    
    tabBox(id = "plot_choice", width = NULL,
           tabPanel("Relationship", value = "rel"),
           tabPanel("Communication", value = "comm")),
    br(),
    
    checkboxGroupInput(
      "entity_types", "Include Entity Types",
      choices  = c("Person", "Organization", "Vessel", "Group", "Location"),
      selected = c("Person", "Organization", "Vessel", "Group", "Location")
    ),
    
    checkboxGroupInput(
      "rel_types", "Include Relationship Sub-types",
      choices  = sort(unique(mc3_nodes_final$sub_type[
        mc3_nodes_final$type == "Relationship"])),
      selected = sort(unique(mc3_nodes_final$sub_type[
        mc3_nodes_final$type == "Relationship"]))
    ),
    
    selectizeInput("entity", "Focus Entity (optional)",
                   choices = sort(unique(mc3_nodes_final$name %||% mc3_nodes_final$id)),
                   multiple = FALSE),
    
    conditionalPanel(
      condition = "input.plot_choice == 'comm'",
      sliderInput("min_comm", "Min. Communication Frequency", 0, 20, 0)
    ),
    
    selectInput("cluster_alg", "Clustering Method",
                choices = c("Louvain" = "louvain",
                            "Leiden"  = "leiden",
                            "Walktrap"= "walktrap",
                            "Edge-Betweenness" = "eb"),
                selected = "louvain")
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "explore",
              fluidRow(
                box(title = "Network", width = 6,
                    visNetworkOutput("net_vis", height = "350px")),
                box(title = "Clustering Overview", width = 6,
                    plotOutput("plot_cluster", height = "350px"))
              ),
              fluidRow(
                box(width = 12,
                    tabsetPanel(type = "tabs",
                                tabPanel("Nodes", DTOutput("tbl_nodes")),
                                tabPanel("Edges", DTOutput("tbl_edges"))
                    )
                )
              )
      )
    )
  )
)

# 5. Server -------------------------------------------------------------
server <- function(input, output, session) {
  
  # 5.1 Reactive graph according to UI filters ---------------------------
  g_current <- reactive({
    
    # choose base graph
    g0 <- if (input$plot_choice == "rel") {
      g_relationship_master
    } else {
      build_comm_proj(mc3_nodes_final, mc3_edges_final,
                      min_weight = input$min_comm)
    }
    
    # filter by entity types & relationship sub-types
    g1 <- g0 %>% activate(nodes) %>%
      filter(!(type == "Entity"       & !sub_type %in% input$entity_types)) %>%
      filter(!(type == "Relationship" & !sub_type %in% input$rel_types))
    
    # focus on a single entity (optional)
    if (nzchar(input$entity)) {
      root <- g1 %>% activate(nodes) %>% 
        filter(name == input$entity | id == input$entity) %>% 
        pull(row_number())
      if (length(root) > 0) {
        vids <- igraph::subcomponent(as.igraph(convert(g1, to_undirected)), root)
        g1   <- g1 %>% activate(nodes) %>% filter(node_is_in(vids))
      }
    }
    g1
  })
  
  # 5.2 Clustering -------------------------------------------------------
  cluster_tbl <- reactive({
    g <- g_current()
    if (gorder(g) == 0) return(tibble(node = integer(), cluster = integer()))
    
    algo <- switch(input$cluster_alg,
                   louvain = igraph::cluster_louvain,
                   leiden  = igraph::cluster_leiden,
                   walktrap= igraph::cluster_walktrap,
                   eb      = igraph::cluster_edge_betweenness)
    
    memb <- algo(as.igraph(g),
                 weights = edge_attr(g, "weight", default = 1))$membership
    tibble(node = seq_along(memb), cluster = memb)
  })
  
  # 5.3 Renderers --------------------------------------------------------
  output$net_vis <- renderVisNetwork({
    vis_from_graph(g_current(), cluster_tbl())
  })
  
  output$plot_cluster <- renderPlot({
    plot_cluster_graph(g_current(), cluster_tbl())
  })
  
  output$tbl_nodes <- renderDT({
    as_tibble(g_current(), active = "nodes")
  })
  
  output$tbl_edges <- renderDT({
    as_tibble(g_current(), active = "edges")
  })
}

# 6. Launch -------------------------------------------------------------
shinyApp(ui, server)
