library(shiny)
library(pacman)
pacman::p_load(
  tidyverse, jsonlite, dplyr, igraph, tidygraph, ggraph,
  scales, visNetwork, SmartEDA, widyr, ggforce, tidytext, tidyr, DT)

MC3 <- fromJSON("data/MC3_graph.json")
mc3_nodes <- as_tibble(MC3$nodes)
mc3_edges <- as_tibble(MC3$edges)

# Clean nodes
mc3_nodes_cleaned <- mc3_nodes %>%
  mutate(id = as.character(id)) %>%
  filter(!is.na(id)) %>%
  distinct(id, .keep_all = TRUE) %>%
  select(-thing_collected)

# Clean edges
mc3_edges_cleaned <- mc3_edges %>%
  rename(from_id = source, to_id = target) %>%
  mutate(across(c(from_id, to_id), as.character)) %>%
  filter(from_id %in% mc3_nodes_cleaned$id,
         to_id   %in% mc3_nodes_cleaned$id) %>%
  filter(!is.na(from_id), !is.na(to_id))

# Reindex
node_index_lookup <- mc3_nodes_cleaned %>%
  mutate(.row_id = row_number()) %>%
  select(id, .row_id)

mc3_edges_indexed <- mc3_edges_cleaned %>%
  left_join(node_index_lookup, by = c("from_id" = "id")) %>%
  rename(from = .row_id) %>%
  left_join(node_index_lookup, by = c("to_id" = "id")) %>%
  rename(to   = .row_id) %>%
  select(from, to, is_inferred, type) %>%
  filter(!is.na(from) & !is.na(to))

used_node_indices <- sort(unique(c(mc3_edges_indexed$from, mc3_edges_indexed$to)))
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

# Relationships
event_ids <- mc3_nodes_final %>% filter(type == "Event") %>% pull(new_index)
location_ids <- mc3_nodes_final %>% filter(sub_type == "Location") %>% pull(new_index)
blocked_ids <- c(event_ids, location_ids)

node_lookup <- mc3_nodes_final %>%
  filter(!new_index %in% blocked_ids) %>%
  select(new_index, type, sub_type, label)

rel_nodes <- node_lookup %>%
  filter(type == "Relationship") %>%
  mutate(
    rel_subtype = sub_type,
    rel_id = as.character(new_index)
  ) %>%
  select(rel_id, rel_subtype)

rel_ids <- rel_nodes$rel_id
undirected_types <- c("Friends", "Colleagues") # used for direction logic

edges_all_base <- mc3_edges_final %>%
  filter(from %in% node_lookup$new_index | to %in% node_lookup$new_index) %>%
  filter(!from %in% blocked_ids, !to %in% blocked_ids) %>%
  mutate(across(c(from, to), as.character))

undir_edges <- edges_all_base %>%
  filter(to %in% rel_ids) %>%
  left_join(rel_nodes, by = c("to" = "rel_id")) %>%
  filter(rel_subtype %in% undirected_types) %>%
  transmute(from = to, to = from, direction = "undirected")

edges_all_base <- bind_rows(
  edges_all_base %>% mutate(direction = "directed"),
  undir_edges
)

entity_rel_tbl <- edges_all_base %>%
  filter(from %in% rel_ids | to %in% rel_ids) %>%
  mutate(
    rel_id = if_else(from %in% rel_ids, from, to),
    entity = if_else(from %in% rel_ids, to, from)
  ) %>%
  select(rel_id, entity)

triplets <- entity_rel_tbl %>%
  left_join(rel_nodes,  by = "rel_id") %>%
  group_by(rel_id) %>%
  filter(n_distinct(entity) >= 2) %>%               # keep ≥2-party relations
  summarise(
    pair_mat    = list(t(combn(sort(unique(entity)), 2))),  # all pairs
    rel_subtype = first(rel_subtype),
    .groups     = "drop"
  ) %>%
  unnest(pair_mat) %>%                              # one row per pair
  transmute(
    rel_id,
    rel_subtype,
    e1      = pair_mat[, 1],
    e2      = pair_mat[, 2],
    pair_id = paste(pmin(e1, e2), pmax(e1, e2), sep = "_")
  ) %>%
  arrange(pair_id, rel_subtype, rel_id) %>%
  group_by(pair_id) %>%                             # one edge per pair
  slice(1) %>%
  ungroup()

# Unique subtypes
all_rel_subtypes <- mc3_nodes_final %>%
  filter(type == "Relationship") %>%
  distinct(sub_type) %>%
  arrange(sub_type) %>%
  pull(sub_type)

entity_types <- c("Person", "Organization", "Vessel", "Group")

# Visualization setup
palette <- c(
  Person = "orange", Organization = "red", Vessel = "forestgreen",
  Group = "orchid", Relationship = "skyblue"
)

shapes <- c(
  Person = "dot", Organization = "diamond", Vessel = "triangle",
  Group = "star", Relationship = "box"
)

edges_df <- mc3_edges_final %>%
  select(from, to) %>%                 
  mutate(across(everything(), as.character))   

vertices_df <- mc3_nodes_final %>%
  transmute(
    name      = as.character(new_index),  
    label     = label,                    
    sub_type  = sub_type
  )

full_graph <- graph_from_data_frame(
  d         = edges_df,
  directed  = FALSE,
  vertices  = vertices_df
)

# centrality measures
centralities <- tibble(
  name        = V(full_graph)$name,                 # unique ids
  degree      = degree(full_graph),
  betweenness = betweenness(full_graph, normalized = TRUE),
  closeness   = closeness(full_graph,  normalized = TRUE),
  eigen       = evcent(full_graph)$vector
)

# final summary table 
summary_tbl <- centralities %>%
  left_join(vertices_df, by = "name") %>%
  transmute(
    Name                     = label,
    `Sub-type`               = sub_type,
    `Degree centrality`      = round(degree,      3),
    `Betweenness centrality` = round(betweenness, 3),
    `Closeness centrality`   = round(closeness,   3),
    `Eigenvector centrality` = round(eigen,       3)
  ) %>%
  arrange(desc(`Degree centrality`))

# tidy summary table 
summary_tbl <- centralities %>%
  left_join(
    mc3_nodes_final %>% select(name = label, sub_type),
    by = "name"
  ) %>%
  transmute(
    Name                       = name,
    `Sub-type`                 = sub_type,
    `Degree centrality`        = round(degree,      3),
    `Betweenness centrality`   = round(betweenness, 3),
    `Closeness centrality`     = round(closeness,   3),
    `Eigenvector centrality`   = round(eigen,       3)
  ) %>%
  arrange(desc(`Degree centrality`))

## ── global max communication weight ─────────────────────────────
valid_entity_types <- c("Person", "Organization", "Vessel", "Group")
node_lkp <- mc3_nodes_final %>% select(new_index, sub_type)

max_comm_weight <- mc3_edges_final %>%
  # keep Comm ↔ entity edges only
  left_join(node_lkp, by = c("from" = "new_index")) %>%
  rename(from_type = sub_type) %>%
  left_join(node_lkp, by = c("to"   = "new_index")) %>%
  rename(to_type   = sub_type) %>%
  filter(
    (from_type %in% valid_entity_types & to_type == "Communication") |
      (to_type %in% valid_entity_types & from_type == "Communication")
  ) %>%
  transmute(
    entity = if_else(from_type %in% valid_entity_types, from, to),
    comm   = if_else(from_type %in% valid_entity_types, to, from)
  ) %>%
  inner_join(., ., by = "comm", suffix = c("_a", "_b"),
             relationship = "many-to-many") %>%
  filter(entity_a != entity_b) %>%
  mutate(pair_id = map2_chr(entity_a, entity_b,
                            ~ paste(sort(c(.x, .y)), collapse = "||"))) %>%
  count(pair_id, name = "weight") %>%
  summarise(max_w = max(weight)) %>%
  pull(max_w)


# UI
ui <- navbarPage(
  title = "Network Explorer",
  
  # ── Page 1 ─────────────────────────────────────────────────────────────
  tabPanel(
    "Relationship Network",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(
          "rel_sel", "Select relationship sub-types:",
          choices  = all_rel_subtypes,
          selected = all_rel_subtypes
        ),
        checkboxGroupInput(
          "ent_sel", "Select entity sub-types:",
          choices  = entity_types,
          selected = entity_types
        ),
        uiOutput("dense_warn"),
        width = 3
      ),
      mainPanel(
        visNetworkOutput("graph", height = "600px"),
        tabsetPanel(
          tabPanel("Nodes", DTOutput("nodes_tbl")),
          tabPanel("Edges", DTOutput("edges_tbl"))
        )
      )
    )
  ),
  
  # ── Page 2 ─────────────────────────────────────────────────────────────
  tabPanel(
    "Clustering Analysis",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(
          "cc_ent_sel", "Select entity sub-types:",
          choices  = entity_types,
          selected = entity_types
        ),
        selectInput(
          "cluster_method", "Clustering method",
          choices  = c("Label propagation" = "label_prop",
                       "Louvain"           = "louvain"),
          selected = "label_prop"
        ),
        sliderInput("min_comm_weight", "Select Minimum Communication Frequency:",
                    min = 1, max = 10, value = 3),
        width = 3
      ),
      mainPanel(
        visNetworkOutput("comm_network", height = "500px"),
        h4("📋 Cluster Members Table"),
        dataTableOutput("cluster_table")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # ───────────────────────── Relationship tab ──────────────────────────
  build_vis <- function(keep_rels, keep_entities) {
    allowed_ids <- mc3_nodes_final %>%
      filter(sub_type %in% keep_entities) %>% pull(new_index) %>% as.character()
    
    trip_filt <- triplets %>%
      filter(rel_subtype %in% keep_rels,
             e1 %in% allowed_ids, e2 %in% allowed_ids)
    if (nrow(trip_filt) == 0) return(NULL)
    
    trip_directed   <- filter(trip_filt, !rel_subtype %in% undirected_types)
    trip_undirected <- filter(trip_filt,  rel_subtype %in% undirected_types)
    
    edges_directed <- trip_directed %>%
      pivot_longer(c(e1, e2), names_to = "end", values_to = "entity") %>%
      transmute(from = if_else(end == "e1", entity, rel_id),
                to   = if_else(end == "e1", rel_id, entity),
                direction = "directed")
    
    edges_undirected <- trip_undirected %>%
      pivot_longer(c(e1, e2), names_to = "end", values_to = "entity") %>%
      transmute(from = if_else(end == "e1", entity, rel_id),
                to   = if_else(end == "e1", rel_id, entity),
                direction = "undirected")
    
    edges_all <- bind_rows(edges_directed, edges_undirected) %>% distinct()
    used_ids  <- unique(c(edges_all$from, edges_all$to))
    
    nodes_tbl <- mc3_nodes_final %>%
      filter(new_index %in% used_ids) %>%
      mutate(
        vis_type = case_when(
          type == "Relationship"     ~ "Relationship",
          sub_type %in% entity_types ~ sub_type,
          TRUE                       ~ "Other"),
        new_index = as.character(new_index)) %>%
      filter(vis_type %in% keep_entities | vis_type == "Relationship") %>%
      select(node_id = new_index, label, vis_type) %>%
      mutate(row_id = row_number())
    
    id_map <- select(nodes_tbl, node_id, row_id)
    
    vis_edges <- edges_all %>%
      left_join(id_map, by = c("from" = "node_id")) %>% rename(.from = row_id) %>%
      left_join(id_map, by = c("to"   = "node_id")) %>% rename(.to   = row_id) %>%
      filter(!is.na(.from), !is.na(.to)) %>%
      transmute(from = .from, to = .to,
                arrows = if_else(direction == "undirected", "", "to"),
                color = "grey")
    
    vis_nodes <- nodes_tbl %>%
      left_join(rel_nodes, by = c("node_id" = "rel_id")) %>%
      mutate(
        group = vis_type,
        shape = shapes[group], color = palette[group],
        title = if_else(group == "Relationship", rel_subtype, NA_character_),
        shape = if_else(is.na(shape), "ellipse", shape),
        color = if_else(is.na(color), "gray",    color)) %>%
      transmute(
        id    = row_id,
        label = if_else(group == "Relationship", rel_subtype, label),
        group = group, shape = shape, color = color, title = title)
    
    visNetwork(vis_nodes, vis_edges, width = "100%", height = "800px") %>%
      visNodes(font = list(size = 18)) %>%
      visEdges(smooth = FALSE) %>%
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 2, hover = TRUE),
        nodesIdSelection = TRUE) %>%
      visLegend(
        addNodes = data.frame(label  = names(palette),
                              shape  = shapes[names(palette)],
                              color  = palette[names(palette)]),
        useGroups = FALSE, position = "right") %>%
      visLayout(randomSeed = 42)
  }
  
  graph_info <- reactive({
    req(input$rel_sel, input$ent_sel)
    
    keep_nodes <- mc3_nodes_final %>%
      filter(sub_type %in% input$ent_sel) %>% pull(new_index) %>% as.character()
    
    trip_filt <- triplets %>%
      filter(rel_subtype %in% input$rel_sel,
             e1 %in% keep_nodes, e2 %in% keep_nodes)
    
    if (nrow(trip_filt) == 0)
      return(list(nodes_disp = tibble(Name = character()),
                  edges_disp = tibble(From = character(),
                                      To = character(),
                                      Relationship = character())))
    
    node_ids <- unique(c(trip_filt$e1, trip_filt$e2))
    
    nodes_disp <- mc3_nodes_final %>%
      filter(new_index %in% node_ids) %>%
      transmute(ID = as.character(new_index), Name = label)
    
    label_lookup <- mc3_nodes_final %>%
      transmute(new_index = as.character(new_index), label)
    
    edges_disp <- trip_filt %>%
      select(e1, e2, rel_subtype) %>%
      mutate(across(c(e1, e2), as.character)) %>%
      left_join(label_lookup, by = c("e1" = "new_index")) %>% rename(From = label) %>%
      left_join(label_lookup, by = c("e2" = "new_index")) %>% rename(To   = label) %>%
      transmute(From, To, Relationship = rel_subtype)
    
    list(nodes_disp = nodes_disp, edges_disp = edges_disp)
  })
  
  visible_summary <- reactive({
    vis_ids <- graph_info()$nodes_disp$ID
    summary_tbl %>%
      left_join(vertices_df %>% select(name, label, sub_type),
                by = c("Name" = "name")) %>%
      transmute(
        ID = Name, Name = label, `Sub-type` = sub_type,
        `Degree centrality`, `Betweenness centrality`,
        `Closeness centrality`, `Eigenvector centrality`) %>%
      filter(ID %in% vis_ids) %>% select(-ID)
  })
  
  output$graph <- renderVisNetwork({
    build_vis(input$rel_sel, input$ent_sel)
  })
  
  output$nodes_tbl <- renderDT({
    datatable(visible_summary(),
              options = list(pageLength = 5, dom = "tip"),
              class   = "compact stripe hover row-border order-column")
  })
  output$edges_tbl <- renderDT({
    datatable(graph_info()$edges_disp,
              options = list(pageLength = 5, dom = "tip"),
              class   = "compact stripe hover row-border order-column")
  })
  
  output$dense_warn <- renderUI({
    if (length(input$rel_sel) == length(all_rel_subtypes))
      tags$div(style = "color:#b00;font-weight:bold;font-size:0.9em;",
               "⚠️  Including all sub-types will produce a very dense graph.")
  })
  
  # Clustering
  output$comm_network <- renderVisNetwork({
    req(mc3_nodes_final, mc3_edges_final, input$cluster_method, input$cc_ent_sel, input$min_comm_weight)
    
    valid_entity_types <- c("Person", "Organization", "Vessel", "Group")
    
    node_lookup <- mc3_nodes_final %>% select(new_index, sub_type, name)
    
    edges_labeled <- mc3_edges_final %>%
      left_join(node_lookup, by = c("from" = "new_index")) %>%
      rename(from_type = sub_type, from_name = name) %>%
      left_join(node_lookup, by = c("to" = "new_index")) %>%
      rename(to_type = sub_type, to_name = name)
    
    # Filter entity-communication edges
    entity_comm <- edges_labeled %>%
      filter(
        (from_type %in% valid_entity_types & to_type == "Communication") |
          (to_type %in% valid_entity_types & from_type == "Communication")
      ) %>%
      transmute(
        entity = if_else(from_type %in% valid_entity_types, from_name, to_name),
        comm   = if_else(from_type %in% valid_entity_types, to, from),
        type   = if_else(from_type %in% valid_entity_types, from_type, to_type)
      ) %>%
      filter(type %in% input$cc_ent_sel)
    
    if (nrow(entity_comm) < 2) return(NULL)
    
    # Pairwise co-occurrence edges with communication frequency
    edges_raw <- entity_comm %>%
      inner_join(entity_comm, by = "comm", suffix = c("_a", "_b")) %>%
      filter(entity_a != entity_b) %>%
      mutate(pair_id = map2_chr(entity_a, entity_b, ~ paste(sort(c(.x, .y)), collapse = "||"))) %>%
      count(pair_id, entity_a, entity_b, name = "weight")
    
    # Filter weak edges by communication frequency
    edges_raw <- edges_raw %>% filter(weight >= input$min_comm_weight)
    
    if (nrow(edges_raw) == 0) return(NULL)
    
    # Create node and edge tables
    nodes_tbl <- entity_comm %>%
      distinct(name = entity, sub_type = type) %>%
      mutate(row_id = row_number())
    
    id_lkp <- nodes_tbl %>% select(name, row_id)
    edges_final <- edges_raw %>%
      left_join(id_lkp, by = c("entity_a" = "name")) %>% rename(from = row_id) %>%
      left_join(id_lkp, by = c("entity_b" = "name")) %>% rename(to = row_id) %>%
      select(from, to, weight)
    
    # Remove nodes not in use after filtering
    used_ids <- unique(c(edges_final$from, edges_final$to))
    used_names <- id_lkp %>%
      filter(row_id %in% used_ids) %>%
      pull(name)
    
    nodes_tbl <- nodes_tbl %>%
      filter(name %in% used_names) %>%
      mutate(row_id = row_number())  # reset row_id
    
    # Re-map edges again based on new row_ids
    id_lkp <- nodes_tbl %>% select(name, row_id)
    edges_final <- edges_raw %>%
      filter(entity_a %in% nodes_tbl$name & entity_b %in% nodes_tbl$name) %>%
      left_join(id_lkp, by = c("entity_a" = "name")) %>% rename(from = row_id) %>%
      left_join(id_lkp, by = c("entity_b" = "name")) %>% rename(to = row_id) %>%
      select(from, to, weight)
    
    if (nrow(edges_final) == 0) return(NULL)
    
    full_graph <- tbl_graph(nodes = nodes_tbl, edges = edges_final, directed = FALSE) %>%
      mutate(
        degree      = centrality_degree(),
        betweenness = centrality_betweenness(),
        closeness   = centrality_closeness(),
        eigen       = centrality_eigen()
      )
    
    vis_nodes <- full_graph %>%
      as_tibble() %>%
      mutate(
        id = as.character(row_number()),
        label = name,
        value = scales::rescale(degree, to = c(5, 25)),
        shape = case_when(
          sub_type == "Person"       ~ "dot",
          sub_type == "Vessel"       ~ "triangle",
          sub_type == "Organization" ~ "box",
          sub_type == "Group"        ~ "star",
          TRUE ~ "ellipse"
        ),
        title = paste0(
          "<b>", name, "</b><br>",
          "Degree: ", degree, "<br>",
          "Betweenness: ", round(betweenness, 2), "<br>",
          "Closeness: ", round(closeness, 3), "<br>",
          "Eigenvector: ", round(eigen, 3)
        )
      ) %>%
      select(id, label, shape, value, title)
    
    vis_edges <- edges_final %>%
      mutate(
        from = as.character(from),
        to = as.character(to),
        arrows = "none",
        color = "gray",
        width = scales::rescale(weight, to = c(1, 5))  # edge width reflects frequency
      ) %>%
      distinct()
    
    # Clustering based on user choice
    g_igraph <- igraph::graph_from_data_frame(
      d = vis_edges %>% select(from, to),
      vertices = vis_nodes %>% select(id, label),
      directed = FALSE
    )
    
    clustering_method <- input$cluster_method
    clusters <- switch(
      clustering_method,
      louvain    = cluster_louvain(g_igraph),
      label_prop = cluster_label_prop(g_igraph),
      stop("Unsupported clustering method")
    )
    
    membership_df <- data.frame(
      id = names(membership(clusters)),
      cluster_id = membership(clusters),
      stringsAsFactors = FALSE
    )
    
    n_clusters <- length(unique(membership_df$cluster_id))
    palette <- RColorBrewer::brewer.pal(max(3, min(8, n_clusters)), "Set2")
    
    vis_nodes_clustered <- vis_nodes %>%
      left_join(membership_df, by = "id") %>%
      mutate(
        group = paste0("Cluster ", cluster_id),
        color = palette[as.numeric(factor(cluster_id))]
      )
    
    output$cluster_table <- renderDataTable({
      req(vis_nodes_clustered)
      
      cluster_table <- vis_nodes_clustered %>%
        mutate(cluster_id = as.numeric(gsub("Cluster ", "", group))) %>%
        arrange(cluster_id, label) %>%
        group_by(cluster_id) %>%
        summarise(`Entity List` = paste(label, collapse = ", "), .groups = "drop") %>%
        select(Cluster = cluster_id, `Entity List`)
      
      datatable(cluster_table, options = list(
        pageLength = 5,
        autoWidth = TRUE
      ))
    })
    
    legend_data <- vis_nodes_clustered %>%
      distinct(group, color) %>%
      mutate(label = group, shape = "square") %>%
      select(label, shape, color)
    
    entity_shape_legend <- tibble::tibble(
      label = c("Person", "Vessel", "Organization", "Group"),
      shape = c("dot", "triangle", "box", "star"),
      color = rep("gray", 4)
    )
    
    visNetwork(vis_nodes_clustered, vis_edges) %>%
      visNodes(font = list(size = 16)) %>%
      visEdges(smooth = FALSE) %>%
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
        nodesIdSelection = TRUE
      ) %>%
      visLegend(
        addNodes = rbind(legend_data, entity_shape_legend),
        useGroups = FALSE,
        position = "right"
      ) %>%
      visLayout(randomSeed = 42, improvedLayout = TRUE) %>%
      visIgraphLayout(layout = "layout_with_fr")
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

