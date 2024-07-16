#' Network plot of the model
#'
#' @author Iwo Augustynski
#'
#' @param model SFC model object created with \code{\link{create_model}}
#' @param save_file name and path to save the plot as html file
#'
#' @details This function creates a representation of a model as a directed graph. 
#' Additionally it shows cycles in the model.
#' Graph can be saved as html file.
#' 
#' @return visNetwork object
#' @export
#'
#' @examples
#' model <- godley::create_model(name = "SFC model", template = "BMW")
#' plot_cycles(model)
#' 
plot_cycles <- function(model, save_file = NULL) {
  # argument check
  # type
  checkmate::assert_class(model, "SFC")
  checkmate::assert_logical(info)
  checkmate::assert_character(save_file, len = 1, null.ok = TRUE)
  
  # Check correctness of equations entered by the user
  res <- godley:::validate_model_input(model, info = FALSE)
  equations_sep <- res[[1]]
  variables_exo <- res[[2]]
  functions <- res[[3]]
  
  # Prepare them for the simulation process
  km <- godley:::find_adjacency(equations_sep)
  
  # Find cycles in model
  blocks <- unique(sort(calls$block))
  equations_id <- lapply(blocks, function(x) calls[, "id"][calls[, "block"] == x])
  cycles <- equations_id[lapply(equations_id, length) > 1]
  
  for (i in seq_along(cycles)) {
    #cycles[[i]] <- filter(calls, id %in% cycles[[i]]) |> pull(lhs)
    cycles[[i]] <- calls[calls$id %in% cycles[[i]],]$lhs
  }
  
  # Teraz można pokolorować wierzchołki w zależności od tego, w której pętli są
  
  # Prepare igraph
  graph <- igraph::graph_from_adjacency_matrix(km, mode = "directed")
  
  # plotting
  # plot(graph, layout = igraph::layout_with_fr(graph))
  # tkplot(graph)
  
  # plotting in visNetwork
  visgraph <- visNetwork::toVisNetworkData(graph)
  visgraph$edges$arrows = "from"
  visgraph$nodes$title = visgraph$nodes$id
  visgraph$nodes <- visgraph$nodes[c("id", "title")]
  visgraph$nodes$group <- "X"
  for (i in seq_along(cycles)) {
    visgraph$nodes$group[visgraph$nodes$id %in% cycles[[i]]] <- LETTERS[i]
  }
  
  network <- visNetwork::visNetwork(visgraph$nodes, visgraph$edges, width = "100%", height = "600px") |>
    
    visNetwork::visOptions(highlightNearest = list(enabled = T, hover = T), 
                           nodesIdSelection = T,
                           selectedBy = list(variable = "group"))
  
  if (!is.null(save_file)) {
    if (!grepl(".html$", save_file)) {
      save_file <- paste0(save_file, ".html")
    }
    visNetwork::visSave(network, file = save_file)
  }
  return(network)
}
