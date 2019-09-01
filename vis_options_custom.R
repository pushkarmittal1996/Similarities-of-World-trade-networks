library(visNetwork)
visOptions_custom <- function (graph, width = NULL, height = NULL, highlightNearest = FALSE, 
                               nodesIdSelection = FALSE, selectedBy = NULL, autoResize = NULL, 
                               clickToUse = NULL, manipulation = NULL) 
{
  if (!any(class(graph) %in% c("visNetwork", "visNetwork_Proxy"))) {
    stop("graph must be a visNetwork or a visNetworkProxy object")
  }
  options <- list()
  options$autoResize <- autoResize
  options$clickToUse <- clickToUse
  if (is.null(manipulation)) {
    options$manipulation <- list(enabled = FALSE)
  }
  else {
    options$manipulation <- list(enabled = manipulation)
  }
  options$height <- height
  options$width <- width
  if (!is.null(manipulation)) {
    if (manipulation) {
      graph$x$datacss <- paste(readLines(system.file("htmlwidgets/lib/css/dataManipulation.css", 
                                                     package = "visNetwork"), warn = FALSE), collapse = "\n")
    }
  }
  if (!"nodes" %in% names(graph$x) && any(class(graph) %in% 
                                          "visNetwork")) {
    highlight <- list(enabled = FALSE)
    idselection <- list(enabled = FALSE)
    byselection <- list(enabled = FALSE)
  }
  else {
    highlight <- list(enabled = FALSE, hoverNearest = FALSE, 
                      degree = 1, algorithm = "all")
    if (is.list(highlightNearest)) {
      if (any(!names(highlightNearest) %in% c("enabled", 
                                              "degree", "hover", "algorithm"))) {
        stop("Invalid 'highlightNearest' argument")
      }
      if ("algorithm" %in% names(highlightNearest)) {
        stopifnot(highlightNearest$algorithm %in% c("all", 
                                                    "hierarchical"))
        highlight$algorithm <- highlightNearest$algorithm
      }
      if ("degree" %in% names(highlightNearest)) {
        highlight$degree <- highlightNearest$degree
      }
      if (highlight$algorithm %in% "hierarchical") {
        if (is.list(highlight$degree)) {
          stopifnot(all(names(highlight$degree) %in% 
                          c("from", "to")))
        }
        else {
          highlight$degree <- list(from = highlight$degree, 
                                   to = highlight$degree)
        }
      }
      if ("hover" %in% names(highlightNearest)) {
        stopifnot(is.logical(highlightNearest$hover))
        highlight$hoverNearest <- highlightNearest$hover
      }
      if ("enabled" %in% names(highlightNearest)) {
        stopifnot(is.logical(highlightNearest$enabled))
        highlight$enabled <- highlightNearest$enabled
      }
    }
    else {
      stopifnot(is.logical(highlightNearest))
      highlight$enabled <- highlightNearest
    }
    if (highlight$enabled && any(class(graph) %in% "visNetwork")) {
      if (!"label" %in% colnames(graph$x$nodes)) {
        #graph$x$nodes$label <- as.character(graph$x$nodes$id)
      }
      if (!"group" %in% colnames(graph$x$nodes)) {
        graph$x$nodes$group <- 1
      }
    }
    idselection <- list(enabled = FALSE, style = "width: 150px; height: 26px")
    if (is.list(nodesIdSelection)) {
      if (any(!names(nodesIdSelection) %in% c("enabled", 
                                              "selected", "style", "values"))) {
        stop("Invalid 'nodesIdSelection' argument. List can have 'enabled', 'selected', 'style', 'values'")
      }
      if ("selected" %in% names(nodesIdSelection)) {
        if (any(class(graph) %in% "visNetwork")) {
          if (!nodesIdSelection$selected %in% graph$x$nodes$id) {
            stop(nodesIdSelection$selected, " not in data. nodesIdSelection$selected must be valid.")
          }
        }
        idselection$selected <- nodesIdSelection$selected
      }
      if ("enabled" %in% names(nodesIdSelection)) {
        idselection$enabled <- nodesIdSelection$enabled
      }
      else {
        idselection$enabled <- TRUE
      }
      if ("style" %in% names(nodesIdSelection)) {
        idselection$style <- nodesIdSelection$style
      }
    }
    else if (is.logical(nodesIdSelection)) {
      idselection$enabled <- nodesIdSelection
    }
    else {
      stop("Invalid 'nodesIdSelection' argument")
    }
    if (idselection$enabled) {
      if ("values" %in% names(nodesIdSelection)) {
        idselection$values <- nodesIdSelection$values
        if (length(idselection$values) == 1) {
          idselection$values <- list(idselection$values)
        }
        if ("selected" %in% names(nodesIdSelection)) {
          if (!idselection$selected %in% idselection$values) {
            stop(idselection$selected, " not in data/selection. nodesIdSelection$selected must be a valid value.")
          }
        }
      }
    }
    byselection <- list(enabled = FALSE, style = "width: 150px; height: 26px", 
                        multiple = FALSE)
    if (!is.null(selectedBy)) {
      if (is.list(selectedBy)) {
        if (any(!names(selectedBy) %in% c("variable", 
                                          "selected", "style", "values", "multiple"))) {
          stop("Invalid 'selectedBy' argument. List can have 'variable', 'selected', 'style', 'values', 'multiple'")
        }
        if ("selected" %in% names(selectedBy)) {
          byselection$selected <- as.character(selectedBy$selected)
        }
        if (!"variable" %in% names(selectedBy)) {
          stop("'selectedBy' need at least 'variable' information")
        }
        byselection$variable <- selectedBy$variable
        if ("style" %in% names(selectedBy)) {
          byselection$style <- selectedBy$style
        }
        if ("multiple" %in% names(selectedBy)) {
          byselection$multiple <- selectedBy$multiple
        }
      }
      else if (is.character(selectedBy)) {
        byselection$variable <- selectedBy
      }
      else {
        stop("Invalid 'selectedBy' argument. Must a 'character' or a 'list'")
      }
      if (any(class(graph) %in% "visNetwork_Proxy")) {
        byselection$enabled <- TRUE
        if ("values" %in% names(selectedBy)) {
          byselection$values <- selectedBy$values
        }
        if ("selected" %in% names(byselection)) {
          byselection$selected <- byselection$selected
        }
      }
      else {
        if (!byselection$variable %in% colnames(graph$x$nodes)) {
          warning("Can't find '", byselection$variable, 
                  "' in node data.frame")
        }
        else {
          byselection$enabled <- TRUE
          byselection$values <- unique(graph$x$nodes[, 
                                                     byselection$variable])
          if (byselection$multiple) {
            byselection$values <- unique(gsub("^[[:space:]]*|[[:space:]]*$", 
                                              "", do.call("c", strsplit(as.character(byselection$values), 
                                                                        split = ","))))
          }
          if (any(c("integer", "numeric") %in% class(graph$x$nodes[, 
                                                                   byselection$variable]))) {
            byselection$values <- sort(byselection$values)
          }
          else {
            byselection$values <- sort(as.character(byselection$values))
          }
          if ("values" %in% names(selectedBy)) {
            byselection$values <- selectedBy$values
          }
          if ("selected" %in% names(byselection)) {
            if (!byselection$selected %in% byselection$values) {
              stop(byselection$selected, " not in data/selection. selectedBy$selected must be a valid value.")
            }
            byselection$selected <- byselection$selected
          }
          if (!"label" %in% colnames(graph$x$nodes)) {
            graph$x$nodes$label <- ""
          }
          if (!"group" %in% colnames(graph$x$nodes)) {
            graph$x$nodes$group <- 1
          }
        }
      }
    }
  }
  x <- list(highlight = highlight, idselection = idselection, 
            byselection = byselection)
  if (highlight$hoverNearest) {
    graph <- visInteraction(graph, hover = TRUE)
  }
  if (any(class(graph) %in% "visNetwork_Proxy")) {
    data <- list(id = graph$id, options = options)
    graph$session$sendCustomMessage("visShinyOptions", data)
    if (missing(highlightNearest)) {
      x$highlight <- NULL
    }
    if (missing(nodesIdSelection)) {
      x$idselection <- NULL
    }
    if (missing(selectedBy)) {
      x$byselection <- NULL
    }
    data <- list(id = graph$id, options = x)
    graph$session$sendCustomMessage("visShinyCustomOptions", 
                                    data)
  }
  else {
    graph$x <- visNetwork:::mergeLists(graph$x, x)
    graph$x$options <- visNetwork:::mergeLists(graph$x$options, options)
  }
  graph
}