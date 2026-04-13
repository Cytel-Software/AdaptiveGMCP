# --------------------------------------------------------------------------------------------------
#
# ©2025 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------


#' Interactive plot for the graph
#' @param HypothesisName Hypothesis Name
#' @param w  Initial weights
#' @param G Initial Transition Matrix
#' @param activeStatus Logical; False: greyed out the unvalid hypothes
#' @param Title Title of the plot
#' @param Text Additional information to be added in the plot
#' @export
plotGraph <- function(HypothesisName, w, G, activeStatus, Title, Text) {
  # Libraries
  library(visNetwork)
  library(reshape2)
  #------------ -

  if (is.null(w) & is.null(G)) {
    nodes <- data.frame(
      id = 1:length(HypothesisName),
      label = HypothesisName,
      shape = "circle",
      color = "grey",
      smooth = FALSE,
      shadow = T,
      physics = FALSE,
      row.names = NULL
    )
    visN <- visNetwork(nodes, main = Title) %>%
      visNodes(font = list(size = 16))
  } else {
    n <- length(w)
    rownames(G) <- colnames(G) <- NULL

    nodesLabel <- paste(HypothesisName, "(",
      round(w, 2), ")",
      sep = ""
    )
    nodesWeight <- w

    if (missing(activeStatus)) {
      nodeColor <- rep("skyblue", n)
    } else {
      nodeColor <- rep("skyblue", n)
      nodeColor[!activeStatus] <- "grey"
    }
    nodes <- data.frame(
      id = 1:n,
      label = nodesLabel,
      # group = c("GrA", "GrB"),
      shape = "circle",
      color = nodeColor,
      smooth = FALSE,
      shadow = T,
      physics = FALSE,
      row.names = NULL
    )
    if (all(G == 0)) {
      edges <- NULL
    } else {
      edges_df <- melt(G,
        varnames = c("from", "to"),
        value.name = "weight"
      )
      edges_df <- edges_df[edges_df$weight != 0, ]

      edges <- data.frame(
        from = edges_df$from,
        to = edges_df$to,
        label = as.character(round(edges_df$weight, 3)),
        color = "black",
        background = "white",
        shape = "box",
        length = 100,
        arrows = "to",
        dashes = FALSE,
        smooth = TRUE,
        shadow = TRUE,
        physics = TRUE,
        row.names = NULL
      )
    }


    Title <- ifelse(missing(Title), "Graph", Title)

    if (missing(Text)) {
      if (length(edges) == 0) {
        visN <- visNetwork(nodes, main = Title) %>%
          visNodes(font = list(size = 16))
      } else {
        visN <- visNetwork(nodes, edges, main = Title) %>%
          visNodes(font = list(size = 16)) %>%
          visEdges(font = list(size = 20))
      }
    } else {
      if (length(edges) == 0) {
        visN <- visNetwork(nodes,
          main = Title,
          submain = list(
            text = Text,
            style = "font-family:Times New Roman;font-size:14px;text-align:center;"
          )
        ) %>%
          visNodes(font = list(size = 16))
      } else {
        visN <- visNetwork(nodes, edges,
          main = Title,
          submain = list(
            text = Text,
            style = "font-family:Times New Roman;font-size:14px;text-align:center;"
          )
        ) %>%
          visNodes(font = list(size = 16)) %>%
          visEdges(font = list(size = 20))
      }
    }
    # End of else check for null values in nodes and edeges
  }
  htmltools::tagList(print(visN))
}


# Plot text for MAMSMEP designs
getPlotText <- function(HypoMap) {
  texts <- lapply(1:nrow(HypoMap), function(i) {
    Hypo <- HypoMap$Hypothesis[i]

    EPArms <- paste(paste("EP", HypoMap$Groups[i], sep = ""),
      "Ctr",
      paste("trt", (HypoMap$Treatment[i] - 1), sep = ""),
      sep = ","
    )
    paste("(", Hypo, ":", EPArms, ")", sep = "")
  })
  paste(texts, collapse = ",")
}
