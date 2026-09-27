#' Get the similarity matrix
#'
#' @param y A data.frame of enrichment result
#' @param geneSets A list, the names of geneSets are term ids,
#' and every element is a vector of genes.
#' @param method Method of calculating the similarity between nodes,
#' one of "Resnik", "Lin", "Rel", "Jiang" , "Wang"  and
#' "JC" (Jaccard similarity coefficient) methods
#' @param semData GOSemSimDATA object
#' @noRd
get_similarity_matrix <- function(y, geneSets, method, semData = NULL) {
    id <- y[, "ID"]
    geneSets <- geneSets[id]
    y_id <- unlist(strsplit(y$ID[1], ":"))[1]
    ## Choose the method to calculate the similarity
    if (method == "JC") {
        w <- .cal_jc_similarity(geneSets, id = id, name = y$Description)
        rownames(y) <- y$ID
        rownames(w) <- colnames(w) <- unname(get_term_labels(y, id))
        return(w)
    }

    if (y_id == "GO") {
        if (is.null(semData)) {
            stop(
                "The semData parameter is missing,
                and it can be obtained through godata function in GOSemSim package."
            )
        }
        w <- GOSemSim::mgoSim(
            id,
            id,
            semData = semData,
            measure = method,
            combine = NULL
        )
    }

    if (y_id == "DOID") {
        w <- DOSE::doSim(id, id, measure = method)
    }
    rownames(y) <- y$ID
    rownames(w) <- colnames(w) <- unname(get_term_labels(y, colnames(w)))
    return(w)
}


#' Check whether the similarity matrix exists
#'
#' @param x result of enrichment analysis
#'
#' @noRd
has_pairsim <- function(x) {
    if (length(x@termsim) == 0 || is.null(dim(x@termsim))) {
        yulab.utils::yulab_abort(paste0(
            "Term similarity matrix not available, so the terms cannot be laid out ",
            "as a network or a tree. Call `pairwise_termsim()` on the result first:\n",
            "  x <- pairwise_termsim(x)\n",
            "`emapplot()`, `treeplot()` and `ssplot()` all require it."
        ))
    }
    invisible(TRUE)
}


#' Validate and select enrichment-map edges
#'
#' The threshold mode deliberately retains the historical matrix-cell
#' behavior; the other modes use deterministic term-label tie breaking.
#'
#' @param edge_data data.frame with the first two columns identifying terms and
#'   the third column containing numeric similarities.
#' @param edge_filter one of `"threshold"`, `"top_k"`, or `"adaptive"`.
#' @param min_edge minimum similarity for `"threshold"` mode.
#' @param top_k number of strongest neighbors per term for `"top_k"` mode.
#' @param target_density target proportion of unique term pairs for
#'   `"adaptive"` mode.
#' @return A logical vector indicating retained rows.
#' @noRd
filter_emap_edges <- function(
    edge_data,
    edge_filter = "threshold",
    min_edge = .2,
    top_k = 5,
    target_density = .1
) {
    if (!is.data.frame(edge_data) || ncol(edge_data) < 3) {
        stop("`edge_data` must be a data.frame with at least three columns.")
    }
    edge_filter <- match.arg(edge_filter, c("threshold", "top_k", "adaptive"))
    if (length(min_edge) != 1L || !is.numeric(min_edge) ||
        !is.finite(min_edge) || min_edge < 0 || min_edge > 1) {
        stop('"min_edge" should be a finite number between 0 and 1.')
    }
    if (length(top_k) != 1L || !is.numeric(top_k) ||
        !is.finite(top_k) || top_k < 1 || top_k != floor(top_k)) {
        stop('"top_k" should be a positive integer.')
    }
    if (length(target_density) != 1L || !is.numeric(target_density) ||
        !is.finite(target_density) || target_density < 0 ||
        target_density > 1) {
        stop('"target_density" should be a finite number between 0 and 1.')
    }

    similarity <- as.numeric(edge_data[[3L]])
    from <- as.character(edge_data[[1L]])
    to <- as.character(edge_data[[2L]])
    keep <- rep(FALSE, nrow(edge_data))
    valid <- !is.na(similarity) & !is.na(from) & !is.na(to) & from != to
    if (!any(valid)) {
        return(keep)
    }

    if (edge_filter == "threshold") {
        keep <- valid & similarity >= min_edge
        return(keep)
    }

    ## Work with unique, unordered pairs for the two density-oriented modes.
    valid_idx <- which(valid)
    left <- pmin(from[valid_idx], to[valid_idx])
    right <- pmax(from[valid_idx], to[valid_idx])
    pair_key <- paste(left, right, sep = "\\r")
    pair_rows <- split(valid_idx, pair_key)
    pair_names <- names(pair_rows)
    pair_left <- vapply(pair_rows, function(ii) {
        min(from[ii[1L]], to[ii[1L]])
    }, character(1L))
    pair_right <- vapply(pair_rows, function(ii) {
        max(from[ii[1L]], to[ii[1L]])
    }, character(1L))
    pair_idx <- vapply(pair_rows, function(ii) {
        ii[order(-similarity[ii], from[ii], to[ii], method = "radix")[1L]]
    }, integer(1L))
    pair_similarity <- similarity[pair_idx]
    eligible_pairs <- pair_names[pair_similarity >= min_edge]

    if (edge_filter == "top_k") {
        if (length(eligible_pairs) == 0L) {
            return(keep)
        }
        eligible_idx <- match(eligible_pairs, pair_names)
        terms <- sort(unique(c(
            pair_left[eligible_idx], pair_right[eligible_idx]
        )))
        selected_pairs <- character()
        for (term in terms) {
            incident <- eligible_idx[
                pair_left[eligible_idx] == term |
                    pair_right[eligible_idx] == term
            ]
            ord <- order(
                -pair_similarity[incident],
                pair_names[incident],
                method = "radix"
            )
            selected_pairs <- c(
                selected_pairs,
                pair_names[incident[ord[seq_len(min(length(ord), top_k))]]]
            )
        }
        keep[valid_idx] <- pair_key %in% unique(selected_pairs)
        return(keep)
    }

    ## Adaptive mode retains the strongest eligible global pairs up to the
    ## requested density among all possible displayed term pairs.
    n_terms <- length(unique(c(from[valid_idx], to[valid_idx])))
    possible_pairs <- choose(n_terms, 2)
    n_keep <- if (target_density == 0 || possible_pairs == 0) {
        0L
    } else {
        min(length(eligible_pairs), ceiling(target_density * possible_pairs))
    }
    if (n_keep == 0L) {
        return(keep)
    }
    eligible_idx <- match(eligible_pairs, pair_names)
    ord <- order(
        -pair_similarity[eligible_idx],
        pair_names[eligible_idx],
        method = "radix"
    )
    selected_pairs <- eligible_pairs[ord[seq_len(n_keep)]]
    keep[valid_idx] <- pair_key %in% selected_pairs
    keep
}

#' Diagnose enrichment-map edge density
#'
#' @param pair_sim A square numeric similarity matrix.
#' @param min_edge minimum similarity threshold for `"threshold"` mode.
#' @param edge_filter edge filtering strategy.
#' @param top_k number of neighbors per term for `"top_k"` mode.
#' @param target_density target proportion of unique pairs for `"adaptive"`.
#' @param dense_threshold density above which a higher threshold is suggested.
#' @return A one-row data.frame with retained pair/edge counts, density,
#'   filter settings, and a suggestion when the graph is dense.
#' @export
emapplot_edge_density <- function(
    pair_sim,
    min_edge = .2,
    edge_filter = "threshold",
    top_k = 5,
    target_density = .1,
    dense_threshold = .2
) {
    if (!is.matrix(pair_sim) || nrow(pair_sim) != ncol(pair_sim) ||
        !is.numeric(pair_sim)) {
        stop("`pair_sim` must be a square numeric matrix.")
    }
    if (length(dense_threshold) != 1L || !is.numeric(dense_threshold) ||
        !is.finite(dense_threshold) || dense_threshold < 0 ||
        dense_threshold > 1) {
        stop('"dense_threshold" should be a finite number between 0 and 1.')
    }
    labels <- rownames(pair_sim)
    if (is.null(labels) || length(labels) != nrow(pair_sim) ||
        anyNA(labels) || anyDuplicated(labels)) {
        labels <- as.character(seq_len(nrow(pair_sim)))
    }
    rownames(pair_sim) <- colnames(pair_sim) <- labels
    wd <- as.data.frame(as.table(pair_sim), stringsAsFactors = FALSE)
    names(wd) <- c("from", "to", "similarity")
    keep <- filter_emap_edges(
        wd,
        edge_filter = edge_filter,
        min_edge = min_edge,
        top_k = top_k,
        target_density = target_density
    )
    retained_pairs <- unique(vapply(which(keep), function(i) {
        paste(sort(c(as.character(wd$from[i]), as.character(wd$to[i]))),
            collapse = "\\r")
    }, character(1L)))
    possible <- choose(nrow(pair_sim), 2)
    retained <- length(retained_pairs)
    density <- if (possible == 0) 0 else retained / possible
    suggestion <- if (density > dense_threshold) {
        "Graph density is high; consider increasing `min_edge` or using `top_k`/`adaptive`."
    } else {
        "Graph density is within the requested display range."
    }
    suggested_min_edge <- NA_real_
    if (density > dense_threshold && edge_filter == "threshold") {
        values <- wd$similarity[wd$from != wd$to & is.finite(wd$similarity)]
        higher <- sort(unique(values[values > min_edge]))
        if (length(higher) > 0) {
            suggested_min_edge <- higher[1L]
        }
    }
    data.frame(
        n_terms = nrow(pair_sim),
        possible_pairs = possible,
        retained_pairs = retained,
        edge_rows = sum(keep),
        density = density,
        edge_filter = match.arg(edge_filter, c("threshold", "top_k", "adaptive")),
        min_edge = min_edge,
        top_k = as.integer(top_k),
        target_density = target_density,
        dense_threshold = dense_threshold,
        suggested_min_edge = suggested_min_edge,
        suggestion = suggestion,
        stringsAsFactors = FALSE
    )
}

# Descriptive alias for callers that prefer a diagnostic name.
edge_density_diagnostic <- emapplot_edge_density

#' Get graph_from_data_frame() result
#'
#' @importFrom igraph graph.empty
#' @importFrom igraph graph_from_data_frame
#' @param enrichDf A data.frame of enrichment result.
#' @param geneSets A list gene sets with the names of enrichment IDs
#' @param color a string, the column name of y for nodes colours
#' @param cex_line Numeric, scale of line width
#' @param min_edge The minimum similarity threshold for whether
#' two nodes are connected; should be between 0 and 1 (default `0.2`).
#' @param pair_sim Semantic similarity matrix.
#' @param method Method of calculating the similarity between nodes,
#' one of "Resnik", "Lin", "Rel", "Jiang" , "Wang"  and
#' "JC" (Jaccard similarity coefficient) methods
#' @return result of graph_from_data_frame()
#' @importFrom igraph V
#' @importFrom igraph 'V<-'
#' @importFrom igraph E
#' @importFrom igraph 'E<-'
#' @importFrom igraph add_vertices
#' @importFrom igraph delete.edges
#' @noRd
build_emap_graph <- function(
    enrichDf,
    geneSets,
    color,
    cex_line,
    min_edge,
    pair_sim,
    edge_filter = "threshold",
    top_k = 5,
    target_density = .1
) {
    if (length(cex_line) != 1L || !is.numeric(cex_line) ||
        !is.finite(cex_line) || cex_line < 0) {
        stop('"size_edge" should be a finite non-negative number.')
    }
    ## Validate arguments even when there is only one displayed term.
    filter_emap_edges(
        data.frame(character(), character(), numeric()),
        edge_filter = edge_filter,
        min_edge = min_edge,
        top_k = top_k,
        target_density = target_density
    )
    if (is.null(dim(enrichDf)) || nrow(enrichDf) == 1) {
        # when just one node
        g <- igraph::make_empty_graph(n = 1, directed = FALSE)
        labels <- get_geneSet_labels(geneSets)
        label <- unname(labels[as.character(enrichDf$ID)])
        if (is.na(label) || !nzchar(label)) {
            label <- unname(get_term_labels(enrichDf, enrichDf$ID))
        }
        V(g)$name <- label
        V(g)$color <- "red"
        diagnostic <- data.frame(
            n_terms = 1L,
            possible_pairs = 0,
            retained_pairs = 0L,
            edge_rows = 0L,
            density = 0,
            edge_filter = edge_filter,
            min_edge = min_edge,
            top_k = as.integer(top_k),
            target_density = target_density,
            dense_threshold = 0.2,
            suggested_min_edge = NA_real_,
            suggestion = "Fewer than two terms are available for edge diagnostics.",
            stringsAsFactors = FALSE
        )
        g <- igraph::set_graph_attr(
            g, "enrichplot_edge_diagnostic", diagnostic
        )
        return(g)
    } else {
        w <- pair_sim[
            unname(get_term_labels(enrichDf, enrichDf$ID)),
            unname(get_term_labels(enrichDf, enrichDf$ID))
        ]
    }

    wd <- reshape2::melt(w)
    wd <- wd[wd[, 1] != wd[, 2], ]
    ## The edge selector removes missing similarities and applies the requested
    ## strategy.  Threshold mode is intentionally equivalent to the former
    ## `similarity >= min_edge` matrix-cell filter.
    keep_edge <- filter_emap_edges(
        wd,
        edge_filter = edge_filter,
        min_edge = min_edge,
        top_k = top_k,
        target_density = target_density
    )

    label_map <- get_term_labels(enrichDf, enrichDf$ID)
    vertex_df <- data.frame(
        name = unname(label_map),
        stringsAsFactors = FALSE
    )
    if (!any(keep_edge)) {
        g <- graph_from_data_frame(
            data.frame(from = character(0), to = character(0)),
            directed = FALSE,
            vertices = vertex_df
        )
    } else {
        edge_df <- wd[keep_edge, , drop = FALSE]
        if (edge_filter != "threshold" && nrow(edge_df) > 0) {
            edge_key <- paste(
                pmin(as.character(edge_df[[1L]]), as.character(edge_df[[2L]])),
                pmax(as.character(edge_df[[1L]]), as.character(edge_df[[2L]])),
                sep = "\\r"
            )
            edge_df <- edge_df[!duplicated(edge_key), , drop = FALSE]
        }
        g <- graph_from_data_frame(edge_df[, -3], directed = FALSE, vertices = vertex_df)
        E(g)$width <- sqrt(edge_df[, 3] * 5) * cex_line
        # Use similarity as the weight(length) of an edge
        E(g)$weight <- edge_df[, 3]
    }

    diagnostic <- emapplot_edge_density(
        w,
        min_edge = min_edge,
        edge_filter = edge_filter,
        top_k = top_k,
        target_density = target_density
    )
    g <- igraph::set_graph_attr(
        g, "enrichplot_edge_diagnostic", diagnostic
    )

    idx <- match(V(g)$name, label_map)
    cnt <- sapply(geneSets[enrichDf$ID[idx]], length)
    V(g)$size <- cnt
    if (color %in% names(enrichDf)) {
        colVar <- enrichDf[idx, color]
    } else {
        colVar <- color
    }
    V(g)$color <- colVar
    return(g)
}

#' Merge the compareClusterResult file
#'
#' @param yy A data.frame of enrichment result.
#'
#' @return a data.frame
#' @noRd
merge_compareClusterResult <- function(yy) {
    yy_union <- yy[!duplicated(yy$ID), ]
    yy_ids <- lapply(split(yy, yy$ID), function(x) {
        ids <- unique(unlist(strsplit(x$geneID, "/")))
        cnt <- length(ids)
        list(ID = paste0(ids, collapse = "/"), cnt = cnt)
    })

    ids <- vapply(yy_ids, function(x) x$ID, character(1))
    cnt <- vapply(yy_ids, function(x) x$cnt, numeric(1))

    yy_union$geneID <- ids[yy_union$ID]
    yy_union$Count <- cnt[yy_union$ID]
    yy_union$Cluster <- NULL
    yy_union
}






#' Get the location of group label
#'
#' @param node_data node information data frame
#' @param label_format A numeric value sets wrap length, alternatively a
#' custom function to format axis labels.
#' @return a data.frame object.
#' @noRd
get_label_location <- function(node_data, label_format) {
    label_func <- default_labeller(label_format)
    if (is.function(label_format)) {
        label_func <- label_format
    }
    label_x <- stats::aggregate(x ~ color2, node_data, mean)
    label_y <- stats::aggregate(y ~ color2, node_data, mean)
    data.frame(x = label_x$x, y = label_y$y, label = label_func(label_x$color2))
}


#' Cluster similar nodes together by k-means
#'
#' @param node_data node information data frame.
#' @param enrichDf data.frame of enrichment result.
#' @param nWords Numeric, the number of words in the cluster tags.
#' @param clusterFunction clustering method function, such as `stats::kmeans`, `cluster::clara`,
#' `cluster::fanny`, or `cluster::pam`.
#' @param nCluster Numeric, the number of clusters,
#' the default value is square root of the number of nodes.
#' @noRd
groupNode <- function(
    node_data,
    enrichDf,
    nWords,
    clusterFunction = stats::kmeans,
    nCluster
) {
    wrongMessage <- paste(
        "Wrong clusterFunction parameter or unsupported clustering method;",
        "set to default `clusterFunction = kmeans`"
    )
    if (is.character(clusterFunction)) {
        clusterFunction <- eval(parse(text = clusterFunction))
    }
    if (!"color2" %in% colnames(node_data)) {
        dat <- data.frame(x = node_data$x, y = node_data$y)
        nCluster <- ifelse(
            is.null(nCluster),
            floor(sqrt(nrow(dat))),
            min(nCluster, nrow(dat))
        )
        node_data$color2 <- tryCatch(
            expr = clusterFunction(dat, nCluster)$cluster,
            error = function(e) {
                message(wrongMessage)
                clusterFunction(dat, nCluster)$cluster
            }
        )
        if (is.null(node_data$color2)) {
            message(wrongMessage)
            node_data$color2 <- clusterFunction(dat, nCluster)$cluster
        }
    }
    goid <- enrichDf$ID
    cluster_color <- unique(node_data$color2)
    clusters <- lapply(cluster_color, function(i) {
        goid[which(node_data$color2 == i)]
    })
    cluster_label <- sapply(
        cluster_color,
        get_wordcloud,
        node_data = node_data,
        nWords = nWords
    )
    names(cluster_label) <- cluster_color
    node_data$color2 <- cluster_label[as.character(node_data$color2)]
    return(node_data)   
}

#' Add ellipse to group nodes
#'
#' @param node_data node data frame
#' @param group_legend Logical, if TRUE, the grouping legend will be displayed.
#' The default is FALSE.
#' @param label logical, TRUE to label the ellipse (default)
#' @param ellipse_style style of ellipse, one of "ggforce" and "polygon".
#' @param ellipse_pro numeric indicating confidence value for the ellipses
#' @param alpha the transparency of ellipse fill.
#' @importFrom rlang check_installed
#' @importFrom ggplot2 scale_fill_discrete
#' @noRd
add_ellipse <- function(
    node_data,
    group_legend,
    label = TRUE,
    ellipse_style = "ggforce",
    # ellipse_pro = 0.95,
    alpha = 0.3,
    ...
) {
    show_legend <- c(group_legend, FALSE)
    names(show_legend) <- c("fill", "color")
    ellipse_style <- match.arg(ellipse_style, c("ggforce", "polygon"))

    require_suggested('ggforce', 'for `add_ellipse()`.');

    if (ellipse_style == "ggforce") {
        if (label) {
            p <- ggforce::geom_mark_ellipse(
                data = node_data,
                aes(
                    x = !!sym('x'),
                    y = !!sym('y'),
                    fill = !!sym('color2'),
                    label = !!sym('color2')
                ),
                alpha = alpha,
                color = NA,
                show.legend = show_legend
            )
        } else {
            p <- ggforce::geom_mark_ellipse(
                data = node_data,
                aes(x = !!sym('x'), y = !!sym('y'), fill = !!sym('color2')),
                alpha = alpha,
                color = NA,
                show.legend = show_legend
            )
        }
    }

    # not in used
    if (FALSE && ellipse_style == "polygon") {
        ellipse_pro <- 0.95  # Define default ellipse_pro value
        p <- ggplot2::stat_ellipse(
            data = node_data,
            aes(x = !!sym('x'), y = !!sym('y'), fill = !!sym('color2')),
            geom = "polygon",
            level = ellipse_pro,
            alpha = alpha,
            show.legend = group_legend,
            ...
        )
    }

    if (group_legend) {
        p <- list(p, scale_fill_discrete(name = "groups"))
    }

    return(p)
}


list2df <- ggtangle:::list2df

