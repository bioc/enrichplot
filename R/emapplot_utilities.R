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
    rownames(w) <- colnames(w) <- y[colnames(w), "Description"]
    return(w)
}


#' Check whether the similarity matrix exists
#'
#' @param x result of enrichment analysis
#'
#' @noRd
has_pairsim <- function(x) {
    if (length(x@termsim) == 0) {
        error_message <- paste(
            "Term similarity matrix not available.",
            "Please use pairwise_termsim function to",
            "deal with the results of enrichment analysis."
        )
        stop(error_message)
    }
}


#' Filter and diagnose enrichment-map edges
#'
#' @param edge_data data.frame whose first two columns identify terms and whose
#'   third column contains similarities.
#' @param edge_filter one of `"threshold"`, `"top_k"`, or `"adaptive"`.
#' @param min_edge minimum similarity floor.
#' @param top_k strongest neighbors per term in top-k mode.
#' @param target_density target proportion of unique pairs in adaptive mode.
#' @noRd
filter_emap_edges <- function(
    edge_data,
    edge_filter = "threshold",
    min_edge = .2,
    top_k = 5,
    target_density = .1
) {
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
    if (!is.data.frame(edge_data) || ncol(edge_data) < 3) {
        stop("`edge_data` must contain at least three columns.")
    }

    similarity <- as.numeric(edge_data[[3L]])
    from <- as.character(edge_data[[1L]])
    to <- as.character(edge_data[[2L]])
    keep <- rep(FALSE, nrow(edge_data))
    valid <- is.finite(similarity) & !is.na(from) & !is.na(to) & from != to
    if (!any(valid)) {
        return(keep)
    }
    if (edge_filter == "threshold") {
        return(valid & similarity >= min_edge)
    }

    valid_idx <- which(valid)
    left <- pmin(from[valid_idx], to[valid_idx])
    right <- pmax(from[valid_idx], to[valid_idx])
    pair_key <- paste(left, right, sep = "\\r")
    pair_rows <- split(valid_idx, pair_key)
    pair_names <- names(pair_rows)
    pair_idx <- vapply(pair_rows, function(ii) {
        ii[order(-similarity[ii], from[ii], to[ii], method = "radix")[1L]]
    }, integer(1L))
    pair_similarity <- similarity[pair_idx]
    eligible <- pair_names[pair_similarity >= min_edge]
    if (length(eligible) == 0L) {
        return(keep)
    }

    if (edge_filter == "top_k") {
        pair_left <- vapply(pair_rows, function(ii) from[ii[1L]], character(1L))
        pair_right <- vapply(pair_rows, function(ii) to[ii[1L]], character(1L))
        raw_left <- pair_left
        raw_right <- pair_right
        pair_left <- pmin(raw_left, raw_right)
        pair_right <- pmax(raw_left, raw_right)
        eligible_idx <- match(eligible, pair_names)
        terms <- sort(unique(c(pair_left[eligible_idx], pair_right[eligible_idx])))
        selected <- character()
        for (term in terms) {
            incident <- eligible_idx[
                pair_left[eligible_idx] == term |
                    pair_right[eligible_idx] == term
            ]
            ord <- order(-pair_similarity[incident], pair_names[incident],
                         method = "radix")
            selected <- c(
                selected,
                pair_names[incident[ord[seq_len(min(length(ord), top_k))]]]
            )
        }
        keep[valid_idx] <- pair_key %in% unique(selected)
        return(keep)
    }

    possible <- choose(length(unique(c(from[valid_idx], to[valid_idx]))), 2)
    n_keep <- min(length(eligible), ceiling(target_density * possible))
    if (n_keep < 1L) {
        return(keep)
    }
    eligible_idx <- match(eligible, pair_names)
    ord <- order(-pair_similarity[eligible_idx], pair_names[eligible_idx],
                 method = "radix")
    keep[valid_idx] <- pair_key %in% eligible[ord[seq_len(n_keep)]]
    keep
}

#' Diagnose enrichment-map edge density
#'
#' @param pair_sim square numeric similarity matrix.
#' @param min_edge minimum similarity threshold.
#' @param edge_filter edge filtering strategy.
#' @param top_k number of neighbors per term.
#' @param target_density target adaptive density.
#' @return A one-row data.frame with retained-pair counts and density.
#' @export
emapplot_edge_density <- function(
    pair_sim,
    min_edge = .2,
    edge_filter = "threshold",
    top_k = 5,
    target_density = .1
) {
    if (!is.matrix(pair_sim) || nrow(pair_sim) != ncol(pair_sim) ||
        !is.numeric(pair_sim)) {
        stop("`pair_sim` must be a square numeric matrix.")
    }
    wd <- reshape2::melt(pair_sim)
    keep <- filter_emap_edges(
        wd, edge_filter = edge_filter, min_edge = min_edge,
        top_k = top_k, target_density = target_density
    )
    keys <- vapply(which(keep), function(i) {
        paste(sort(as.character(wd[i, 1:2])), collapse = "\\r")
    }, character(1L))
    possible <- choose(nrow(pair_sim), 2)
    retained <- length(unique(keys))
    data.frame(
        n_terms = nrow(pair_sim),
        possible_pairs = possible,
        retained_pairs = retained,
        edge_rows = sum(keep),
        density = if (possible == 0) 0 else retained / possible,
        edge_filter = match.arg(edge_filter, c("threshold", "top_k", "adaptive")),
        min_edge = min_edge,
        top_k = as.integer(top_k),
        target_density = target_density,
        suggestion = if (possible > 0 && retained / possible > .2) {
            "Graph density is high; consider increasing `min_edge` or using `top_k`/`adaptive`."
        } else {
            "Graph density is within the requested display range."
        },
        stringsAsFactors = FALSE
    )
}


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
    method,
    edge_filter = "threshold",
    top_k = 5,
    target_density = .1
) {
    if (length(cex_line) != 1L || !is.numeric(cex_line) ||
        !is.finite(cex_line) || cex_line < 0) {
        stop('"size_edge" should be a finite non-negative number.')
    }
    filter_emap_edges(
        data.frame(character(), character(), numeric()),
        edge_filter = edge_filter,
        min_edge = min_edge,
        top_k = top_k,
        target_density = target_density
    )
    if (is.null(dim(enrichDf)) || nrow(enrichDf) == 1) {
        g <- igraph::make_empty_graph(n = 1, directed = FALSE)
        V(g)$name <- as.character(enrichDf$Description)
        V(g)$color <- "red"
        g <- igraph::set_graph_attr(
            g, "enrichplot_edge_diagnostic",
            data.frame(
                n_terms = 1L, possible_pairs = 0, retained_pairs = 0L,
                edge_rows = 0L, density = 0, edge_filter = edge_filter,
                min_edge = min_edge, top_k = as.integer(top_k),
                target_density = target_density,
                suggestion = "Fewer than two terms are available.",
                stringsAsFactors = FALSE
            )
        )
        return(g)
    }

    w <- pair_sim[
        as.character(enrichDf$Description),
        as.character(enrichDf$Description),
        drop = FALSE
    ]
    wd <- reshape2::melt(w)
    wd <- wd[wd[, 1] != wd[, 2], , drop = FALSE]
    wd <- wd[!is.na(wd[, 3]), , drop = FALSE]
    raw_edge_df <- wd
    keep <- filter_emap_edges(
        raw_edge_df, edge_filter = edge_filter, min_edge = min_edge,
        top_k = top_k, target_density = target_density
    )

    if (edge_filter == "threshold") {
        # Keep the release branch's default graph construction exactly: build
        # from all matrix cells, then remove sub-threshold edges. This retains
        # the historical behavior for empty or disconnected maps.
        g <- igraph::graph_from_data_frame(
            raw_edge_df[, -3, drop = FALSE], directed = FALSE
        )
        E(g)$width <- sqrt(raw_edge_df[, 3] * 5) * cex_line
        E(g)$weight <- raw_edge_df[, 3]
        g <- igraph::delete_edges(g, E(g)[raw_edge_df[, 3] < min_edge])
        idx <- unlist(lapply(V(g)$name, function(label) {
            which(label == enrichDf$Description)
        }))
        V(g)$size <- lengths(geneSets[idx])
        V(g)$color <- if (color %in% names(enrichDf)) {
            enrichDf[idx, color]
        } else {
            color
        }
    } else {
        edge_df <- raw_edge_df[keep, , drop = FALSE]
        if (nrow(edge_df) > 0) {
            key <- paste(
                pmin(as.character(edge_df[, 1]), as.character(edge_df[, 2])),
                pmax(as.character(edge_df[, 1]), as.character(edge_df[, 2])),
                sep = "\\r"
            )
            edge_df <- edge_df[!duplicated(key), , drop = FALSE]
        }
        vertex_df <- data.frame(
            name = unique(as.character(enrichDf$Description)),
            stringsAsFactors = FALSE
        )
        g <- igraph::graph_from_data_frame(
            edge_df[, -3, drop = FALSE], directed = FALSE,
            vertices = vertex_df
        )
        if (igraph::ecount(g) > 0) {
            E(g)$width <- sqrt(edge_df[, 3] * 5) * cex_line
            E(g)$weight <- edge_df[, 3]
        }
        idx <- match(V(g)$name, as.character(enrichDf$Description))
        V(g)$size <- lengths(geneSets[idx])
        V(g)$color <- if (color %in% names(enrichDf)) {
            enrichDf[idx, color]
        } else {
            color
        }
    }
    diagnostic <- emapplot_edge_density(
        w, min_edge = min_edge, edge_filter = edge_filter,
        top_k = top_k, target_density = target_density
    )
    igraph::set_graph_attr(g, "enrichplot_edge_diagnostic", diagnostic)
}




#' Get an iGraph object
#'
#' @param x Enrichment result.
#' @param nCategory Number of enriched terms to display.
#' @param color variable that used to color enriched terms, e.g. 'pvalue',
#' 'p.adjust' or 'qvalue'.
#' @param cex_line Scale of line width.
#' @param min_edge The minimum similarity threshold for whether
#' two nodes are connected, should between 0 and 1, default value is 0.2.
#'
#' @return an iGraph object
#' @noRd
get_igraph <- function(
    x, nCategory, color, cex_line, min_edge,
    edge_filter = "threshold", top_k = 5, target_density = .1
) {
    y <- as.data.frame(x)
    geneSets <- geneInCategory(x) ## use core gene for gsea result
    if (is.numeric(nCategory)) {
        y <- y[1:nCategory, ]
    } else {
        y <- y[match(nCategory, y$Description), ]
        nCategory <- length(nCategory)
    }

    if (nCategory == 0) {
        stop("no enriched term found...")
    }

    build_emap_graph(
        enrichDf = y,
        geneSets = geneSets,
        color = color,
        cex_line = cex_line,
        min_edge = min_edge,
        pair_sim = x@termsim,
        method = x@method,
        edge_filter = edge_filter,
        top_k = top_k,
        target_density = target_density
    )
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

    check_installed('ggforce', 'for `add_ellipse()`.');

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

