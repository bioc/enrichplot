#' @rdname emapplot
#' @exportMethod emapplot
setMethod(
    "emapplot",
    signature(x = "enrichResult"),
    function(
        x,
        layout = igraph::layout_with_kk,
        coords = NULL,
        showCategory = 30,
        color = "p.adjust",
        size_category = 1,
        min_edge = .2,
        color_edge = "grey",
        size_edge = .5,
        group = NULL,
        group_legend = TRUE,
        node_label = "category",
        node_label_size = 5,
        pie = "equal",
        layer = NULL,
        label_format = 30,
        clusterFunction = stats::kmeans,
        nWords = 4,
        nCluster = NULL,
        show_category_size_legend = TRUE,
        edge_filter = "threshold",
        top_k = 5,
        target_density = .1,
        edge_diagnostic = FALSE,
        ...
    ) {
        emapplot_internal(
            x,
            layout = layout,
            coords = coords,
            showCategory = showCategory,
            color = color,
            size_category = size_category,
            min_edge = min_edge,
            edge_filter = edge_filter,
            top_k = top_k,
            target_density = target_density,
            edge_diagnostic = edge_diagnostic,
            color_edge = color_edge,
            size_edge = size_edge,
            group = group,
            group_legend = group_legend,
            node_label = node_label,
            node_label_size = node_label_size,
            pie = pie,
            layer = layer,
            label_format = label_format,
            clusterFunction = clusterFunction,
            nWords = nWords,
            nCluster = nCluster,
            show_category_size_legend = show_category_size_legend,
            ...
        )
    }
)

#' @rdname emapplot
#' @exportMethod emapplot
setMethod(
    "emapplot",
    signature(x = "gseaResult"),
    function(
        x,
        layout = igraph::layout_with_kk,
        coords = NULL,
        showCategory = 30,
        color = "p.adjust",
        size_category = 1,
        min_edge = .2,
        color_edge = "grey",
        size_edge = .5,
        group = NULL,
        group_legend = TRUE,
        node_label = "category",
        node_label_size = 5,
        pie = "equal",
        layer = NULL,
        label_format = 30,
        clusterFunction = stats::kmeans,
        nWords = 4,
        nCluster = NULL,
        show_category_size_legend = TRUE,
        edge_filter = "threshold",
        top_k = 5,
        target_density = .1,
        edge_diagnostic = FALSE,
        ...
    ) {
        emapplot_internal(
            x,
            layout = layout,
            coords = coords,
            showCategory = showCategory,
            color = color,
            size_category = size_category,
            min_edge = min_edge,
            edge_filter = edge_filter,
            top_k = top_k,
            target_density = target_density,
            edge_diagnostic = edge_diagnostic,
            color_edge = color_edge,
            size_edge = size_edge,
            group = group,
            group_legend = group_legend,
            node_label = node_label,
            node_label_size = node_label_size,
            pie = pie,
            layer = layer,
            label_format = label_format,
            clusterFunction = clusterFunction,
            nWords = nWords,
            nCluster = nCluster,
            show_category_size_legend = show_category_size_legend,
            ...
        )
    }
)

#' @rdname emapplot
#' @exportMethod emapplot
setMethod(
    "emapplot",
    signature(x = "compareClusterResult"),
    function(
        x,
        layout = igraph::layout_with_kk,
        coords = NULL,
        showCategory = 30,
        color = "p.adjust",
        size_category = 1,
        min_edge = .2,
        color_edge = "grey",
        size_edge = .5,
        group = NULL,
        group_legend = TRUE,
        node_label = "category",
        node_label_size = 5,
        pie = "equal",
        layer = NULL,
        label_format = 30,
        clusterFunction = stats::kmeans,
        nWords = 4,
        nCluster = NULL,
        show_category_size_legend = TRUE,
        edge_filter = "threshold",
        top_k = 5,
        target_density = .1,
        edge_diagnostic = FALSE,
        ...
    ) {
        emapplot_internal(
            x,
            layout = layout,
            coords = coords,
            showCategory = showCategory,
            color = color,
            size_category = size_category,
            min_edge = min_edge,
            edge_filter = edge_filter,
            top_k = top_k,
            target_density = target_density,
            edge_diagnostic = edge_diagnostic,
            color_edge = color_edge,
            size_edge = size_edge,
            group = group,
            group_legend = group_legend,
            node_label = node_label,
            node_label_size = node_label_size,
            pie = pie,
            layer = layer,
            label_format = label_format,
            clusterFunction = clusterFunction,
            nWords = nWords,
            nCluster = nCluster,
            show_category_size_legend = show_category_size_legend,
            ...
        )
    }
)

#' @rdname emapplot
#' @exportMethod emapplot
setMethod(
    "emapplot",
    signature(x = "mnseaResult"),
    function(
        x,
        layout = igraph::layout_with_kk,
        coords = NULL,
        showCategory = 30,
        color = "p.adjust",
        size_category = 1,
        min_edge = .2,
        color_edge = "grey",
        size_edge = .5,
        group = NULL,
        group_legend = TRUE,
        node_label = "category",
        node_label_size = 5,
        pie = "equal",
        layer = NULL,
        label_format = 30,
        clusterFunction = stats::kmeans,
        nWords = 4,
        nCluster = NULL,
        show_category_size_legend = TRUE,
        edge_filter = "threshold",
        top_k = 5,
        target_density = .1,
        edge_diagnostic = FALSE,
        ...
    ) {
        emapplot_internal(
            x,
            layout = layout,
            coords = coords,
            showCategory = showCategory,
            color = color,
            size_category = size_category,
            min_edge = min_edge,
            edge_filter = edge_filter,
            top_k = top_k,
            target_density = target_density,
            edge_diagnostic = edge_diagnostic,
            color_edge = color_edge,
            size_edge = size_edge,
            group = group,
            group_legend = group_legend,
            node_label = node_label,
            node_label_size = node_label_size,
            pie = pie,
            layer = layer,
            label_format = label_format,
            clusterFunction = clusterFunction,
            nWords = nWords,
            nCluster = nCluster,
            show_category_size_legend = show_category_size_legend,
            ...
        )
    }
)


#' @rdname emapplot
#' @param layout igraph layout
#' @param coords optional user-supplied coordinate data.frame with `x`, `y`
#'   and row names matching node labels.
#' @param color Variable used to color enriched terms, e.g. 'pvalue',
#' 'p.adjust' or 'qvalue'.
#' @param size_category relative size of the categories
#' @param min_edge The minimum similarity threshold for whether
#' two nodes are connected, should be between 0 and 1, default value is 0.2.
#' @param edge_filter Edge filtering strategy. `min_edge` applies to all three;
#' `top_k` only applies to `"top_k"` and `target_density` only to `"adaptive"`.
#' Supplying a non-default value for the inactive one warns rather than being
#' ignored silently.
#' @param top_k Number of strongest neighbors per term. Only used when
#' `edge_filter = "top_k"`.
#' @param target_density Target proportion of unique term pairs. Only used when
#' `edge_filter = "adaptive"`.
#' @param edge_diagnostic Logical; if `TRUE`, attach a one-row
#'   `enrichplot_edge_diagnostic` data frame to the returned plot. The
#'   diagnostic reports retained pairs, rendered edge rows and graph density.
#' @param color_edge color of the network edge
#' @param size_edge relative size of edge width
#' @param node_label Select which labels to display and whether the groups are
#' outlined. One of:
#'
#'   | `node_label` | group outline | group labels | category labels |
#'   | --- | --- | --- | --- |
#'   | `"none"` | no | no | no |
#'   | `"category"` | no | no | yes |
#'   | `"category_grouped"` | yes | no | yes |
#'   | `"group"` | yes | yes | no |
#'   | `"all"` | yes | yes | yes |
#'
#' @param node_label_size size of node label, default is 5.
#' @param pie one of 'equal' or 'Count' to set the slice ratio of the pies
#' @param layer optional layer or layers to retain for `mnseaResult` plots.
#' @param group Deprecated. `group = TRUE` is `node_label = "category_grouped"`;
#' `group = FALSE` is `node_label = "none"`. Use `node_label` instead.
#' @param group_legend logical, if TRUE, draw a legend for the groups.
#' @param label_format a numeric value sets wrap length, alternatively a custom function to format axis labels.
#' @param clusterFunction clustering method function, such as `stats::kmeans` (default),
#' `cluster::clara`, `cluster::fanny`, or `cluster::pam`.
#' @param nWords Numeric, the number of words in the cluster tags, the default value is 4.
#' @param nCluster Numeric, the number of clusters,
#' the default value is square root of the number of nodes.
#' @param show_category_size_legend Logical, whether to draw the category-size
#'   annotation legend for compareCluster pie charts.
#' @importFrom ggplot2 scale_size
#' @importFrom ggtangle geom_edge
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggrepel geom_label_repel
#' @author Guangchuang Yu
prepare_emapplot_data <- function(x, showCategory, color, min_edge, size_edge,
                                   edge_filter = "threshold", top_k = 5,
                                   target_density = .1) {
    ## this path feeds x@termsim straight into the graph builder, so an
    ## unpopulated matrix used to crash with "no 'dimnames' attribute for array"
    has_pairsim(x)
    selected <- select_terms(x, showCategory)
    g <- build_emap_graph(
        enrichDf = selected$result,
        geneSets = selected$geneSets,
        color = color,
        cex_line = size_edge,
        min_edge = min_edge,
        pair_sim = x@termsim,
        edge_filter = edge_filter,
        top_k = top_k,
        target_density = target_density
    )
    plot_result <- selected$result
    plot_result$Description <- unname(selected$labels)

    list(
        graph = g,
        geneSet = selected$geneSets,
        result = plot_result
    )
}

prepare_emapplot_mnsea_feature_data <- function(x, ids, layer = NULL) {
    ids <- unique(as.character(ids))
    ids <- ids[!is.na(ids) & nzchar(ids)]
    if (length(ids) == 0) {
        return(data.frame())
    }

    feature_list <- lapply(ids, function(id) {
        fortify_mnsea_contribution(
            x,
            level = "feature",
            pathway_id = id,
            layer = layer
        )
    })
    feature_df <- do.call(rbind, feature_list)
    if (is.null(feature_df) || nrow(feature_df) == 0) {
        return(data.frame())
    }
    rownames(feature_df) <- NULL
    feature_df
}

prepare_mnsea_similarity_data <- function(x, showCategory, layer = NULL) {
    selected <- select_terms(x, showCategory)
    if (nrow(selected$result) == 0) {
        yulab.utils::yulab_abort("No mnsea pathways available for plotting.")
    }

    pathway_df <- fortify_mnsea_contribution(x, level = "pathway", layer = layer)
    feature_df <- prepare_emapplot_mnsea_feature_data(x, ids = selected$ids, layer = layer)
    if (nrow(pathway_df) == 0 || nrow(feature_df) == 0) {
        yulab.utils::yulab_abort("No mnsea layers available after filtering.")
    }

    pathway_df <- pathway_df[pathway_df$ID %in% selected$ids, , drop = FALSE]
    feature_df <- feature_df[feature_df$ID %in% selected$ids, , drop = FALSE]
    retained_ids <- intersect(unique(pathway_df$ID), unique(feature_df$ID))
    if (length(retained_ids) == 0) {
        yulab.utils::yulab_abort("No mnsea pathways available after filtering.")
    }

    keep <- selected$ids %in% retained_ids
    selected$result <- selected$result[keep, , drop = FALSE]
    selected$ids <- selected$ids[keep]
    selected$labels <- selected$labels[keep]
    selected$geneSets <- lapply(selected$ids, function(id) {
        unique(feature_df$Feature[feature_df$ID == id])
    })
    names(selected$geneSets) <- selected$ids
    selected$geneSets <- set_geneSet_labels(selected$geneSets, selected$labels)

    pair_sim <- x@termsim
    method <- x@method
    term_labels <- unname(selected$labels)

    use_cached_termsim <- length(pair_sim) > 0 &&
        !is.null(dim(pair_sim)) &&
        all(term_labels %in% rownames(pair_sim))

    if (use_cached_termsim) {
        pair_sim <- pair_sim[term_labels, term_labels, drop = FALSE]
        if (!nzchar(method)) {
            method <- "JC"
        }
    } else {
        pair_sim <- get_similarity_matrix(
            y = selected$result,
            geneSets = selected$geneSets,
            method = "JC"
        )
        method <- "JC"
    }

    pathway_summary <- stats::aggregate(
        cbind(contribution, share, n_feature) ~ ID + Description,
        data = pathway_df[pathway_df$ID %in% selected$ids, , drop = FALSE],
        FUN = max
    )
    pathway_summary$Description <- as.character(pathway_summary$Description)

    plot_result <- selected$result
    plot_result$Description <- term_labels
    plot_result <- merge(
        plot_result,
        pathway_summary,
        by = c("ID", "Description"),
        all.x = TRUE,
        sort = FALSE
    )

    list(
        result = plot_result,
        geneSet = selected$geneSets,
        pair_sim = pair_sim,
        method = method
    )
}

prepare_emapplot_mnsea_data <- function(
    x, showCategory, color, min_edge, size_edge,
    layer = NULL, edge_filter = "threshold", top_k = 5,
    target_density = .1
) {
    plot_data <- prepare_mnsea_similarity_data(
        x,
        showCategory = showCategory,
        layer = layer
    )

    g <- build_emap_graph(
        enrichDf = plot_data$result,
        geneSets = plot_data$geneSet,
        color = color,
        cex_line = size_edge,
        min_edge = min_edge,
        pair_sim = plot_data$pair_sim,
        edge_filter = edge_filter,
        top_k = top_k,
        target_density = target_density
    )

    list(
        graph = g,
        geneSet = plot_data$geneSet,
        result = plot_data$result
    )
}

## Decide how grouped nodes are drawn from the user-facing `node_label` and the
## compatibility argument `group`.  Pure function, kept out of
## `emapplot_internal()` so every combination can be checked without building a
## plot -- this logic has been the source of past regressions (#292, #339).
## Returns:
##   group_enabled  - draw the group outline (and compute `node_data`)
##   group_label    - draw one label per group (for "group" and "all")
##   category_label - draw one label per category (for "category",
##                   "category_grouped" and "all")
## Every label is added by `emapplot_internal()` itself, so the outline layer
## and the label layers no longer depend on one another.
.resolve_group_display <- function(node_label, group = NULL) {
    node_label <- match.arg(
        node_label,
        c("category", "group", "all", "none", "category_grouped")
    )
    was_all <- identical(node_label, "all")

    ## "category_grouped" is the `node_label` spelling of the deprecated
    ## `group = TRUE` + `node_label = "category"`: outline the groups, but keep
    ## labelling the categories.
    group_enabled <- node_label %in% c("group", "all", "category_grouped")

    if (!is.null(group)) {
        group_enabled <- isTRUE(group)
        if (!group_enabled && node_label == "group") {
            node_label <- "none"
        }
    }

    ## `node_label = "all"` wins over `group`: it always groups, and labels both
    ## the groups and the categories.
    if (was_all) {
        group_enabled <- TRUE
        node_label <- "category"
    }

    list(
        group_enabled = group_enabled,
        group_label = was_all || identical(node_label, "group"),
        category_label = node_label %in% c("category", "category_grouped")
    )
}

## Report argument combinations that would otherwise be dropped silently, plus
## the deprecated `group` argument.  Called once from `emapplot_internal()` so
## that every user-facing entry point (`emapplot()`, `ssplot()`) is covered
## exactly once per call.
.emapplot_arg_notices <- function(edge_filter, top_k, target_density, group) {
    edge_filter <- match.arg(edge_filter, c("threshold", "top_k", "adaptive"))

    if (!is.null(group)) {
        yulab.utils::yulab_warn(c(
            "The `group` argument is deprecated.",
            "i" = paste0(
                "Use `node_label` instead: \"category_grouped\" for ",
                "`group = TRUE`, and \"none\" for `group = FALSE`."
            )
        ), class = "deprecatedGroupArgument")
    }

    ## `min_edge` applies to every mode, so it is never reported here.
    if (edge_filter != "top_k" && !isTRUE(all.equal(as.numeric(top_k), 5))) {
        yulab.utils::yulab_warn(c(
            sprintf("`top_k` is ignored because `edge_filter` is \"%s\".", edge_filter),
            "i" = "`top_k` only applies when `edge_filter = \"top_k\"`."
        ), class = "ignoredEdgeParameter")
    }
    if (edge_filter != "adaptive" &&
        !isTRUE(all.equal(as.numeric(target_density), 0.1))) {
        yulab.utils::yulab_warn(c(
            sprintf("`target_density` is ignored because `edge_filter` is \"%s\".",
                    edge_filter),
            "i" = "`target_density` only applies when `edge_filter = \"adaptive\"`."
        ), class = "ignoredEdgeParameter")
    }

    invisible(TRUE)
}

emapplot_internal <- function(
    x,
    layout = igraph::layout_with_kk,
    coords = NULL,
    showCategory = 30,
    color = "p.adjust",
    size_category = 1,
    min_edge = .2,
    edge_filter = "threshold",
    top_k = 5,
    target_density = .1,
    color_edge = "grey",
    size_edge = .5,
    group = NULL,
    group_legend = TRUE,
    node_label = "category",
    node_label_size = 5,
    pie = "equal",
    layer = NULL,
    label_format = 30,
    clusterFunction = stats::kmeans,
    nWords = 4,
    nCluster = NULL,
    show_category_size_legend = TRUE,
    edge_diagnostic = FALSE
) {
    .emapplot_arg_notices(edge_filter, top_k, target_density, group)

    if (length(edge_diagnostic) != 1L || is.na(edge_diagnostic) ||
        !is.logical(edge_diagnostic)) {
        stop('"edge_diagnostic" should be a single logical value.')
    }
    if (inherits(x, 'compareClusterResult')) {
        gg <- graph_from_compareClusterResult(
            x,
            showCategory = showCategory,
            color = color,
            min_edge = min_edge,
            size_edge = size_edge,
            edge_filter = edge_filter,
            top_k = top_k,
            target_density = target_density
        )
    } else if (inherits(x, 'mnseaResult')) {
        gg <- prepare_emapplot_mnsea_data(
            x,
            showCategory = showCategory,
            color = color,
            min_edge = min_edge,
            size_edge = size_edge,
            edge_filter = edge_filter,
            top_k = top_k,
            target_density = target_density,
            layer = layer
        )
    } else {
        gg <- prepare_emapplot_data(
            x, showCategory, color, min_edge, size_edge,
            edge_filter = edge_filter,
            top_k = top_k,
            target_density = target_density
        )
    }

    g <- gg$graph
    size <- vapply(gg$geneSet, length, FUN.VALUE = numeric(1))
    names(size) <- unname(get_geneSet_labels(gg$geneSet))
    V(g)$size = size[V(g)$name]

    if (!is.null(coords)) {
        coords <- as.data.frame(coords)
        if (!all(c("x", "y") %in% colnames(coords))) {
            yulab.utils::yulab_abort("`coords` must contain `x` and `y` columns.")
        }
        coords <- coords[, c("x", "y"), drop = FALSE]
        if (is.null(rownames(coords)) || anyDuplicated(rownames(coords))) {
            yulab.utils::yulab_abort(
                "`coords` must use unique node labels as row names."
            )
        }
        if (!all(vapply(coords, is.numeric, logical(1))) ||
            any(!is.finite(as.matrix(coords)))) {
            yulab.utils::yulab_abort(
                "`coords` must contain finite numeric `x` and `y` values."
            )
        }
        if (!all(igraph::V(g)$name %in% rownames(coords))) {
            yulab.utils::yulab_abort(
                "`coords` row names must include every displayed node label."
            )
        }
        layout_coords <- coords
        layout <- function(graph) {
            as.matrix(layout_coords[igraph::V(graph)$name, c("x", "y"), drop = FALSE])
        }
    }

    p <- ggplot(g, layout = layout)
    if (igraph::ecount(g) > 0) {
        p <- p + geom_edge(color = color_edge, linewidth = size_edge)
    }

    if (inherits(x, 'compareClusterResult')) {
        p <- add_node_pie(
            p,
            gg$data,
            pie,
            category_scale = size_category,
            show_size_legend = show_category_size_legend
        )
    } else {
        if (color %in% names(gg$result)) {
            color_scale <- switch(
                color,
                NES = list(
                    colors = get_enrichplot_color(3),
                    transform = "identity",
                    reverse = TRUE
                ),
                contribution = list(
                    colors = get_enrichplot_color(2),
                    transform = "identity",
                    reverse = TRUE
                ),
                share = list(
                    colors = get_enrichplot_color(2),
                    transform = "identity",
                    reverse = TRUE
                ),
                list(
                    colors = get_enrichplot_color(2),
                    transform = "log10",
                    reverse = TRUE
                )
            )
            p <- p %<+%
                gg$result[, c("Description", color)] +
                geom_point(aes(color = .data[[color]], size = .data$size)) +
                scale_size(
                    range = c(3, 8) * size_category,
                    name = if (inherits(x, "mnseaResult")) "Feature count" else ggplot2::waiver()
                )
            p <- p + set_enrichplot_color(
                colors = color_scale$colors,
                name = if (inherits(x, "mnseaResult")) mnsea_plot_label(color) else color,
                transform = color_scale$transform,
                reverse = color_scale$reverse
            )
            p <- p +
                guides(
                    size = guide_legend(order = 1),
                    color = guide_colorbar(order = 2, reverse = color_scale$reverse)
                )
        } else {
            p <- p %<+%
                gg$result[, "Description", drop = FALSE] +
                geom_point(aes(size = .data$size), color = color) +
                scale_size(
                    range = c(3, 8) * size_category,
                    name = if (inherits(x, "mnseaResult")) "Feature count" else ggplot2::waiver()
                )
        }
    }

    display <- .resolve_group_display(node_label, group)
    group_enabled <- display$group_enabled
    group_label <- display$group_label
    category_label <- display$category_label

    if (group_enabled) {
        if (inherits(x, 'compareClusterResult')) {
            p <- p + ggnewscale::new_scale_fill()
        } #else {
        # p <- p + ggnewscale::new_scale_color()
        #}
        node_data <- groupNode(
            p@data,
            as.data.frame(x),
            nWords,
            clusterFunction = clusterFunction,
            nCluster = nCluster
        )

        p <- p +
            add_ellipse(
                node_data,
                group_legend = group_legend
            )
    }

    ## add node label
    if (category_label) {
        p <- p +
            geom_text_repel(
                aes(label = .data$label),
                bg.color = "white",
                bg.r = .1,
                size = node_label_size
            )
    }
    ## add group label
    if (group_label) {
        label_location <- get_label_location(
            node_data = node_data,
            label_format = label_format
        )
        p <- p +
            geom_text_repel(
                aes(x = .data$x, y = .data$y, label = .data$label),
                data = label_location,
                bg.color = "white",
                bg.r = .1,
                size = node_label_size
            )
    }

    p <- p +
        coord_equal() +
        guides(
            size = guide_legend(order = 1),
            color = guide_colorbar(order = 2)
        )
    if (isTRUE(edge_diagnostic)) {
        attr(p, "enrichplot_edge_diagnostic") <- igraph::graph_attr(
            g, "enrichplot_edge_diagnostic"
        )
    }
    p
}

graph_from_compareClusterResult <- function(
    x,
    showCategory = 30,
    color = "p.adjust",
    min_edge = .2,
    size_edge = .5,
    edge_filter = "threshold",
    top_k = 5,
    target_density = .1
) {
    ## x@termsim goes straight into the graph builder below, so an unpopulated
    ## matrix used to crash with "no 'dimnames' attribute for array"
    has_pairsim(x)
    d <- tidy_compareCluster(x, showCategory)
    mergedEnrichDf <- merge_compareClusterResult(d)
    term_labels <- unname(get_term_labels(mergedEnrichDf, mergedEnrichDf$ID))
    label_map <- stats::setNames(term_labels, mergedEnrichDf$ID)
    d$Description <- unname(label_map[d$ID])
    mergedEnrichDf$Description <- term_labels
    gs <- setNames(
        strsplit(as.character(mergedEnrichDf$geneID), "/", fixed = TRUE),
        mergedEnrichDf$ID
    )

    g <- build_emap_graph(
        enrichDf = mergedEnrichDf,
        geneSets = gs,
        color = color,
        cex_line = size_edge,
        min_edge = min_edge,
        pair_sim = x@termsim,
        edge_filter = edge_filter,
        top_k = top_k,
        target_density = target_density
    )
    return(list(graph = g, geneSet = gs, data = d))
}
