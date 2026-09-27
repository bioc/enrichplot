test_that("edge filters preserve threshold and select deterministic pairs", {
    labels <- LETTERS[1:4]
    sim <- matrix(
        c(
            1, .9, .2, .4,
            .9, 1, .8, .3,
            .2, .8, 1, .7,
            .4, .3, .7, 1
        ),
        nrow = 4,
        dimnames = list(labels, labels)
    )
    edge_data <- as.data.frame(as.table(sim), stringsAsFactors = FALSE)

    threshold <- enrichplot:::filter_emap_edges(
        edge_data,
        edge_filter = "threshold",
        min_edge = .5
    )
    expect_equal(sum(threshold), 6L)

    top_k <- enrichplot:::filter_emap_edges(
        edge_data,
        edge_filter = "top_k",
        min_edge = 0,
        top_k = 1
    )
    top_pairs <- unique(vapply(which(top_k), function(i) {
        paste(sort(as.character(edge_data[i, 1:2])), collapse = "|")
    }, character(1)))
    expect_setequal(top_pairs, c("A|B", "B|C", "C|D"))

    adaptive <- enrichplot:::filter_emap_edges(
        edge_data,
        edge_filter = "adaptive",
        min_edge = 0,
        target_density = .25
    )
    adaptive_pairs <- unique(vapply(which(adaptive), function(i) {
        paste(sort(as.character(edge_data[i, 1:2])), collapse = "|")
    }, character(1)))
    expect_length(adaptive_pairs, 2)
    expect_setequal(adaptive_pairs, c("A|B", "B|C"))
})

test_that("edge density diagnostic reports unique pairs and rendered rows", {
    sim <- matrix(
        c(1, .8, .1, .5, .8, 1, .6, .2, .1, .6, 1, .7, .5, .2, .7, 1),
        nrow = 4,
        byrow = TRUE,
        dimnames = list(LETTERS[1:4], LETTERS[1:4])
    )
    diagnostic <- emapplot_edge_density(sim, min_edge = .5)

    expect_s3_class(diagnostic, "data.frame")
    expect_equal(diagnostic$n_terms, 4L)
    expect_equal(diagnostic$possible_pairs, 6)
    expect_equal(diagnostic$retained_pairs, 4L)
    expect_equal(diagnostic$edge_rows, 8L)
    expect_equal(diagnostic$density, 2 / 3)
    expect_match(diagnostic$suggestion, "Graph density is high")
})

test_that("emapplot exposes opt-in filters and diagnostics", {
    env <- new.env(parent = globalenv())
    sys.source(test_path("helper-mock-results.R"), env)
    x <- pairwise_termsim(env$mock_enrich_result(), method = "JC")

    p <- emapplot(
        x,
        showCategory = 2,
        node_label = "none",
        edge_filter = "top_k",
        top_k = 1,
        edge_diagnostic = TRUE
    )
    expect_s3_class(p, "ggplot")
    expect_error(ggplot2::ggplot_build(p), NA)
    diagnostic <- attr(p, "enrichplot_edge_diagnostic")
    expect_s3_class(diagnostic, "data.frame")
    expect_equal(diagnostic$edge_filter, "top_k")
    expect_lte(diagnostic$retained_pairs, diagnostic$possible_pairs)

    expect_error(
        emapplot(x, showCategory = 2, node_label = "none", top_k = 0),
        "positive integer"
    )
    expect_error(
        emapplot(x, showCategory = 2, node_label = "none", size_edge = -1),
        "non-negative"
    )
})

test_that("ssplot forwards edge controls without changing term selection", {
    env <- new.env(parent = globalenv())
    sys.source(test_path("helper-mock-results.R"), env)
    x <- pairwise_termsim(env$mock_enrich_result(), method = "JC")

    p <- ssplot(
        x,
        showCategory = c("T1", "T2"),
        node_label = "none",
        min_edge = .3,
        size_edge = .8,
        edge_filter = "adaptive",
        target_density = .5,
        edge_diagnostic = TRUE
    )
    expect_s3_class(p, "ggplot")
    expect_error(ggplot2::ggplot_build(p), NA)
    diagnostic <- attr(p, "enrichplot_edge_diagnostic")
    expect_s3_class(diagnostic, "data.frame")
    expect_equal(diagnostic$edge_filter, "adaptive")
    expect_equal(diagnostic$min_edge, .3)
})

test_that("ordinary ssplot handles one and two selected terms", {
    env <- new.env(parent = globalenv())
    sys.source(test_path("helper-mock-results.R"), env)
    x <- pairwise_termsim(env$mock_enrich_result(), method = "JC")

    for (selection in list("T1", c("T1", "T2"))) {
        p <- ssplot(x, showCategory = selection, node_label = "none")
        expect_s3_class(p, "ggplot")
        expect_error(ggplot2::ggplot_build(p), NA)
    }
})
