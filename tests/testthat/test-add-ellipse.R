ellipse_node_data <- function() {
    set.seed(1)
    data.frame(
        x = c(rnorm(20), rnorm(20) + 3),
        y = c(rnorm(20), rnorm(20)),
        color2 = rep(c("a", "b"), each = 20),
        stringsAsFactors = FALSE
    )
}

test_that("add_ellipse() falls back to stat_ellipse() when ggforce is absent", {
    nd <- ellipse_node_data()
    p0 <- ggplot2::ggplot(nd, ggplot2::aes(x = x, y = y))

    ## `ggforce` is only suggested, but grouping is on by default in ssplot(),
    ## so the missing-package path must still draw the grouped outline.
    local_mocked_bindings(.has_ggforce = function() FALSE,
                          .package = "enrichplot")

    p <- p0 + add_ellipse(nd, group_legend = FALSE)
    b <- ggplot2::ggplot_build(p)

    expect_length(b$data, 1L)
    expect_true(all(c("x", "y", "group") %in% names(b$data[[1]])))
    expect_equal(length(unique(b$data[[1]]$group)), 2L)
})

test_that("add_ellipse() only draws the outline, never labels", {
    nd <- ellipse_node_data()
    p0 <- ggplot2::ggplot(nd, ggplot2::aes(x = x, y = y))

    p <- p0 + add_ellipse(nd, group_legend = FALSE)
    b <- ggplot2::ggplot_build(p)

    ## group labels are added by emapplot_internal(), so the outline layer must
    ## not carry a label aesthetic of its own
    expect_length(b$data, 1L)
    expect_false("label" %in% names(p$layers[[1]]$mapping))
})

test_that("the ggforce path still draws a single mark-ellipse layer", {
    skip_if_not_installed("ggforce")
    nd <- ellipse_node_data()
    p0 <- ggplot2::ggplot(nd, ggplot2::aes(x = x, y = y))

    p <- p0 + add_ellipse(nd, group_legend = FALSE)
    b <- ggplot2::ggplot_build(p)

    expect_length(b$data, 1L)
    expect_equal(length(unique(b$data[[1]]$group)), 2L)
})

test_that("group_legend = TRUE appends the fill legend scale", {
    skip_if_not_installed("ggforce")
    nd <- ellipse_node_data()
    p0 <- ggplot2::ggplot(nd, ggplot2::aes(x = x, y = y))

    p <- p0 + add_ellipse(nd, group_legend = TRUE)
    expect_true(any(vapply(p$scales$scales, function(s) "fill" %in% s$aesthetics,
                           logical(1))))
})
