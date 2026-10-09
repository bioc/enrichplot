## The node_label x group matrix has regressed twice before (#292, #339), so it
## is locked here in one place instead of being spread through emapplot().

test_that(".resolve_group_display() maps node_label and group to display flags", {
    cases <- list(
        list(node_label = "category", group = NULL,  ellipse = FALSE, glabel = FALSE, clabel = TRUE),
        list(node_label = "category", group = TRUE,  ellipse = TRUE,  glabel = FALSE, clabel = TRUE),
        list(node_label = "category", group = FALSE, ellipse = FALSE, glabel = FALSE, clabel = TRUE),
        list(node_label = "group",    group = NULL,  ellipse = TRUE,  glabel = TRUE,  clabel = FALSE),
        list(node_label = "group",    group = TRUE,  ellipse = TRUE,  glabel = TRUE,  clabel = FALSE),
        list(node_label = "group",    group = FALSE, ellipse = FALSE, glabel = FALSE, clabel = FALSE),
        list(node_label = "all",      group = NULL,  ellipse = TRUE,  glabel = TRUE,  clabel = TRUE),
        list(node_label = "all",      group = TRUE,  ellipse = TRUE,  glabel = TRUE,  clabel = TRUE),
        list(node_label = "all",      group = FALSE, ellipse = TRUE,  glabel = TRUE,  clabel = TRUE),
        list(node_label = "none",     group = NULL,  ellipse = FALSE, glabel = FALSE, clabel = FALSE),
        list(node_label = "none",     group = TRUE,  ellipse = TRUE,  glabel = FALSE, clabel = FALSE),
        list(node_label = "none",     group = FALSE, ellipse = FALSE, glabel = FALSE, clabel = FALSE),
        list(node_label = "category_grouped", group = NULL,  ellipse = TRUE,  glabel = FALSE, clabel = TRUE),
        list(node_label = "category_grouped", group = TRUE,  ellipse = TRUE,  glabel = FALSE, clabel = TRUE),
        list(node_label = "category_grouped", group = FALSE, ellipse = FALSE, glabel = FALSE, clabel = TRUE)
    )

    got <- lapply(cases, function(cs) {
        .resolve_group_display(cs$node_label, cs$group)
    })
    want <- lapply(cases, function(cs) {
        list(group_enabled = cs$ellipse,
             group_label = cs$glabel,
             category_label = cs$clabel)
    })

    expect_identical(got, want)
})

test_that(".resolve_group_display() keeps the historical group_enabled answers", {
    ## the extraction must not drift from the logic it replaced
    old_group_enabled <- function(node_label, group) {
        group_enabled <- node_label %in% c("group", "all")
        if (!is.null(group)) {
            group_enabled <- isTRUE(group)
            if (!group_enabled && node_label == "group") node_label <- "none"
        }
        if (node_label == "all") group_enabled <- TRUE
        group_enabled
    }
    for (nl in c("category", "group", "all", "none")) {
        for (g in list(NULL, TRUE, FALSE)) {
            expect_identical(.resolve_group_display(nl, g)$group_enabled,
                             old_group_enabled(nl, g))
        }
    }
})

test_that("category_grouped replaces the deprecated group = TRUE", {
    ## `group = TRUE` with `node_label = "category"` outlined the groups while
    ## still labelling the categories; "category_grouped" is its node_label form
    expect_identical(.resolve_group_display("category_grouped", NULL),
                     .resolve_group_display("category", TRUE))
})

test_that("an unknown node_label is rejected instead of doing nothing", {
    expect_error(.resolve_group_display("categoy", NULL), "should be one of")
})
