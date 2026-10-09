## Argument-combination feedback: `top_k` / `target_density` only apply to one
## `edge_filter` each, and `group` is deprecated in favour of `node_label`.

test_that(".emapplot_arg_notices() stays quiet for the defaults", {
    expect_silent(.emapplot_arg_notices("threshold", 5, 0.1, NULL))
    expect_silent(.emapplot_arg_notices("top_k", 5, 0.1, NULL))
    expect_silent(.emapplot_arg_notices("adaptive", 5, 0.1, NULL))
    ## `min_edge` applies to every mode, so it is never reported
})

test_that(".emapplot_arg_notices() reports the inactive edge parameter", {
    expect_warning(.emapplot_arg_notices("threshold", 10, 0.1, NULL),
                   "top_k.*is ignored")
    expect_warning(.emapplot_arg_notices("threshold", 5, 0.3, NULL),
                   "target_density.*is ignored")
    expect_warning(.emapplot_arg_notices("top_k", 5, 0.3, NULL),
                   "target_density.*is ignored")
    expect_warning(.emapplot_arg_notices("adaptive", 10, 0.1, NULL),
                   "top_k.*is ignored")
})

test_that(".emapplot_arg_notices() flags the deprecated group argument", {
    expect_warning(.emapplot_arg_notices("threshold", 5, 0.1, TRUE),
                   "deprecated")
    expect_warning(.emapplot_arg_notices("threshold", 5, 0.1, FALSE),
                   "deprecated")
})
