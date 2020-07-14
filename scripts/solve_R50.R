

#' Apply the R50 model to solve for R50 or a side of the confidence interval.
#'
#' @param R50_mod The model to used to predict.
#' @param turbines  Predict under what condition? A binary vector where 0 = no turbines, 1 = turbines present.
#' @param type A character value. One of "pred", "lower" or "upper". "pred" gives the predicted R50 value. "lower" or "upper" give the corresponding side of the confidence interval.
solve_R50 <- function(R50_mod, turbines, type) {
  # logic for `type` argument
  if (type %in% c("lower", "upper")) {
    pred_type <- "conf_int"
    pred_col <- sym(str_glue(".pred_{type}_1"))
  } else if (type == "pred") {
    pred_type <- "prob"
    pred_col <- sym(".pred_1")
  } else {
    stop(str_glue(
      "type = '{type}'. ",
      "Should be one of: 'pred', 'lower', 'upper',"
    ))
  }

  # function for solving in `uniroot`
  R50_root <- function(range, turbines) {
    suppressMessages(
      predict.model_fit(R50_mod,
                        tibble(range = range,
                               turbines = turbines),
                        type = pred_type)) %>%
      pull(pred_col) - .5
  }

  map_dbl(turbines,
          ~ uniroot(R50_root,
                    interval = range(R50_mod$fit$data$range),
                    turbines = .x)$root)
}
