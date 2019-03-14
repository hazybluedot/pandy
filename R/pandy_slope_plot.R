#' pandy_slope_plot
#'
#' @param .data
#' @param groupCol
#' @param slopeCol
pandy_slope_plot <- function(.data, groupCol, slopeCol = NULL, title = "") {
  groupCol <- enquo(groupCol)
  slopeCol <- enquo(slopeCol)

  title <- if_else(str_length(title) > 0, title, paste0('Success Metrics across ', rlang::quo_text(groupCol), ' and ', rlang::quo_text(slopeCol)))

  contour_labels <- data.frame(y = c(.6, .7, .8, .9)) %>%
    mutate(value = paste0(y*100, "% grad rate"), angle = (180/pi) * (-y / (1.5)))

  n_persist_terms <- attr(.data, "n_persist_term")

  point_aes <- if (!is.null(slopeCol)) {
    aes_(color = slopeCol, shape = groupCol)
  } else {
    aes_(color = groupCol)
  }

  p <- ggplot(.data %>% filter(n > 5), aes(persist, yield)) +
    stat_function(fun = function(x) c(.60 / x, .70 / x, .80 / x, .90 / x), alpha = 0.1) +
    geom_text(aes(label = value, y = y, x = 1.0, angle = angle), alpha = 0.5, data = contour_labels, nudge_x = -0.025)

  p <- if (!is.null(slopeCol)) {
    p + geom_line(aes_(group = groupCol))
  } else {
    p
  }

  p + geom_point(point_aes, size = 4) +
    xlim(c(.60, 1.00)) +
    ylim(c(.60, 1.00)) +
    xlab(paste0("persist through ", n_persist_terms,"th semester (ratio)")) +
    ylab("yield (ratio)") +
    labs(shape = "Matriculation Major",
         title = title) +
    scale_color_brewer(palette = "Dark2")
}
