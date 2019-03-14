#' facet_plot
#'
#' @param .data
#' @param majorCol
#' @param facetCol
#' @param major
facet_plot <- function(.data, majorCol, facetCol, major = params$major_of_interest) {
  matriculation_col <- enquo(majorCol)
  facet_col <- enquo(facetCol)
  .data %>%
    mutate(!!matriculation_col := fct_rev(fct_relevel(!!matriculation_col, major))) %>%
    pandy::pandy_summarize(!!matriculation_col, !!facet_col, strict = FALSE) %>%
    filter(!is.na(!!facet_col)) %>%
    #add_persist_and_yield() %>%
    ggplot(aes(persist, yield)) +
    stat_function(fun = function(x) c(.60 / x, .70 / x, .80 / x, .90 / x), alpha = 0.1) +
    geom_text(aes(label = value, y = y*1.015, x = 0.995, angle = -y*45),
              alpha = 0.5,
              data = contour_labels,
              nudge_x = -0.025, size = 3) +
    geom_point(aes(fill = if_else(!!matriculation_col == major, major, "Other"), size = n), shape = 21,
               stroke = 0.5, color = "black") +
    xlim(c(.60, 1.00)) +
    ylim(c(.60, 1.00)) +
    facet_grid(cols = vars(!!facet_col)) +
    labs(fill = "Matriculation Major") +
    xlab(paste0("persist in COE through ", params$n_persist,"th semester, ", major, " through 6th (ratio)")) +
    ylab("yield (ratio)")
}
