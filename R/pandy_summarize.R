add_persist_and_yield <- function(success_tbl) {
  dplyr::mutate(success_tbl, persist = n_semester_persist/n,
         yield = n_year_degree/n_semester_persist,
         grad_rate = persist*yield)
}

#' Summarize persistence and yield
#'
#' @param .data
#' @param strict when TRUE, `n_semester_persist` only counts students who's `max_term` is `>= persist_term`. When FALSE, `n_semester_persist` also counts students who earned a COE degree, regatdless of their `max_term` value. This was implemented because under strict=TRUE the degree rates for CS and possibly other majors were much lower than expected, including those students who earned a degree in fewer than 8 semesters resulted in degree rates that were more reasonable. This parameter allows for comparing the two methods.
pandy_summarize <- function(.data, ..., persist_term = 8, degree_time = 5.92, persist_in_major = 6, strict = FALSE) {
  group_var <- quos(...)

  n_persist_expr <- 'max_term >= persist_term & terms_in_major >= persist_in_major'
  if (!strict) {
    n_persist_expr <- 'max_term >= persist_term & terms_in_major >= persist_in_major | hasDegree'
  }

  #.data %>% add_degree_status(!!matriculationCol) -> .data
  if (strict) {
    .data <- dplyr::mutate(.data, hasDegree = if_else(max_term >= persist_term, hasDegree, FALSE))
  }

  structure(
    .data %>%
      dplyr::group_by(!!!group_var) %>%
      dplyr::summarize(
        n = n(),
        n_semester_persist = sum(!!!rlang::parse_exprs(n_persist_expr)),
        n_year_degree = sum(hasDegree &
                              coe_years_to_degree <= degree_time, na.rm = TRUE)
      ) %>%
      add_persist_and_yield(),
    n_persist_term = persist_term,
    n_degree_years = degree_time
  )
}

#' filter student records and count number of terms for each student
#'
#' @param .data A tbl. Must have an id column
#' @param ... Logical predicates defined in terms of variable in .data
filter_count_terms <- function(.data, .name, .predicate) {
  filter_args <- enquo(.predicate)
  .name <- enquo(.name)
  .data %>%
    dplyr::filter(!!filter_args) %>%
    dplyr::group_by(id) %>%
    dplyr::summarize(!!.name := max(term_count) - min(term_count))
}
