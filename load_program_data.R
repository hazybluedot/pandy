library(racadia)
library(dplyr)
library(midfieldr)
library(readr)

sets = list(
  cip_filter(series = c("^14XX", "^1401")) %>% cip_label(program = "GE"),
  cip_filter(series = "^1435") %>% cip_label(program = "IE"),
  cip_filter(series = "^1408") %>% cip_label(program = "CE"),
  cip_filter(series = "^1419") %>% cip_label(program = "ME"),
  cip_filter(series = "^1407") %>% cip_label(program = "ChE"),
  cip_filter(series = "^1410") %>% cip_label(program = "EE"),
  cip_filter(series = c("110101", "110701")) %>%cip_label("CS")
)

institutions = c("Clemson University", "University of Colorado", "University of Florida", "NC A&T University", "NC State University", "University of Oklahoma", "Utah State University")

program_group <- do.call(bind_rows, sets)

data_path <- '~/ENGE/data/MIDFIELD'

students_colspec <- cols(
  institution = col_character(),
  term_enter = col_double(),
  sex = col_factor(c("Male", "Female", "Other")),
  race = col_character(),
  us_citizen = col_factor(c("Y", "N")),
  home_zip = col_character(),
  veteran = col_factor(c("Y", "N")),
  Age = col_double(),
  sat_math = col_double(),
  sat_verbal = col_double(),
  act_comp = col_double(),
  act_math = col_double(),
  act_english = col_double(),
  act_reading = col_double(),
  act_science = col_double(),
  hs_gpa = col_double(),
  hs_rank = col_double(),
  hs_size = col_double(),
  transfer = col_factor(c("Y", "N")),
  cip6 = col_character(),
  sat = col_double(),
  ID = col_character()
)

midfield_term <- function(x) {
  seasons = c("Fall" = 1, "Winter" = 2, "Spring" = 3, "Summer" = 4, "Summer 1" = 5, "Summer 2" = 6)
  year <- stringr::str_sub(x, 1, 4)
  term_id <- as.integer(stringr::str_sub(x, 5, 6))
  term <- names(seasons)[term_id]
  as_term(paste(term, year))  
}

read_csv(file.path(data_path, 'midfield_students_20180906.csv'), col_types = students_colspec) %>%
  mutate(term_enter = midfield_term(term_enter)) ->
  midfield_students

read_csv(file.path(data_path, 'midfield_terms_20180906.csv')) %>%
  mutate(term = midfield_term(term)) ->
  midfield_terms

read_csv(file.path(data_path, 'midfield_courses_20180906.csv')) %>%
  mutate(term_course = midfield_term(term_course)) ->
  midfield_courses

read_csv(file.path(data_path, 'midfield_degrees_20180906.csv')) %>%
  mutate(term_degree = midfield_term(term_degree)) ->
  midfield_degrees


## Generate program data

midfield_degrees %>%
  group_by(institution) %>%
  summarize(max_term_degree = as_term(max(term_degree))) ->
  max_term_degree

grad_window_fct <- function(degree_data) {
  function(.data, termvar, years) {
    termvar <- enquo(termvar)
    .data %>%
      left_join(degree_data, by = "institution") %>%
      filter(max_term_degree - !! termvar >= years - 0.33)
  }
}

grad_window <- grad_window_fct(max_term_degree)

midfield_students %>%
  filter(institution %in% institutions) %>%
  #mutate(term_enter = midfield_term(term_enter)) %>%
  #left_join(max_term_degree, by = "institution") %>%
  #filter(max_term_degree - term_enter >= 6 - 0.33) %>%
  mutate(urm = !race %in% c("White", "Asian")) %>%
  left_join(program_group %>% select(cip6, cip4name, program), by = "cip6") %>%
  left_join(midfield_terms %>%
              filter(institution %in% institutions) %>%
              left_join(program_group %>% select(cip6, cip4name, program), by = "cip6") %>%
              filter(program != "GE") %>%
              group_by(ID) %>%
              summarize(matriculation_major = dplyr::first(program, order_by = term)), by = "ID") ->
  program_students

program_students %>%
  group_by(institution) %>%
  summarize(modemat = deepr::Mode(program, na.rm = TRUE)) %>%
  mutate(admit = as_factor(if_else(modemat == "GE", "FYE", "Direct"))) %>%
  select(-modemat) ->
  institution_type

midfield_terms %>%
  semi_join(program_students, by = "ID") %>%
  #mutate(term = midfield_term(term)) %>%
  enrolled_time(ID, term) ->
  program_student_time

midfield_terms %>%
  semi_join(program_students, by = "ID") %>%
  #mutate(term = midfield_term(term)) %>%
  left_join(program_student_time, by = c("ID", "term")) ->
  program_terms

midfield_degrees %>%
  semi_join(program_students, by = "ID") %>%
  #mutate(term_degree = midfield_term(term_degree)) %>%
  left_join(program_group %>% select(cip6, cip4name, program), by = "cip6") ->
  program_degrees

save(program_students, program_terms, program_degrees, institution_type, program_group, grad_window, file = "program_midfield.rda")
