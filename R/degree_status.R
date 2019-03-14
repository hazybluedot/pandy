add_degree_status <- function(data, matriculation_major_col) {
  major_col <- enquo(matriculation_major_col)
  data %>%
    # hasDegree is true if the degree major equals the matriculation major and it was conferred after the first term
    mutate(hasCOEDegree = !is.na(degree_term) & degree_term > coe_matriculation_term & (is_eng_major(degree1) | is_eng_major(degree2)),
           hasDegree = hasCOEDegree &
             ((!is.na(degree1) & degree1 == as.character(!! major_col)) | (!is.na(degree2) & degree2 == as.character(!! major_col)))
    )
}
