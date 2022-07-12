#' Tabulates results from checks output from "compare_survey"
#'
#' This function creates flextable formatted tables from objects stored in full_compare list output by compare_survey.
#' @param item Number of check to tabulate.
#' @return Flextable object.
#' @export
#' @examples
#' \dontrun{tabulate_comparison(1)}

tabulate_comparison <- function(item){
  return(flextable(full_compare[[item]]) %>%
           align(align = "right", part = "body") %>%
           align(align = "center", part = "header") %>%
           fontsize(size=10, part="all") %>%
           font(fontname="Calibri", part="all") %>%
           bold(bold = TRUE, part="header") %>%
           bold(bold = TRUE, j=1) %>%
           set_table_properties(layout="autofit"))
}
