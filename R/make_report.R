#' Produces an HTML report to evaluate regressivity in the supplied jurisdiction
#'
#' @param ratios a dataframe which has been preprocessed by \code{\link{reformat_data}}
#' @param jurisdiction_name the name of the jurisdiction being analyzed
#'
#' @return produces a file named `jurisdiction_name.html` in the working directory

#' @export
make_report <-
  function(ratios, jurisdiction_name) {

  output_loc <- paste0(getwd(), "/", jurisdiction_name, ".html")

  rmarkdown::render(system.file("rmd", "report.Rmd", package = "cmfproperty"),
                    params = list(title = jurisdiction_name,
                                  inputdata = ratios),
                    output_file = output_loc)
  print(paste0("Report created at ", output_loc))
}
