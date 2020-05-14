#' Produces an HTML report in the current working directory to evaluate regressivity in the supplied jurisdiction
#'
#' @param ratios a dataframe which has been preprocessed by \code{\link{reformat_data}}
#' @param jurisdiction_name the name of the jurisdiction being analyzed
#' @param output_dir optional, specify a folder to save the output in
#' @param diagnostic optional, if TRUE includes additional sections evaluating data quality
#'
#' @return produces a file named `jurisdiction_name.html` in the working directory

#' @export
make_report <-
  function(ratios, jurisdiction_name, output_dir="", diagnostic=FALSE) {

  if (output_dir == ""){
    file_loc <- getwd()
  } else {
    file_loc <- output_dir
  }

  output_loc <- paste0(file_loc, "/", jurisdiction_name, ".html")

  rmarkdown::render(system.file("rmd", "report.Rmd", package = "cmfproperty"),
                    params = list(title = jurisdiction_name,
                                  inputdata = ratios,
                                  diagnostic = diagnostic),
                    output_file = output_loc)
  print(paste0("Report created at ", output_loc))
}
