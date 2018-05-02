rmarkdown::render('dataExploration.Rmd',
                  output_file = paste(
                    'report_', 
                    format(
                      Sys.Date(),
                      format="%Y%m%d"), 
                    '.pdf', 
                    sep=''))


