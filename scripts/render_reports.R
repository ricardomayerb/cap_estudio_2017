library(rmarkdown)

my_encoding = "UTF-8"

render("./reports/heterog_espacios_politicas.Rmd",
       output_format = "pdf_document",
       encoding = my_encoding)




