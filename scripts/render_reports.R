library(rmarkdown)

my_encoding = "UTF-8"

# pre_path <- "~/GitHub/cap_estudio_2017/"
# pre_path <- 'V:/USR/RMAYER/cw/cap_estudio_2017/'

# render("./reports/heterog_espacios_politicas.Rmd",
#        output_format = "pdf_document",
#        encoding = my_encoding)



# render("./reports/heterog_espacios_politicas.Rmd",
#        output_format = "word_document",
#        encoding = my_encoding)


# render("./reports/mapa_post.Rmd",
#       output_format = "pdf_document",
#       encoding = my_encoding)

render("./reports/mapa_post.Rmd",
       output_format = "html_document",
       encoding = my_encoding)

# print(getwd())
# shell.exec(paste0(pre_path,"reports/mapa_post.html"))
# browseURL(paste0(pre_path,"reports/mapa_post.html"))
browseURL("./reports/mapa_post.html")
