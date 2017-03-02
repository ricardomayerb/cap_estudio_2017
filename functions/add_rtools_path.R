

y <- Sys.getenv("PATH")
x <- paste0(y,";","C:\\Rtools\\bin;C:\\Rtools\\gcc-4.6.3\\bin")
Sys.setenv(PATH=x)

