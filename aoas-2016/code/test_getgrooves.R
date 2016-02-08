library(x3pr)
library(x3prplus)
library(ggplot2)

for (file in dir("app/images/Hamby252_3DX3P1of2")) {
    mybullet <- get_bullet(file.path("app/images/Hamby252_3DX3P1of2", file), x = 100)
    x <- get_grooves(mybullet)
    cat(file, "\n")
    print(x$plot)
    #my.loess <- fit_loess(mybullet, x)
    #print(my.loess$resid)
    value <- readline("Press Enter to Continue, or q to Quit")
    if (value == "q") break;
}
