library(rixpress)

d1 <- rxp_r(mtcars_am, filter(mtcars, am == 1))
d2 <- rxp_r(mtcars_head, head(mtcars_am))
d3 <- rxp_r(mtcars_tail, tail(mtcars_head))
d4 <- rxp_r(mtcars_mpg, select(mtcars_tail, mpg))
doc <- rxp_quarto(page, "page.qmd")

rxp_list <- list(d1, d2, d3, d4, doc)

rixpress(rxp_list)

#plot_dag()
