library(rixpress)

d1 <- drv_r(mtcars_am, filter(mtcars, am == 1))
d2 <- drv_r(mtcars_head, head(mtcars_am))
d3 <- drv_r(mtcars_tail, tail(mtcars_head))
d4 <- drv_r(mtcars_mpg, select(mtcars_tail, mpg))
doc <- drv_quarto(page, "page.qmd")

drv_list <- list(d1, d2, d3, d4, doc)

rixpress(drv_list)

#plot_dag()
