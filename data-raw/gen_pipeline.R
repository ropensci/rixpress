library(rixpress)

d1 <- drv_r(mtcars_am, filter(mtcars, am == 1))
d2 <- drv_r(mtcars_head, head(mtcars_am))
doc <- drv_quarto(page, "page.qmd")

drv_list <- list(d1, d2, doc)

rixpress(drv_list)
