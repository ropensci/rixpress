library(rixpress)

d1 <- deriv_r(mtcars_am, filter(mtcars, am == 1))
d2 <- deriv_r(mtcars_head, head(mtcars_am))
doc <- deriv_quarto("page.qmd")

deriv_list <- list(d1, d2, doc)

rixpress(deriv_list)
