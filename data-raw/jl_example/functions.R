prepare_data <- function(laplace){
  laplace_df |>
    mutate(
      x_id = row_number()
    ) |>
    tidyr::pivot_longer(-x_id, names_to = "y_id", values_to = "z") |>
    mutate(
      y_id = gsub("x", "", y_id),
      y_id = as.numeric(y_id)
    )
}

make_gg <- function(laplace_long_df){
  laplace_long_df |>
    ggplot(aes(x = x_id, y = y_id, z = z)) +
    stat_summary_hex(fun = function(x) mean(x), bins = 45)  +
    scale_fill_viridis_c(option = 12) + 
    theme_void() +
    theme(legend.position = "none") +
    labs(subtitle = "hexagonal 2-d heatmap of laplacian matrix")
}

save_gg <- function(path, gg){
  ggsave("gg.png", gg)
}
