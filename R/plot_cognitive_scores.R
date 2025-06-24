plot_cognitive_scores <- function(data, year) {
  # Plots the distribution of cognitive scores over a range of years.
  # Converts the data from wide to long format and generates histograms for each wave.
  # Arguments:
  #   - data: The dataset containing cognitive scores.
  #   - year: The starting year for the range of cognitive scores to plot.
  # Returns:
  #   - A ggplot object showing the distribution of cognitive scores across waves.
  # Plotting dementia transitions
  data |>
    select(ID, any_of(paste0("cogtot27_imp", year:2022))) |>
    na.omit() |>
    tidyr::pivot_longer(cols = !ID,
                        names_to = "Wave",
                        values_to = "Score") |>
    mutate(Wave = as.double(stringr::str_replace(Wave, "cogtot27_imp", ""))) |>
    ggplot(aes(x = Score)) +
    geom_histogram(fill = "#56B4E9",
                   colour = "black",
                   alpha = 0.5) +
    scale_x_continuous(breaks = seq(0, 27, by = 3)) +
    labs(
      title = paste0("Distribution of cognitive scores ", year, "- 2022"),
      x = "Cognitive Score", y = "") +
    facet_wrap( ~ Wave)
}