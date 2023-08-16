#theme_set(theme_minimal())
ci_colors <- c(
  "primaryPurple"        = "#191e3f",
  "secondaryPurple"       = "#4c2a9d",
  "purple3" = "#4c4a9d",
  "green"     = "#05d350",
  "dkgreen"     = "#036a49",
  "brown" = "#6a4903",
  "white"      = "#FFFFFF")


ci_pal <- function(
  primary = 'primaryPurple',
  other ="dkgreen",
  direction = 1
) {
  stopifnot(primary %in% names(ci_colors))

  function(n) {
    if (n > 6) warning("Coleridge Color Palette only has 7 colors.")

    if (n == 2) {
      other <- if (!other %in% names(ci_colors)) {
        other
      } else {
        ci_colors[other]
      }
      color_list <- c(other, ci_colors[primary])
    } else {
      color_list <- ci_colors[1:n]
    }

    color_list <- unname(unlist(color_list))
    if (direction >= 0) color_list else rev(color_list)
  }
}


scale_colour_ci <- function(
  primary = 'primaryPurple',
  other = "dkgreen",
  direction = 1,
  ...
) {
  ggplot2::discrete_scale(
    "colour", "ci",
    ci_pal(primary, other, direction),
    ...
  )
}

scale_color_carsey <- scale_colour_carsey
