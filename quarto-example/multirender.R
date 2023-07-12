library(palmerpenguins)

# Получаем доступные названия видов и островов
(all_species <- unique(penguins$species))
(all_islands <- unique(penguins$island ))

# Функция-обертка для рендеринга с параметрами
render_report <- function(species, island){
  quarto::quarto_render(
    input = 'parametrised-penguins.qmd',
    output_format = 'html',
    execute_params = list(species = species, island = island),
    output_file = tolower(glue::glue('penguin-report-{species}-{island}.html'))
  )
}

# Рендеринг
grid <- tidyr::expand_grid(all_species, all_islands)
purrr::walk2(grid$all_species, grid$all_islands, render_report)
