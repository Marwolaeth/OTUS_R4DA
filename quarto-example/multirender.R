library(palmerpenguins)
library(dplyr)

# Функция-обертка для рендеринга с параметрами
render_report <- function(species, island, format = 'html'){
  format <- match.arg(format, c('html', 'pdf', 'docx'))
  quarto::quarto_render(
    input = 'parametrised-penguins.qmd',
    output_format = format,
    execute_params = list(species = species, island = island),
    output_file = tolower(
      glue::glue('penguin-report-{species}-{island}.{format}')
    )
  )
}

# Получаем доступные комбинации видов и островов
grid <- penguins |>
  count(species, island)

# Рендеринг
purrr::walk2(grid$species, grid$island, render_report)
