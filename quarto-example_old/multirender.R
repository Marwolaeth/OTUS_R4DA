library(arrow)
library(dplyr)

# Загружаем и фильтруем данные, не помещая их в память
(jobs <- open_dataset('data/db/meta') |>
    distinct(job) |>
    pull(as_vector = TRUE)
)

# Функция-обертка для рендеринга с параметром
render_job_report <- function(job){
  quarto::quarto_render(
    input = "salaries.qmd",
    output_format = 'html',
    execute_params = list(job = job),
    output_file = glue::glue("{job}-report.html")
  )
}

# Рендеринг
purrr::walk(jobs, render_job_report)
