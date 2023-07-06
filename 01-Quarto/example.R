library(arrow)
library(dplyr)

(jobs <- open_dataset('data/db/meta') |>
    distinct(job) |>
    pull(as_vector = TRUE)
)

render_job_report <- function(job){
  quarto::quarto_render(
    input = "salaries.qmd",
    output_format = 'html',
    execute_params = list(job = job),
    output_file = glue::glue("{job}-report.html")
  )
}

purrr::walk(jobs, render_job_report)

(files <- list.files(pattern = '*report.html'))
rstudioapi::viewer(files[1])
