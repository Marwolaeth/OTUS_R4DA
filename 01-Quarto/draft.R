devtools::install_github("ropensci/tif", build = TRUE)
install.packages("duckdb", type = 'source', INSTALL_opts = '--byte-compile')


library(tif)
# library(stringi)
library(ggplot2)

# vacancies <- readRDS('data/vacancies.RDS') |>
#   filter(salary.currency == "RUB") |>
#   mutate(
#     salary = if_else(is.na(salary.from), salary.to, salary.from),
#     salary.gross = if_else(is.na(salary.gross), TRUE, salary.gross),
#     salary = if_else(
#       salary.gross,
#       salary * .87,
#       salary
#     ),
#     description = map_chr(description, strip_html),
#     experience = factor(experience, levels = hh_dict_experience, ordered = TRUE)
#   ) |>
#   filter(salary >= 1e4, salary <= 5e5)
# 
# vacancies <- vacancies |>
#   mutate(
#     job_id = stri_trans_general(job, 'Russian-Latin/BGN') |>
#       stri_replace_all_fixed(' ', '_')
#   )
# (text_features <- c('description', 'key_skills', 'specializations'))
# vacancies_text <- vacancies |>
#   select(id, job_id, job, all_of(text_features))
# vacancies_meta <- vacancies |>
#   select(id, job_id, job, everything(), -all_of(text_features))
# write_dataset(
#   vacancies_meta, 'data/db/meta',
#   partitioning = 'job_id',
#   hive_style = FALSE,
#   basename_template = "meta-{i}.parquet"
# )
# write_dataset(
#   vacancies_text, 'data/db/text',
#   partitioning = 'job_id',
#   hive_style = FALSE,
#   basename_template = "text-{i}.parquet"
# )
# rm(vacancies)
df_meta <- open_dataset('data/db/meta', partitioning = 'job_id') |>
  filter(job == params$job)

df_meta |>
  collect() |>
  ggplot(aes(x = experience, y = salary)) +
  geom_jitter(width = .25, alpha = .25) +
  stat_summary(fun.data = 'median_hilow', colour = 'red3') +
  scale_x_discrete('Требуемый опыт работы') +
  scale_y_continuous('Предлагаемая заработная плата', labels = format_int_rus) +
  ggtitle(sprintf('Рынок труда: %s', params$job)) +
  ggthemes::theme_tufte()

spacy_initialize(model = 'ru_core_news_lg')

df_description <- open_dataset('data/db/text', partitioning = 'job_id') |>
  # filter(job == params$job) |>
  select(doc_id = id, text = description)

tif_is_corpus_df(collect(df_description))
(unused_pos <- c(
  'SPACE', 'PUNCT', 'ADP', 'CCONJ', 'SCONJ'
))
desc_lemma <- spacy_parse(df_description |> collect(), entity = FALSE)
save(desc_lemma, file = 'data/description_lemmartized_raw.RData')
desc_lemmatized <- desc_lemma |>
  filter(!(pos %in% unused_pos)) |>
  group_by(doc_id) |>
  summarise(description_lemmatized = paste(lemma, collapse = ' '))
spacy_finalize()

df_text <- open_dataset('data/db/text', partitioning = 'job_id') |> collect()
df_text <- right_join(
  df_text, desc_lemmatized, by = c('id' = 'doc_id')
)
write_dataset(
  df_text, 'data/db/text',
  partitioning = 'job_id',
  hive_style = FALSE,
  basename_template = "text-{i}.parquet"
)

install.packages('quarto', type = 'source', INSTALL_opts = '--byte-compile')
library(quarto)
quarto_render(
  'salaries.qmd',
  output_format = 'pdf',
  execute_params = list(job = 'Бухгалтер')
)
