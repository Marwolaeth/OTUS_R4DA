library(httr)

# Удаление из текста HTML-тегов
strip_html <- function(s) {
  require(rvest)
  html_text(read_html(paste0('<body>', s, '</body>')))
}

# Списки значений переменных HH: названия категорий на русском ----
hh_dict_experience <- GET('https://api.hh.ru/dictionaries') %>%
  httr::content() |>
  getElement('experience') |>
  map_chr('name')

# Форматы чисел ----
format_int_rus <- scales::label_number(
  accuracy = 1,
  big.mark = ' ',
  decimal.mark = ','
)
format_thous_rus <- scales::label_number(
  accuracy = 1,
  big.mark = ' ',
  scale = 0.001,
  suffix = ' тыс.'
)
format_mln_rus <- scales::label_number(
  accuracy = 1,
  big.mark = ' ',
  scale = 0.000001,
  suffix = ' млн'
)
format_mln_float_rus <- scales::label_number(
  accuracy = .01,
  big.mark = ' ',
  decimal.mark = ',',
  scale = 0.000001,
  suffix = ' млн'
)
format_percent_rus <- scales::label_percent(
  accuracy = 1,
  big.mark = ' ',
  decimal.mark = ','
)
format_percent_float <- scales::label_percent(
  accuracy = .01,
  big.mark = '',
  decimal.mark = '.'
)
format_number_rus <- scales::label_number(
  accuracy = .01,
  big.mark = ' ',
  decimal.mark = ','
)
format_small_rus <- scales::label_number(
  accuracy = .0001,
  big.mark = ' ',
  decimal.mark = ','
)
format_battery_rus <- scales::label_number(
  accuracy = .1,
  big.mark = ' ',
  decimal.mark = ','
)
format_date_rus <- scales::label_date(
  format = '%d %B %Y',
  tz = 'Europe/Moscow'
)
format_time_rus <- scales::label_time(
  format = '%H:%M',
  tz = 'Europe/Moscow'
)

