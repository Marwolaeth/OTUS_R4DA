# МОТИВАЦИЯ ----

## ЦЕЛИ РАЗВЕДЫВАТЕЛЬНОГО АНАЛИЗА ----
# 1. Оценка объема и структуры данных.
# 2. Оценка качества данных и их соответствия задаче (garbage in → garbage out):
#   а) правильный формат данных (Tidy Data);
#   б) правильный формат переменных;
#   в) наличие пропущенных или бессмысленных значений.
# 3. Анализ распределения признаков и выявление выбросов.
# 4. Поиск связей между признаками и определение важности признаков для целевой переменной.
# 5. Подготовка данных для построения модели и принятия решений.

# Получение представления о структуре, качестве и характеристиках данных позволяет понять, какие методы анализа и обработки данных будут наиболее эффективными и подходящими для конкретной задачи.
# 
# Кроме того, разведывательный анализ данных позволяет выявить ошибки и неточности в данных, что может повлиять на результаты анализа.
# 
# Наличие автоматизированного конвейера загрузки, обработки и анализа данных не исключает необходимости проведения разведывательного анализа данных. Конвейер может обрабатывать данные, но без предварительного их изучения вы можете упустить важные детали, которые могут повлиять на результаты анализа.

## ЗАГРУЗКА БИБЛИОТЕК И ДАННЫХ ----
if (grepl('rtools', Sys.getenv("PATH"))) {
  tp <- 'source'
  opts = '--byte-compile'
} else {
  tp <- getOption("pkgType")
  opts = NULL
}
required_packages <- c(
  'dplyr',              # Операции с данными в стиле SQL
  'ggplot2',            # Визуализация
  'forcats',            # Ради набора данных `gss_cat`
  'skimr',              # Качество данных
  'dlookr',             # Качество данных, кореляция с откликом и EDA-отчет
  'naniar',             # Для визуализации пропущенных значений
  'DataExplorer',       # Интерактивный EDA-отчет в RStudio
  'ppsr',               # Оценка предсказательной силы независимых переменных
  'correlationfunnel',  # «Воронка корреляции» с зависимой переменной
  'explore',            # Интерактивный EDA-отчет в Shiny
  'car',                # Ради функции Фактора инфляции дисперсии
  'dataxray',           # Подробный EDA
  'rattle'              # Ради набора данных `wine`
)
(missing_packages <- setdiff(required_packages, installed.packages()))
install.packages(missing_packages, type = tp, INSTALL_opts = opts)
if (!require(dataxray)) devtools::install_github('agstn/dataxray')
data('wine', package = 'rattle')
data('mpg', package = 'ggplot2')
data('gss_cat', package = 'forcats')

## ЭТАПЫ И БИБЛИОТЕКИ ----
### I. Проверка структуры данных: base и dplyr ----
library(dplyr)
library(ggplot2)
glimpse(gss_cat)
summary(gss_cat) # Для перемнных-факторов должна быть таблица значений
levels(gss_cat$rincome)
is.ordered(gss_cat$rincome)

### II. Проверка качества данных: skimr, naniar ----
##### skimr ----
library(skimr)
# Распределения переменных и пропущенные значения все-в-одном
skim(gss_cat)

# `Скимминг` по группам
gss_cat |>
  group_by(year) |>
  skim()

##### naniar ----
library(naniar)
# Пропущенные значения по пременным
miss_var_summary(gss_cat)
gg_miss_var(gss_cat)

# Пропущенные значения по наблюдениям
miss_case_summary(gss_cat)
gg_miss_case(gss_cat) # И что это?

# Визуализация пропущенных значения
## Есть ли паттерны по наблюдениям?
vis_miss(gss_cat,facet = year)
## Есть ли паттерны по переменным?
gg_miss_upset(gss_cat)

### III. Полный статический EDA: dlookr, DataExplorer ----
##### DataExplorer ----
library(DataExplorer)
# Общее описание
introduce(gss_cat)
plot_intro(gss_cat)
plot_density(gss_cat)
plot_bar(gss_cat)

# Пропущенные значения
plot_missing(gss_cat)
profile_missing(gss_cat)

# Многомерный EDA
plot_correlation(gss_cat, maxcat = 15)
(income_levels <- levels(gss_cat$rincome))
gss_cat_income <- gss_cat |>
  # Оставим только наблюдения с известным уровнем дохода
  filter(grepl('\\d', rincome)) |>
  # Преобразуем в порядковый фактор
  mutate(
    rincome = droplevels(rincome),
    rincome = factor(rincome, ordered = TRUE, levels = rev(income_levels[4:15]))
  )
plot_correlation(gss_cat_income)

# Автоматический отчет
gss_cat_income |>
  select(-denom) |>
  create_report(
    output_file  = 'gss_survey_data_profile_report',
    output_dir   = 'output',
    y            = 'rincome',
    report_title = 'EDA Report - GSS Demographic Survey'
  )
# Пример выводов по результатам EDA:
# Может, стоит создать переменную вроде «крайность», которая будет коррелировать
# с уровнем дохода?

##### dlookr ----
library(dlookr)
# Общее описание и пропущенные значения
diagnose(gss_cat)
diagnose_numeric(gss_cat)
diagnose_category(gss_cat)
gss_cat |> # No
  group_by(year) |>
  diagnose()

# Статистические характеристики
describe(gss_cat)
gss_cat |>
  group_by(year) |>
  describe()

# Выбросы
gss_cat |>
  diagnose_outlier() |>
  filter(outliers_cnt > 0) |>
  pull(variables) |>
  plot_outlier(gss_cat, .dots = _) # Так странно накодили, чтобы показать поэтапно

# Нормальность количественных переменных
normality(gss_cat)
gss_cat |>                          # В нашем случае бессмысленно
  mutate(tvhours_ln = log(tvhours), tvhours_sqrt = sqrt(tvhours)) |>
  group_by(year) |>
  normality(tvhours, tvhours_ln, tvhours_sqrt)

plot_normality(gss_cat, tvhours)

# Многомерный EDA
gss_cat |>
  # group_by(year) |>
  correlate(method = 'pearson') |>
  plot()

# Связь с откликом
pal <- colorspace::choose_palette(n=12)
pal(12)
gss_cat_income_tv <- gss_cat_income |>
  target_by(rincome) |>
  relate(tvhours)
plot(gss_cat_income_tv) +
  scale_colour_discrete(type = pal(12))
summary(gss_cat_income_tv)

# Автоматические отчеты
diagnose_web_report(gss_cat)
gss_cat_income |> eda_web_report(
  target     = 'rincome',
  title      = 'EDA Report - GSS Demographic Survey',
  logo_img   = 'images/owls_R.svg',
  output_dir = '.',
  method     = 'spearman' # Не работает (((
)
# см. https://choonghyunryu.github.io/dlookr/

### IV. Интерактивный EDA: dataxray, explore ----
##### dataxray ----
library(dataxray)
# Интерактивный EDA в RStudio
gss_cat_income |>
  make_xray() |>
  view_xray()
gss_cat_income |>
  make_xray(by = 'year') |>
  view_xray(by = 'year')

# Автоматический отчет
gss_cat_income |>
  report_xray(
    data_name = 'GSS Demographic Survey',
    study     = 'EDA Report',
    loc       = 'output'
  )

##### explore ----
library(explore)
# Обычное описание качества данных
describe_all(gss_cat)
describe_num(gss_cat, tvhours)

# Связь с откликом
explore_all(gss_cat, target = rincome)

# Интерактивный EDA в браузере
explore(gss_cat)
explore(wine)

### V. Рейтинг предикторов: ppsr, correlationfunnel ----
##### ppsr ----
library(ppsr)

### VI. Проверка на мультиколлинеарность (Фактор инфляции дисперсии) ----