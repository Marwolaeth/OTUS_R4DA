# Форматы чисел ----
format_int_rus <- scales::label_number(
  accuracy = 1,
  big.mark = ' ',
  decimal.mark = ','
)

format_small_rus <- scales::label_number(
  accuracy = .001,
  big.mark = ' ',
  decimal.mark = ','
)
