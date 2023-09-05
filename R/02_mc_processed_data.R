# Скрипт для обработки данных проекта МастерСлух

# Рекламные кампании ---------------------------------

# загрузка данных сырых данных
acc_raw <- vroom::vroom(file = "~/GitHub/BI/data/mc_yd_acc_raw.txt")


# подготовим данные к анализу
## задаем вектор имен
mc_names <- c(
  "Calls_F",
  "Calls_A",
  "B24_F",
  "B24_A",
  "Forms_F",
  "Forms_A",
  "EMails_F",
  "EMails_A"
)

## обработаем данные по целям
account <- acc_raw  |> 
  mutate(
    across(c(11:ncol(acc_raw)), ~ ifelse(. == "--", 0, .)),
    across(c(11:ncol(acc_raw)), as.numeric)
  ) |> 
  rename_at(vars(11:ncol(acc_raw)), ~ mc_names) |> 
  mutate(
    GoalsFC = rowSums(across(ends_with("_F"), ~ abs(.x))),
    GoalsLC = rowSums(across(ends_with("_A"), ~ abs(.x)))
  )


# запись обработанных данных на диск
vroom::vroom_write(
  x = account,
  file = "~/GitHub/BI/data/processed/mc_yd_account.txt",
  delim = ",",
  append = FALSE
  )



# Объявления -----------------------------------------

# загрузка данных
ads_raw <- vroom::vroom(file = "~/GitHub/BI/data/mc_yd_ads_raw.txt")


# подготовим данные к анализу
## задаем вектор имен
mc_names <- c(
  "Calls_F",
  "Calls_A",
  "B24_F",
  "B24_A",
  "Forms_F",
  "Forms_A",
  "EMails_F",
  "EMails_A"
)

## обработаем данные по целям
ads <- ads_raw %>%
  mutate(
    across(c(12:ncol(.)), ~ ifelse(. == "--", 0, .)),
    across(c(12:ncol(.)), as.numeric)
  ) %>%
  rename_at(vars(12:ncol(.)), ~mc_names) %>%
  mutate(
    GoalsFC = rowSums(across(ends_with("_F"), ~ abs(.x))),
    GoalsLC = rowSums(across(ends_with("_A"), ~ abs(.x)))
  )

# запись обработанных данных на диск
vroom::vroom_write(
  x = ads,
  file = "~/GitHub/BI/data/processed/mc_yd_ads.txt",
  delim = ",",
  append = FALSE
)


# Площадки ----------------------------------------------------------------

# загрузка данных
places_raw <- vroom::vroom(file = "~/GitHub/BI/data/raw/mc_yd_places_raw.txt")


# подготовим данные к анализу
## задаем вектор имен
names <- c(
  "Calls_F",
  "Calls_A",
  "B24_F",
  "B24_A",
  "Forms_F",
  "Forms_A",
  "EMails_F",
  "EMails_A"
  )

## обработаем данные по целям
places <- places_raw |> 
  mutate(
    across(c(14:ncol(places_raw)), ~ ifelse(. == "--", 0, .)),
    across(c(14:ncol(places_raw)), as.numeric)
  )  |> 
  rename_at(vars(14:ncol(places_raw)), ~names) %>%
  mutate(
    GoalsFC = rowSums(across(ends_with("_F"), ~ abs(.x))),
    GoalsLC = rowSums(across(ends_with("_A"), ~ abs(.x)))
  )

# запись обработанных данных на диск
vroom::vroom_write(
  x = places,
  file = "~/GitHub/BI/data/processed/mc_yd_places.txt",
  delim = ",",
  append = FALSE
)
