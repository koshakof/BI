# Скрипт для запроса данных для проекта МастерСлух

library(ryandexdirect)
library(tidyverse)


# Постоянные переменные ---------------------------------------------------

# вектор имён
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
# для запроса данных API Директ
token <- "~/GitHub/BI/direct_token"
agency_account <- "platzkart.ru-e2"
client_login <- "e-17332282"


# путь для записи обработанных данных
file_path <- "~/GitHub/BI/clients/mc/data/"



# Рекламные кампании ------------------------------------------------------
# запрос данных
acc_raw <- yadirGetReport(
  ReportType = "CUSTOM_REPORT",
  DateRangeType = "CUSTOM_DATE",
  DateFrom = "2023-06-01",
  DateTo = Sys.Date() - 1,
  FieldNames = c(
    "Date",
    "AdNetworkType",
    "CampaignName",
    "CampaignType",
    "AdGroupName",
    "WeightedImpressions",
    "Clicks",
    "Sessions",
    "Bounces",
    "Cost",
    "Conversions"
  ),
  Goals = c(
    301649121,
    301314275,
    301313194,
    301315472
  ),
  AttributionModels = c("FCCD", "AUTO"),
  IncludeVAT = "NO",
  IncludeDiscount = "NO",
  AgencyAccount = agency_account,
  Login = client_login,
  TokenPath = token
)


# обработаем данные по целям
account <- acc_raw  |> 
  mutate(
    across(c(11:ncol(acc_raw)), ~ ifelse(. == "--", 0, .)),
    across(c(11:ncol(acc_raw)), as.numeric)
  ) |> 
  rename_at(vars(11:ncol(acc_raw)), ~ names) |> 
  mutate(
    GoalsFC = rowSums(across(ends_with("_F"), ~ abs(.x))),
    GoalsLC = rowSums(across(ends_with("_A"), ~ abs(.x)))
  )


# запись обработанных данных на диск
data.table::fwrite(
  x = account, 
  file = paste0(file_path, "account.txt")
  )


# Объявления --------------------------------------------------------------
# запрос данных
ads_raw <- yadirGetReport(
  ReportType = "CUSTOM_REPORT",
  DateRangeType = "CUSTOM_DATE",
  DateFrom = "2023-06-01",
  DateTo = Sys.Date() - 1,
  FieldNames = c(
    "Date",
    "AdNetworkType",
    "CampaignName",
    "AdGroupName",
    "AdFormat",
    "AdId",
    "Slot",
    "WeightedImpressions",
    "Clicks",
    "Sessions",
    "Bounces",
    "Cost",
    "Conversions"
  ),
  Goals = c(
    301649121,
    301314275,
    301313194,
    301315472
    ),
  AttributionModels = c("FCCD", "AUTO"),
  IncludeVAT = "NO",
  IncludeDiscount = "NO",
  AgencyAccount = agency_account,
  Login = client_login,
  TokenPath = token
  )

# подготовим данные к анализу
## обработаем данные по целям
ads_temp <- ads_raw |>
  mutate(
    across(c(13:ncol(ads_raw)), ~ ifelse(. == "--", 0, .)),
    across(c(13:ncol(ads_raw)), as.numeric)
  ) |> 
  rename_at(vars(13:ncol(ads_raw)), ~names) |> 
  mutate(
    GoalsFC = rowSums(across(ends_with("_F"), ~ abs(.x))),
    GoalsLC = rowSums(across(ends_with("_A"), ~ abs(.x)))
  )



# запрос текстов объявлений
## запрос метаданных по тексто-графическим кампаниям
meta <- yadirGetCampaign(
  AgencyAccount = agency_account,
  Login = client_login,
  TokenPath = token
  )

# запрос данных по объявлениям
text <- yadirGetAds(
  CampaignIds = meta$Id,
  AgencyAccount = agency_account,
  Login = client_login,
  TokenPath = token
)

# формируем таблицу
ads_text <- tibble(
  text |> 
    mutate(
      Id         = as.character(Id),
      TextAdHref = str_replace_all(TextAdHref, "(\\?.*)", "")
      ) |> 
    select(
      Id,
      TextAdTitle,
      TextAdTitle2,
      TextAdText,
      TextAdHref
      ))

# объеденяем данные
ads <- left_join(x = ads_temp, y = ads_text, by = c("AdId" = "Id"))


# записываем данные по объявлениям
data.table::fwrite(
  x = ads, 
  file = paste0(file_path, "ads.txt")
  )




# Тип клика ---------------------------------------------------------------
# запрос данных
click_type_raw <- yadirGetReport(
  ReportType = "CUSTOM_REPORT",
  DateRangeType = "CUSTOM_DATE",
  DateFrom = "2023-06-01",
  DateTo = Sys.Date() - 1,
  FieldNames = c(
    "AdNetworkType",
    "AdId",
    "ClickType"
    ),
  IncludeVAT = "NO",
  IncludeDiscount = "NO",
  AgencyAccount = agency_account,
  Login = client_login,
  TokenPath = token
)


# запись данных по типу клика
data.table::fwrite(
  x = click_type_raw, 
  file = paste0(file_path, "click_type_raw.txt")
  )



# Сегменты ----------------------------------------------------------------
# запрос данных
segments_raw <- yadirGetReport(
  ReportType = "CUSTOM_REPORT",
  DateRangeType = "CUSTOM_DATE",
  DateFrom = "2023-06-01",
  DateTo = Sys.Date() - 1,
  FieldNames = c(
    "Date",
    "AdNetworkType",
    "CampaignName",
    "LocationOfPresenceName",
    "Device",
    "Gender",
    "Age",
    "IncomeGrade",
    "WeightedImpressions",
    "Clicks",
    "Sessions",
    "Bounces",
    "Cost",
    "Conversions"
  ),
  Goals = c(
    301649121,
    301314275,
    301313194,
    301315472
  ),
  AttributionModels = c("FCCD", "AUTO"),
  IncludeVAT = "NO",
  IncludeDiscount = "NO",
  AgencyAccount = agency_account,
  Login = client_login,
  TokenPath = token
)

# обработаем данные по целям
segments <- segments_raw %>%
  mutate(
    across(c(14:ncol(.)), ~ ifelse(. == "--", 0, .)),
    across(c(14:ncol(.)), as.numeric)
  ) %>%
  rename_at(vars(14:ncol(.)), ~names) %>%
  mutate(
    GoalsFC = rowSums(across(ends_with("_F"), ~ abs(.x))),
    GoalsLC = rowSums(across(ends_with("_A"), ~ abs(.x)))
  )



# запись данных по сегментам
data.table::fwrite(
  x = segments, 
  file = paste0(file_path, "segments.txt")
  )


# Площадки ----------------------------------------------------------------
# запрос данных
places_raw <- yadirGetReport(
  ReportType = "CUSTOM_REPORT",
  DateRangeType = "CUSTOM_DATE",
  DateFrom = "2023-06-01",
  DateTo = Sys.Date() - 1,
  FieldNames = c(
    "Date",
    "Device",
    "CampaignId",
    "CampaignName",
    "AdGroupName",
    "AdFormat",
    "CriterionType",
    "Placement",
    "Cost",
    "Impressions",
    "Clicks",
    "Sessions",
    "Bounces",
    "Conversions"
    ),
  Goals = c(
    301649121,
    301314275,
    301313194,
    301315472
    ),
  AttributionModels = c("FCCD", "AUTO"),
  IncludeVAT = "NO",
  IncludeDiscount = "NO",
  AgencyAccount = agency_account,
  Login = client_login,
  TokenPath = token
)

# исправляем данные в столбцах по достигнутым целям
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
data.table::fwrite(
  x = places, 
  file = paste0(file_path, "places.txt")
)





# Таргеты и ключевые слова ------------------------------------------------
# запрос данных
query_raw <- yadirGetReport(
  ReportType = "SEARCH_QUERY_PERFORMANCE_REPORT",
  DateRangeType = "CUSTOM_DATE",
  DateFrom = "2023-06-01",
  DateTo = Sys.Date() - 1,
  FieldNames = c(
    "Date",
    "CampaignId",
    "CampaignName",
    "AdGroupName",
    "CriterionType",
    "TargetingCategory",
    "Criterion",
    "Query",
    "Cost",
    "WeightedImpressions",
    "Clicks",
    "Conversions"
    ),
  Goals = c(
    301649121,
    301314275,
    301313194,
    301315472
    ),
  AttributionModels = c("FCCD", "AUTO"),
  IncludeVAT = "NO",
  IncludeDiscount = "NO",
  AgencyAccount = agency_account,
  Login = client_login,
  TokenPath = token
)

# исправляем данные в столбцах по достигнутым целям
query <- query_raw |> 
  mutate(
    across(c(12:ncol(places_raw)), ~ ifelse(. == "--", 0, .)),
    across(c(12:ncol(places_raw)), as.numeric)
  )  |> 
  rename_at(vars(12:ncol(places_raw)), ~names) %>%
  mutate(
    GoalsFC = rowSums(across(ends_with("_F"), ~ abs(.x))),
    GoalsLC = rowSums(across(ends_with("_A"), ~ abs(.x)))
  )


# запись обработанных данных на диск
data.table::fwrite(
  x = query, 
  file = paste0(file_path, "query.txt")
  )

