# запрос данных по рекламным кампаниям ----

acc_raw_data <- yadirGetReport(ReportType        = "CUSTOM_REPORT",
                               DateRangeType     = "CUSTOM_DATE",
                               DateFrom          = "2023-06-01",
                               DateTo            = Sys.Date() - 1,
                               FieldNames        = c("Date",
                                                     "AdNetworkType",
                                                     "CampaignName",
                                                     "CampaignType",
                                                     "AdGroupName",
                                                     "WeightedImpressions",
                                                     "Clicks",
                                                     "Sessions",
                                                     "Bounces",
                                                     "Cost",
                                                     "Conversions"),
                               Goals             = c(301649121,
                                                     301314275,
                                                     301313194,
                                                     301315472),
                               AttributionModels = c("FCCD", "AUTO"),
                               IncludeVAT        = "NO",
                               IncludeDiscount   = "NO",
                               AgencyAccount     = "platzkart.ru-e2",
                               Login             = "e-17332282",
                               TokenPath         = "direct_token")

# запись полученных данных на диск
vroom::vroom_write(x      = acc_raw_data, 
                   file   = "~/GitHub/BI/data/mc_yd_acc_raw_data.txt", 
                   delim  = ",",
                   append = FALSE)




# запрос данных по объявлениям ----
ads_raw_data <- yadirGetReport(ReportType        = "CUSTOM_REPORT",
                               DateRangeType     = "CUSTOM_DATE",
                               DateFrom          = "2023-06-01",
                               DateTo            = Sys.Date() - 1,
                               FieldNames        = c("Date",
                                                     "AdNetworkType",
                                                     "CampaignName",
                                                     "AdFormat",
                                                     "AdId",
                                                     "Slot",
                                                     "WeightedImpressions",
                                                     "Clicks",
                                                     "Sessions",
                                                     "Bounces",
                                                     "Cost",
                                                     "Conversions"),
                               Goals             = c(301649121,
                                                     301314275,
                                                     301313194,
                                                     301315472),
                               AttributionModels = c("FCCD", "AUTO"),
                               IncludeVAT        = "NO",
                               IncludeDiscount   = "NO",
                               AgencyAccount     = "platzkart.ru-e2",
                               Login             = "e-17332282",
                               TokenPath         = "direct_token")

# запись данных по объявлениям
vroom::vroom_write(x      = ads_raw_data, 
                   file   = "~/GitHub/BI/data/mc_yd_ads_raw_data.txt", 
                   delim  = ",",
                   append = FALSE)




# запрос текстов объявлений ----
## запрос метаданных по тексто-графическим кампаниям
mc_camp <- yadirGetCampaign(AgencyAccount     = "platzkart.ru-e2",
                            Login             = "e-17332282",
                            TokenPath         = "direct_token",
                            States            = "ON")

# запрос данных по объявлениям
mc_ads_raw <- yadirGetAds(CampaignIds   = mc_camp$Id,
                          AgencyAccount = "platzkart.ru-e2",
                          Login         = "e-17332282",
                          TokenPath     = "direct_token",
                          States        = "ON") 

# формируем таблицу 
mc_ads <- mc_ads_raw %>%
  mutate(Id = as.character(Id),
         TextAdHref = str_replace_all(.$TextAdHref,
                                      '(\\?.*)',
                                      '')) %>% 
  select(Id,
         TextAdTitle, 
         TextAdTitle2, 
         TextAdText, 
         TextAdHref)

# записываем данные по объявлениям
vroom::vroom_write(x      = mc_ads, 
                   file   = "~/GitHub/BI/data/mc_ads.txt", 
                   delim  = ",",
                   append = FALSE)




# запрос данных по типу клика ----
click_type_raw <- yadirGetReport(ReportType        = "CUSTOM_REPORT",
                                 DateRangeType     = "CUSTOM_DATE",
                                 DateFrom          = "2023-06-01",
                                 DateTo            = Sys.Date() - 1,
                                 FieldNames        = c("AdId",
                                                       "ClickType"),
                                 IncludeVAT        = "NO",
                                 IncludeDiscount   = "NO",
                                 AgencyAccount     = "platzkart.ru-e2",
                                 Login             = "e-17332282",
                                 TokenPath         = "direct_token")


# запись данных по типу клика
vroom::vroom_write(x      = click_type_raw, 
                   file   = "~/GitHub/BI/data/mc_click_type_raw.txt", 
                   delim  = ",",
                   append = FALSE)




# запрос данных по сегментам ---- 
segments_raw <- yadirGetReport(ReportType        = "CUSTOM_REPORT",
                               DateRangeType     = "CUSTOM_DATE",
                               DateFrom          = "2023-06-01",
                               DateTo            = Sys.Date() - 1,
                               FieldNames        = c("Date",
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
                                                     "Conversions"),
                               Goals             = c(301649121,
                                                     301314275,
                                                     301313194,
                                                     301315472),
                               AttributionModels = c("FCCD", "AUTO"),
                               IncludeVAT        = "NO",
                               IncludeDiscount   = "NO",
                               AgencyAccount     = "platzkart.ru-e2",
                               Login             = "e-17332282",
                               TokenPath         = "direct_token")

# подготовим данные к анализу
## задаем вектор имен
mc_names <- c("Calls_F", 
              "Calls_A", 
              "B24_F", 
              "B24_A", 
              "Forms_F", 
              "Forms_A", 
              "EMails_F", 
              "EMails_A")

## обработаем данные по целям
segments <- segments_raw %>%
  mutate(across(c(14:ncol(.)), ~ ifelse(. == "--", 0, .)),
         across(c(14:ncol(.)), as.numeric)) %>%
  rename_at(vars(14:ncol(.)), ~ mc_names) %>%
  mutate(GoalsFC = rowSums(across(ends_with("_F"), ~abs(.x))),
         GoalsLC = rowSums(across(ends_with("_A"), ~abs(.x))))



# запись данных по сегментам
vroom::vroom_write(x      = segments, 
                   file   = "~/GitHub/BI/data/mc_segments.txt", 
                   delim  = ",",
                   append = FALSE)



