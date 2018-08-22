library(tidyverse)
library(readxl)

data1 <- read_excel("Project_ImportExport_Updated_20_01.xlsx", sheet = 2, col_names = TRUE, skip = 0)
data1 <- as_tibble(data1)
data1 <- rename(data1, lead_creation_date = "Lead Creation Date_")
data1 <- rename(data1, lead_creation_date_year = "Lead Creation Date_Year")
data1 <- rename(data1, lead_creation_date_month = "Lead Creation Date_Month")
data1 <- rename(data1, lead_creation_date_day = "Lead Creation Date_Day")
data1 <- rename(data1, lead_id = "LEAD_ID")
data1 <- rename(data1, lead_status = "Lead Status")
data1 <- rename(data1, relo_status = "Relo Status")
data1 <- rename(data1, trade_number = "Trade #")
data1 <- rename(data1, customer_name = "Customer Name")
data1 <- rename(data1, export_country = "Export Country")
data1 <- rename(data1, import_country = "Import Country")
data1 <- rename(data1, lead_country = "Lead Country")
data1 <- rename(data1, lead_owner = "Lead Owner")
data1 <- rename(data1, lead_owner_office = "Lead Owner Office")
data1 <- rename(data1, is_web_lead = "Is Web Lead")
data1 <- rename(data1, source = "Source")
data1 <- rename(data1, unqualify_reason = "Unqualify Reason")
data1 <- rename(data1, conversion_date = "Conversion Date_")
data1 <- rename(data1, likely_service_date = "Likely Service Date_")
data1 <- rename(data1, date_hour_minute = "dateHourMinute_")
data1 <- rename(data1, transaction_id = "transactionId")
data1 <- rename(data1, channel_grouping = "channelGrouping")
data1 <- rename(data1, user_type = "userType")
data1 <- rename(data1, device_category = "deviceCategory")
data1 <- rename(data1, website = "Website")
data1 <- rename(data1, sum_total_revenue_amount_usd = "Sum_Total Revenue Amount\r\n (USD)")
data1 <- rename(data1, invoice_last_month = "Invoice Last Month")
data1 <- rename(data1, invoice_last_year = "Invoice Last Year")



table1 <- table(data1$relo_status, data1$lead_creation_date)
table2 <- table(data1$relo_status, data1$lead_creation_date_year)
table3 <- table(data1$relo_status, data1$lead_creation_date_month)
table4 <- table(data1$relo_status, data1$lead_creation_date_day)
table5 <- table(data1$relo_status, data1$customer_name)
table6 <- table(data1$relo_status, data1$export_country)
table7 <- table(data1$relo_status, data1$import_country)
table8 <- table(data1$relo_status, data1$lead_country)
table9 <- table(data1$relo_status, data1$lead_owner)
table10 <- table(data1$relo_status, data1$lead_owner_office)
table11 <- table(data1$relo_status, data1$is_web_lead)
table12 <- table(data1$relo_status, data1$source)
table13 <- table(data1$relo_status, data1$channel_grouping)
table14<- table(data1$relo_status, data1$user_type)
table15 <- table(data1$relo_status, data1$device_category)
table16 <- table(data1$relo_status, data1$website)

chisq.test(table1) # p > 0.05 not statistically significant
chisq.test(table2) # p < 0.05 statistically significant
chisq.test(table3) # p < 0.05 statistically significant
chisq.test(table4) # p < 0.05 statistically significant
chisq.test(table5) # p > 0.05 not statistically significant
chisq.test(table6) # p < 0.05 statistically significant
chisq.test(table7) # p < 0.05 statistically significant
chisq.test(table8) # p < 0.05 statistically significant
chisq.test(table9) # p < 0.05 statistically significant
chisq.test(table10) # p < 0.05 statistically significant
chisq.test(table11) # p < 0.05 statistically significant
chisq.test(table12) # p < 0.05 statistically significant
chisq.test(table13) # p < 0.05 statistically significant
chisq.test(table14) # p < 0.05 statistically significant
chisq.test(table15) # p < 0.05 statistically significant
chisq.test(table16) # p < 0.05 statistically significant


cramersV(table2)  #0.1221245
cramersV(table3)  #0.08506758
cramersV(table4)  #0.01533316
cramersV(table6)  #0.2176837
cramersV(table7)  #0.1472994
cramersV(table8)  #0.2159479
cramersV(table9)  #0.3471392
cramersV(table10) #0.2429278
cramersV(table11) #0.5328308
cramersV(table12) #0.1164199
cramersV(table13) #0.07199563
cramersV(table14) #0.05566483
cramersV(table15) #0.02232178
cramersV(table16) #0.25219


data1_closed_deals <- filter(data1, data1$relo_status == "Booked" | data1$relo_status=="-" | data1$relo_status == "Cancelled" | data1$relo_status == "Lost")
data1_closed_deals$status[data1_closed_deals$relo_status=="Cancelled" | data1_closed_deals$relo_status == "Lost" | data1_closed_deals$relo_status == "-"] = "NotBooked"
data1_closed_deals$status[data1_closed_deals$relo_status=="Booked"] = "Booked"
mod1 <- multinom(data1_closed_deals$status~data1_closed_deals$lead_owner)

data1 %>% filter(lead_creation_date_year < 2015) %>% ggplot(aes(lead_creation_date)) + geom_freqpoly(binwidth = 86400)
data1 %>% filter(lead_creation_date_year > 2014 & lead_creation_date_year < 2016) %>% ggplot(aes(lead_creation_date)) + geom_freqpoly(binwidth = 86400)
data1 %>% filter(lead_creation_date_year > 2015) %>% ggplot(aes(lead_creation_date)) + geom_freqpoly(binwidth = 86400)
