library(pacman)
pacman::p_load("qcc", "lubridate", "dplyr")

load("dataframe_finale.Rdata")
data.transaction.positive$date_ticket <-
  parse_date_time(as.character(data.transaction.positive$date_ticket),
                  orders = c("%Y%m%d%H%M%S"))
head(data.transaction.positive, 10)
interpurchase = data.transaction.positive %>%
  group_by(customer_id) %>% select(customer_id,date_ticket) %>%
  mutate(date_days=date(date_ticket))%>% arrange(date_days) %>%
  mutate(interpurchase_days=as.numeric(date_days - lag(date_days,default=min(date_days)))) %>% filter(interpurchase_days>0)



selected.cust <- interpurchase %>% select(customer_id, date_days, interpurchase_days) %>% filter(customer_id=="6966998838904") %>% arrange(date_days)
selected.cust.last.30 <- tail(selected.cust, 30)




# I - MR chart
i.chart <- qcc(selected.cust.last.30$interpurchase_days, type="xbar.one", plot = TRUE)
my.xmr.raw.r <- matrix(cbind(selected.cust.last.30$interpurchase_days[1:length(selected.cust.last.30$interpurchase_days)-1], selected.cust.last.30$interpurchase_days[2:length(selected.cust.last.30$interpurchase_days)]), ncol=2)
r.chart <- qcc(my.xmr.raw.r, type="R", plot = TRUE, main="Titolo")

