# L'idea in grandi linee è questa, poi magari qualcuno più bravo di me in inglese lo metterà giù per iscritto bene.
# Il problema dei retailer che operano in un mercato non-contrattuale (come ad esempio succede nelle compagnie telco)
# è che non si sa mai se il cliente ha churnato o semplicemente non è più attivo


# https://www.sciencedirect.com/science/article/abs/pii/S0377221703009184?casa_token=bIUWu7TOltcAAAAA:7zG0syKuS4jfZGvFoiddL-0sYAYdzvjtoID5GhJL3UOqcxg4KCLKYf-HjOiHP1iufPHIPUdi
# Dal punto di vista manageriale, non ha senso concentrarsi sui clienti fidelizzati, in quanto è pratica comune che 
# con la stragrande maggioranza dei clienti non fidelizzati il bilancio finale non sia profittevole. Il paper suggerisce di utilizzare
# due condizioni per verificare se un cliente è loyal o no:
# 1. La frequency è maggiore della media
# 2. Il rapporto tra la deviazione standard dell'interpurchase time e la media dell'interpuchase time sia sotto la media
# Infatti il paper suggerisce di non utilizzare la "monetary" in modo da evitare di escludere dei nuovi clienti che non
# hanno ancora speso tanti soldi ma che hanno tutto il potenziale per diventare dei spendaccioni della madonna
# Ci concentreremo quindi su due quarter, per semplicità prenderemo il Q2 e il Q3 del 2022

rm(list = ls())
library(pacman)
pacman::p_load("dplyr", "ggplot2", "lubridate", "readxl")
load("dataframe_finale.Rdata")
data.transaction.positive$date_ticket <- parse_date_time(data.transaction.positive$date_ticket, orders = c("%Y%m%d%H%M%S"))
data.q2 <- data.transaction.positive %>% filter(quarter(date_ticket, with_year = TRUE)=="2022.2")

# Prendo la data massima 
max(data.q2$date_ticket)

# Calcolo recency 
data.q2.freq <- data.q2 %>% 
  select(customer_id, date_ticket) %>% 
  group_by(customer_id) %>% 
  summarise(frequency = n_distinct(date_ticket))



# Calcolo regularity
data.q2.reg <- data.q2 %>%
  group_by(customer_id) %>% select(customer_id,date_ticket) %>%
  mutate(date_days=date(date_ticket))%>% arrange(date_days) %>%
  mutate(inter_purchase=as.numeric(difftime(date_days, lag(date_days,default=min(date_days), units="days")))) %>%
  filter(n_distinct(date(date_ticket))>=3) %>%
  summarise (regularity = sd(inter_purchase)/mean(inter_purchase))


df.loyalty <- left_join(data.q2.reg, data.q2.freq)
m.freq <- mean(df.loyalty$frequency)
m.reg <- mean(df.loyalty$regularity)
df.loyalty$loyalty <- case_when(df.loyalty$frequency >= mean(df.loyalty$frequency) & df.loyalty$regularity <= mean(df.loyalty$regularity) ~ 1,TRUE ~ 0)

paste("There are", sum(df.loyalty$loyalty), "behaviourally loyal clients.")
paste("That is", sprintf(sum(df.loyalty$loyalty)/n_distinct(df.loyalty$customer_id)*100, fmt = '%#.2f'), "% of total clients in this quarter.")


# Adesso troviamo i customers nel quarter successivo che si comportano come loyal e vediamo se quelli del quarter precedente hanno cambiato comportamento
data.q3 <- data.transaction.positive %>% filter(quarter(date_ticket, with_year = TRUE)=="2022.3")

# Prendo la data massima 
max(data.q3$date_ticket)

# Calcolo recency 
data.q3.rec <- data.q3 %>% 
  select(customer_id, date_ticket) %>% 
  group_by(customer_id) %>% 
  summarise(frequency = n_distinct(date_ticket))



# Calcolo regularity
data.q3.reg <- data.q3 %>%
  group_by(customer_id) %>% select(customer_id,date_ticket) %>%
  mutate(date_days=date(date_ticket))%>% arrange(date_days) %>%
  mutate(inter_purchase=as.numeric(difftime(date_days, lag(date_days,default=min(date_days), units="days")))) %>%
  filter(n_distinct(date(date_ticket))>=3) %>%
  summarise (regularity = sd(inter_purchase)/mean(inter_purchase))


df.loyalty.q3 <- left_join(data.q3.reg, data.q3.rec)
df.loyalty.q3$loyalty <- case_when(df.loyalty.q3$frequency >= mean(df.loyalty.q3$frequency) & df.loyalty.q3$regularity <= mean(df.loyalty.q3$regularity) ~ 1,TRUE ~ 0)

paste("There are", sum(df.loyalty.q3$loyalty), "behaviourally loyal clients.")
paste("That is", sprintf(sum(df.loyalty.q3$loyalty)/n_distinct(df.loyalty.q3$customer_id)*100, fmt = '%#.2f'), "% of total clients in this quarter.")

# Now create a dummy variable in the df.loyalty 0 if the customer is behaving the same in the next quarter, 
# 1 if the customer is behaving different by doing the left join, we discard the customers who did not buy anything
# in the Q3 and so we assume they churned.

df <- left_join(df.loyalty, df.loyalty.q3, by=c("customer_id"))
df$frequency.y <- NULL
df$regularity.y <- NULL
df[is.na(df$loyalty.y), "loyalty.y"] <- -1
df$target <- case_when(df$loyalty.x==1 & df$loyalty.y==0 ~ 1, df$loyalty.y==-1 ~ -1, TRUE ~ 0)

# Now drop the customer who churned
df.subsetted <- subset(df, df$target>-1)


paste("Customers who change their behavior:", sum(df.subsetted$target))
paste("Customers who churned in Q3:", nrow(df[df$target==-1,]))
paste("Customers loyal who churned in Q3:", nrow(df[df$target==-1 & df$loyalty.x==1,]))

# Setting up the dataset
df.subsetted <- subset(df.subsetted, df.subsetted$loyalty.x==1)
df.subsetted$loyalty.y <- NULL
df.subsetted$loyalty.x <- NULL

colnames(df.subsetted) <- c("customer_id", "reg", "freq", "target")   


# Le variabili che utilizzeremo sono
# Recency x
# Mean interpurchase x
# Std di interpurchase x
# Regularity x
# Frequency x
# rFrequency -> Frequency in rapporto alla lunghezza della relationship x
# Cluster del cliente x
# Cluster del cliente nel quarter precedente (se c'è) x
# Cluster del cliente nel quarter precedente-1 (se c'è) x
# Monetary x
# rmonetary x
# rMajorTip <- Percentage of shop visits with above-average spending x 
# NoCat <- number of categories purchased in the quarter x 
# rCat <- Aggregated relative spending in 15 different categories <- bello sbatti 
# LoR <- Number of days since first purchase x
# Average moment in time of shopping x
#  Std moment in time of shopping x

# Recupero le variabili che ho cancellato che sono un coglione
rec <- data.q2 %>% 
  group_by(customer_id) %>% 
  summarise(last_purchase=max(date_ticket), recency = as.numeric(difftime(ymd("2022-07-01"), last_purchase, units="days")))



reg <- data.q2 %>%
  group_by(customer_id) %>% select(customer_id,date_ticket) %>%
  mutate(date_days=date(date_ticket))%>% arrange(date_days) %>%
  mutate(inter_purchase=as.numeric(difftime(date_days, lag(date_days,default=min(date_days), units="days")))) %>%
  filter(n_distinct(date(date_ticket))>=3) %>%
  summarise (std_inter = sd(inter_purchase), mean_inter = mean(inter_purchase))

df.subsetted <- left_join(df.subsetted, rec, by=c("customer_id"))
df.subsetted <- left_join(df.subsetted, reg, by=c("customer_id"))
df.subsetted$last_purchase <- NULL

rm(list=c("rec", "reg", "m.freq", "m.reg"))


LoR <- data.q2 %>% 
  group_by(customer_id) %>% 
  summarise(first_purchase=min(date_ticket), LoR = as.numeric(difftime(ymd("2022-07-01"), first_purchase, units="days")))

timing <- data.q2 %>% 
  group_by(customer_id) %>% filter(n_distinct(date_ticket)>=3) %>% 
  summarise(mean_timing=mean(hour(date_ticket)), std_timing = sd(hour(date_ticket)))

df.subsetted <- left_join(df.subsetted, LoR, by=c("customer_id"))
df.subsetted <- left_join(df.subsetted, timing, by=c("customer_id"))
df.subsetted$first_purchase <- NULL

rm(list = c("LoR", "timing"))

df.subsetted$rFrequency <- df.subsetted$freq/df.subsetted$LoR


mon <- data.q2 %>% group_by(customer_id) %>% summarise(
  monetary= sum(price))

df.subsetted <- left_join(df.subsetted, mon, by=c("customer_id"))
df.subsetted$rMonetary <- df.subsetted$monetary/df.subsetted$LoR
rm(list=c("mon"))

products_DB <- read_xlsx("./products_macro_category.xlsx")
colnames(products_DB) <- c("ID", "description", "category")
data.q2 <- left_join(data.q2, products_DB, by = c("description"))

nocat <- data.q2 %>% group_by(customer_id) %>% summarise(NoCat= n_distinct(category))

df.subsetted <- left_join(df.subsetted, nocat, by=c("customer_id"))
rm(list=c("nocat"))

spending.above.mean <- data.q2 %>% group_by(customer_id, date_ticket) %>% summarise(total = sum(price))
spending.above.mean$above_mean <- case_when(spending.above.mean$total > mean(spending.above.mean$total)~1, TRUE~0)
spending.above.mean <- spending.above.mean %>% group_by(customer_id) %>%  summarise(percentage = sum(above_mean)/n_distinct(date_ticket))
colnames(spending.above.mean) <- c("customer_id", "pct_expense_above_average")
df.subsetted <- left_join(df.subsetted, spending.above.mean, by=c("customer_id"))

rm(list = c("spending.above.mean"))



# Loading dell'output della regfmr
load("2022q3.Rdata")
load("2022q2.Rdata")
load("2022q1.Rdata")

RegFMR.Q1 <- subset(RegFMR.Q1, select = c(1,15))
RegFMR.Q2 <- subset(RegFMR.Q2, select = c(1,15))
RegFMR.Q3 <- subset(RegFMR.Q3, select = c(1,15))

colnames(RegFMR.Q1) <- c("customer_id", "clustert-2")
colnames(RegFMR.Q2) <- c("customer_id", "clustert-1")
colnames(RegFMR.Q3) <- c("customer_id", "clustert")

df.subsetted <- left_join(df.subsetted, RegFMR.Q1, by = c("customer_id"))
df.subsetted <- left_join(df.subsetted, RegFMR.Q2, by = c("customer_id"))
df.subsetted <- left_join(df.subsetted, RegFMR.Q3, by = c("customer_id"))

save(df.subsetted, file = "dataframe_beh.Rdata")

# Food
food.exp <- data.q2 %>% group_by(customer_id, category) %>% summarise(total = sum(price)) %>% filter(category=="Food")
food.exp$category <- NULL
colnames(food.exp) <- c("customer_id", "food_tot")
df.subsetted <- left_join(df.subsetted, food.exp, by=c("customer_id"))
df.subsetted[is.na(df.subsetted$food_tot),"food_tot"] <- 0

# Drinks
drinks.exp <- data.q2 %>% group_by(customer_id, category) %>% summarise(total = sum(price)) %>% filter(category=="Drinks")
drinks.exp$category <- NULL
View(drinks.exp)
colnames(drinks.exp) <- c("customer_id", "drinks_tot")
df.subsetted <- left_join(df.subsetted, drinks.exp, by=c("customer_id"))
df.subsetted[is.na(df.subsetted$drinks_tot),"drinks_tot"] <- 0


# Meat
meat.exp <- data.q2 %>% group_by(customer_id, category) %>% summarise(total = sum(price)) %>% filter(category=="Meat")
meat.exp$category <- NULL
View(meat.exp)
colnames(meat.exp) <- c("customer_id", "meat_tot")
df.subsetted <- left_join(df.subsetted, meat.exp, by=c("customer_id"))
df.subsetted[is.na(df.subsetted$meat_tot),"meat_tot"] <- 0

# Fruits and vegetables
fruit.exp <- data.q2 %>% group_by(customer_id, category) %>% summarise(total = sum(price)) %>% filter(category=="Fruits and vegetables")
fruit.exp$category <- NULL
View(fruit.exp)
colnames(fruit.exp) <- c("customer_id", "fruits_vegs_tot")
df.subsetted <- left_join(df.subsetted, fruit.exp, by=c("customer_id"))
df.subsetted[is.na(df.subsetted$fruits_vegs_tot),"fruits_vegs_tot"] <- 0

save(df.subsetted, file = "dataframe_beh.Rdata")
