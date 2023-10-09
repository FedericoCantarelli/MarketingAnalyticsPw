library(dplyr)
library(lubridate)
library(stringr)
library(arules)
library(arulesViz)
library(openxlsx)
rm(list = ls())


# Load Data
data.transaction <- read.csv("./data/dataset.csv", sep = ',', dec = '.')
data.transaction$date_ticket <- format(data.transaction$date_ticket, scientific = F)
data.transaction$customer_id <- format(data.transaction$customer_id, scientific = F)

# Format date
data.transaction$date_ticket <- parse_date_time(as.character(data.transaction$date_ticket), orders = c("%Y%m%d%H%M%S"))
View(data.transaction)


save(list = c("data.transaction"), file="./data/initial_dataset.Rdata")


# Unique identifier
data.transaction$ticket_id <- paste0(gsub("-", "", as.character(date(data.transaction$date_ticket))),
                                          "-", as.character(hour(data.transaction$date_ticket)),
                                          as.character(minute(data.transaction$date_ticket)),
                                          as.character(second(data.transaction$date_ticket)),
                                          "-", str_trim(data.transaction$store_id, side = c("both")),
                                          "-", str_trim(as.character(data.transaction$cashdesk_no), side = c("both")),
                                          "-", str_trim(data.transaction$customer_id, side=c("both")))

View(data.transaction)
length(unique(data.transaction$ticket_id))                                     

# Remove negative transactions
data.transaction.subsetted <- subset(data.transaction, data.transaction$quantity>0 & data.transaction$price>0)
length(unique(data.transaction.subsetted$ticket_id)) 
save(list = c("data.transaction.subsetted"), file="./data/dataset_id_e_no_neg.Rdata")

# Read dataset of product description
product = read.xlsx("./data/products_DB.xlsx")

# Left join
df.product = data.transaction.subsetted %>% left_join(product, by=c("product_code" = "Product.code"))
df.product[!complete.cases(df.product),]
save(list = c("df.product"), file="./data/dataset_prod_description.Rdata")


# Create transaction structure for market basket analysis
df.for.mba = df.product %>% group_by(ticket_id) %>% summarise(items = list(Description))
View(df.for.mba)

trans_list <- df.for.mba$items
names(trans_list) <- df.for.mba$ticket_id
market.Transaction <- transactions(trans_list)

# Save transaction structure
itemFrequencyPlot(market.Transaction, topN=20, type="absolute")
save(list = c("market.Transaction"), file="./data/arules_dataset.Rdata")



# Load
load("./arules_dataset.Rdata")

smin = 1000/length(market.Transaction) # Penso che un numero ragionevole di transazioni possa essere 
                                       # 1000 per poter considerare che una regola sia valida

inspect(head(market.Transaction, 10))

# plot frequent items
itemFrequencyPlot(market.Transaction, topN=35, type="absolute", main="Item Frequency") 

# A priori algorithm
ap_frequent_itemsets <- apriori(market.Transaction, parameter = list (supp = smin, conf = 0.1, maxlen=3))
ap_frequent_itemsets

# Inspect
inspect(head(ap_frequent_itemsets,10))

rules <- sort(ap_frequent_itemsets, by=c("lift", "confidence"), decreasing = TRUE) 
inspect(rules)
