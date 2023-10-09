                        #####################################################
                        ################## RegFMR Analysis ###################
                        #####################################################

# Loading the data
rm(list=ls())
load("dataframe_finale.Rdata")
df = data.transaction.positive
   
# Loading the necessary packages                                             
pacman::p_load(zoo, dplyr, lubridate, ggplot2, plotly,scales, ggplot2, ggalluvial)

# Checking the data frame
str(df) # Looks fine
colnames(df)[10] <- "Basket" # Changing the Key's name into Basket

# Splitting the dataframe in quarters

quartered_df <- split(df, as.yearqtr(df$date_ticket)) 
period <- quartered_df$`2022 Q3` # Select the quarter you prefer

# Regularity-Frequency-Monetary

RegFMR.foo <- function(period){
  
  if (months(max(period$date_ticket)) == "dicembre"){
    first_of_next_month = ceiling_date(max(period$date_ticket), unit = "years")
  } else {
    first_of_next_month = ceiling_date(max(period$date_ticket), unit = "month")
    }  # Setting the day from which we need to calculate the recency
  
  RegFM <- period %>%
    group_by(customer_id) %>%
    filter(as.numeric(max(date_ticket) - min(date_ticket))> 0) %>%
    summarise(
      recency = as.numeric(difftime(first_of_next_month,max(date_ticket),units = "days")),
      frequency = length(unique(date_ticket)),
      monetary = sum(price)
    )

# Comment on how each parameter has been calculated: 
# Moentary and Frequency = We have decided to take the "absolute" values since we are already considering a quite narrow timeframe (a quarter)
# Thus, narrowing even further by dividing both measures by the actual time a customer has computed
# her first and last purchase would have distorted the results.
# Recency = simple difference between last purchase and first day of the next month
  
# Comments for the following lines:
# We calculated the regularity as the interpurchase time' s coefficient of variation
# While the recency_mod consists of our NEW METRIC, which is used to cluster our customers in terms of their behaviours
# and tries to take into consideration the problems that would have stemmed by using the 
# the simple comparison between recency and the product between regularity and mean inter-purchase time  
# recency_mod = regularity * log(recency/mean(inter-purchase time))
  
  Regularity <- period %>%
    group_by(customer_id) %>%
    filter(as.numeric(max(date_ticket)-min(date_ticket))>0) %>%
    ungroup() %>%
    group_by(customer_id, date_ticket) %>%
    summarise(date_ticket = unique(date_ticket)) %>%
    # mutate(date_days = as.double(date_ticket))%>%
    mutate(days_from_prev = as.numeric(difftime(date_ticket,min(date_ticket),units="days"))) %>%
    group_by(customer_id) %>%
    summarise(regularity =sd(days_from_prev)/mean(days_from_prev),
              std = sd(days_from_prev), # in order to have it in the dataframe in order to understand the distributions
              m = mean(days_from_prev), # in order to have it in the dataframe in order to understand the distributions
              recency_mod = (as.numeric(difftime(first_of_next_month,max(date_ticket),units = "days")))/m

    )
  
# Attributing Temperature: Explanation is below in the code.
# Here we simply calculate it and add it to the final data frame
  Regularity <- Regularity %>%
    mutate(temperature = case_when(
      recency_mod <= quantile(recency_mod,1/4)~"Super Hot",
      recency_mod > quantile(recency_mod,1/4) & recency_mod <= quantile(recency_mod,1/2)~"Hot",
      recency_mod > quantile(recency_mod,1/2) & recency_mod <= quantile(recency_mod,3/4)~"Cold",
      TRUE ~ "Super Cold"),
           reliability= case_when(
             regularity <= quantile(regularity,1/3) ~ "Creature of Habit",
             regularity > quantile(regularity,1/3) & regularity <= quantile(regularity,2/3) ~ "Regular",
             TRUE ~ "Unstable"
           ),

    )
  Regularity$recency.state <- paste(Regularity$temperature, "--",Regularity$reliability)
  RegFMR <- na.omit(left_join(RegFM, Regularity, by = "customer_id"))
  return(RegFMR)
  ggplot(RegFMR, aes(x=frequency)) + geom_histogram()
}

period$date_ticket <- parse_date_time(period$date_ticket, orders = c("%Y%m%d%H%M%S"))
RegFMR <- RegFMR.foo(period)


# Explanation of the outliers' removal
# Plotting to understand the behavior and looking for outliers

ggplot(RegFMR, aes(x=frequency)) + geom_histogram()
ggplot(RegFMR, aes(x=monetary)) + geom_histogram()
ggplot(RegFMR, aes(x=recency)) + geom_histogram()

ggplot(RegFMR, aes(x=frequency)) + geom_boxplot()
ggplot(RegFMR, aes(x=monetary)) + geom_boxplot()
ggplot(RegFMR, aes(x=recency)) + geom_boxplot()

quantile(RegFMR$frequency, probs = seq(0, 1, 0.05))
quantile(RegFMR$monetary, probs = seq(0, 1, 0.05))
quantile(RegFMR$recency, probs = seq(0, 1, 0.05))

ggplot(RegFMR, aes(x=frequency)) + geom_density()
ggplot(RegFMR, aes(x=monetary)) + geom_density()

# Cutting off outliers
# From what has been seen from the previous graphs, and considering 
# which type of shops we are looking at the following 
# data customers will be considered as outliers: 
# If they have visited the shops more than 67 days in 90 disposable (ca. 3/4)
# and have spent more than 10 ??? for purchase
# These threshold have been taken into account given the mean behavior of our customers overall
# Note that we had to take into account the last quarter that is made of just one
# month...

ggplot(RegFMR, aes(x=monetary, y=frequency)) + geom_point() + geom_hline(yintercept = 90*3/4, col = "red", linetype = "dotdash") + geom_vline(xintercept = 10*90*3/4,  col = "red", linetype = "dotdash")

if (months(max(period$date_ticket)) == "ottobre"){
  
  RegFMR_Outliers <- RegFMR %>% 
    filter(frequency > 30*3/4 | monetary > 30*10*3/4 | regularity > 10)
  
  RegFMR <- RegFMR %>% 
    filter(frequency <= 30*3/4 & monetary <= 30*10*3/4 & regularity <= 10)

  }else {

    RegFMR_Outliers <- RegFMR %>% 
      filter(frequency > 90*3/4 | monetary > 90*10*3/4| regularity > 10)
    
    RegFMR <- RegFMR %>% 
      filter(frequency <= 90*3/4 & monetary <= 90*10*3/4 & regularity <= 10)
}  

summary(RegFMR)
# Definition of the threshold for the Q3
avg.frequency = mean(RegFMR$frequency)
avg.monetary = mean(RegFMR$monetary)
avg.regularity = mean(RegFMR$regularity)
frequency.th.1 <- quantile(RegFMR$frequency, probs = 0.25)
frequency.th.2 <- quantile(RegFMR$frequency, probs = 0.5)
frequency.th.3 <- quantile(RegFMR$frequency, probs = 0.75)
monetary.th.1 <- quantile(RegFMR$monetary, probs = 0.25)
monetary.th.2 <- quantile(RegFMR$monetary, probs = 0.5)
monetary.th.3 <- quantile(RegFMR$monetary, probs = 0.75)
regularity.th.1 <- quantile(RegFMR$regularity, probs = 0.25)
regularity.th.2 <- quantile(RegFMR$regularity, probs = 0.5)
regularity.th.3 <- quantile(RegFMR$regularity, probs = 0.75)

# Function that will be used below to filter the data sets according to the customers
# that are present in Q3 2022, our reference
filter_customers <- function(period){
  period.filter <- period %>%
    filter(customer_id %in% customers_ref)
  return (period.filter)
}

# Score Attribution:

scoring <- function(RegFMR,freq_mod =1,mon_mod=1,reg_mod=1){

  RegFMR <- RegFMR %>% 
    mutate (Reg=case_when(regularity <= regularity.th.1*reg_mod ~ 4, regularity > regularity.th.1*reg_mod & regularity <= regularity.th.2*reg_mod ~ 3, regularity > regularity.th.2*reg_mod & regularity <= regularity.th.3*reg_mod ~ 2, regularity > regularity.th.3*reg_mod ~ 1 )) %>%
    mutate (F=case_when(frequency <= frequency.th.1*freq_mod ~ 1, frequency > frequency.th.1*freq_mod & frequency <= frequency.th.2*freq_mod ~ 2, frequency > frequency.th.2*freq_mod & frequency <= frequency.th.3*freq_mod ~ 3, frequency > frequency.th.3*freq_mod ~ 4 )) %>%
    mutate (M=case_when(monetary <= monetary.th.1*mon_mod ~ 1, monetary > monetary.th.1*mon_mod & monetary <= monetary.th.2*mon_mod ~ 2, monetary> monetary.th.2*mon_mod & monetary <=monetary.th.3*mon_mod ~ 3, monetary > monetary.th.3*mon_mod ~ 4 ))
  
  RegFMR <- RegFMR %>% 
    mutate(RegFM = Reg * 100 + F * 10 + M)
  
  RegFMR$RegFM = as.factor(RegFMR$RegFM)
  
  RegFM_1 = RegFMR %>% select(customer_id,RegFM) %>% filter(RegFM==444 | RegFM==134 | RegFM==143 | RegFM==133 | RegFM==144)
  RegFM_1 = RegFM_1 %>% mutate (cluster = case_when(RegFM==444 ~ "Champions", TRUE ~ "Needing Attention" ))
  RegFM_1$cluster = as.factor(RegFM_1$cluster)
  print(table(RegFM_1$cluster))
  
  return(RegFMR)
}


# Creating functions for centroid

primary.up <- function (q,d){
  for (i in 2:4){
    Primary[q,i] <- mean(unlist(d[1+i]))
  }
  return (Primary)
}

secondary.up <- function (q,d){
  for (i in 2:4){
    Secondary[q,i] <- mean(unlist(d[1+i]))
  }
  return (Secondary)
}

non.shoppers.up <- function (q,d){
  for (i in 2:4){
    Non.Shopper[q,i] <- mean(unlist(d[1+i]))
  }
  return (Non.Shopper)
}

# Centroids dataframe creation
quarters <- c("2020 Q4","2021 Q1","2021 Q2","2021 Q3","2021 Q4","2022 Q1","2022 Q2","2022 Q3")
Reg<- c(1,1,1,1,1,1,1,1)
F<- c(1,1,1,1,1,1,1,1)
M<- c(1,1,1,1,1,1,1,1)
Primary <- data.frame(quarters,F,M,Reg)
Secondary <- data.frame(quarters,F,M,Reg)
Non.Shopper <- data.frame(quarters,F,M,Reg)

# Q3 2022
head(as.data.frame(table(ceiling(RegFMR$regularity))),10)
hist(RegFMR$regularity)

RegFMR.Q3 <- scoring(RegFMR)
customers_ref <- RegFMR.Q3$customer_id
RegFMR.Q3.Primary <- RegFMR.Q3 %>% filter(M == 4 & F == 4)
RegFMR.Q3.Secondary <- RegFMR.Q3 %>% filter(M == 3 | M == 4) %>% filter(!(M == 4 & F == 4))
RegFMR.Q3.Non.Shopper <- RegFMR.Q3 %>% filter(M==2 | M==1)
Primary <- primary.up(8,d = RegFMR.Q3.Primary)
Secondary <- secondary.up(8,d = RegFMR.Q3.Secondary)
Non.Shopper <- non.shoppers.up(8,RegFMR.Q3.Non.Shopper)

# Creating a column in the RegFMR that tells which cluster the given client 
# is part of

RegFMR.Q3 <- RegFMR.Q3 %>% mutate(
  Cluster = case_when(M == 4 & F == 4 ~ "Primary", (M == 3 & F <= 4) | (M == 4 & F < 4) ~ "Secondary", TRUE ~ "Non_Shopper")
)

# Questo serve a me
save(RegFMR.Q3, file = "2022q3.Rdata")

# Computation of RegFMR Q2 2022
period.Q2 <- filter_customers(quartered_df$`2022 Q2`) # Select the quarter you prefer
length(unique(period.Q2$customer_id))

period.Q2$date_ticket <- parse_date_time(period.Q2$date_ticket, orders = c("%Y%m%d%H%M%S"))
RegFMR.Q2 <- RegFMR.foo(period.Q2)
avg.frequency.Q2 <- mean(RegFMR.Q2$frequency)
avg.monetary.Q2 <- mean(RegFMR.Q2$monetary)
avg.regularity.Q2 <- mean(RegFMR.Q2$regularity)
RegFMR.Q2<- scoring(RegFMR.Q2,freq_mod =avg.frequency.Q2/avg.frequency,mon_mod = avg.monetary.Q2/avg.monetary, reg_mod = avg.regularity.Q2/avg.regularity )
RegFMR.Q2.Primary <- RegFMR.Q2 %>% filter(M == 4 & F == 4)
RegFMR.Q2.Secondary <- RegFMR.Q2 %>% filter(M == 3 | M ==4) %>% filter(!(M == 4 & F == 4))
RegFMR.Q2.Non.Shopper <- RegFMR.Q2 %>% filter(M==2 | M==1)
Primary <- primary.up(7,d = RegFMR.Q2.Primary)
Secondary <- secondary.up(7,d = RegFMR.Q2.Secondary)
Non.Shopper <- non.shoppers.up(7,RegFMR.Q2.Non.Shopper)

# Creating a column in the RegFMR that tells which cluster the given client 
# is part of

RegFMR.Q2 <- RegFMR.Q2 %>% mutate(
  Cluster = case_when(M == 4 & F == 4 ~ "Primary", (M == 3 & F <= 4) | (M == 4 & F < 4) ~ "Secondary", TRUE ~ "Non_Shopper")
)

# Questo serve a me
save(RegFMR.Q2, file = "2022q2.Rdata")


ggplot(RegFMR.Q2, aes(x=Reg)) + geom_bar()
ggplot(RegFMR.Q2, aes(x=F)) + geom_bar()
ggplot(RegFMR.Q2, aes(x=M)) + geom_bar()

 # Computation of RegFMR Q1 2022
period.Q1 <- filter_customers(quartered_df$`2022 Q1`) # Select the quarter you prefer
period.Q1$date_ticket <- parse_date_time(period.Q1$date_ticket, orders = c("%Y%m%d%H%M%S"))
RegFMR.Q1 <- RegFMR.foo(period.Q1)
length(unique(period.Q1$customer_id))
avg.frequency.Q1 <- mean(RegFMR.Q1$frequency)
avg.monetary.Q1 <- mean(RegFMR.Q1$monetary)
avg.regularity.Q1 <- mean(RegFMR.Q1$regularity)
RegFMR.Q1<- scoring(RegFMR.Q1,freq_mod =avg.frequency.Q1/avg.frequency,mon_mod = avg.monetary.Q1/avg.monetary, reg_mod = avg.regularity.Q1/avg.regularity )
RegFMR.Q1.Primary <- RegFMR.Q1 %>% filter(M == 4 & F == 4)
RegFMR.Q1.Secondary <- RegFMR.Q1 %>% filter(M == 3 | M ==4) %>% filter(!(M == 4 & F == 4))
RegFMR.Q1.Non.Shopper <- RegFMR.Q1 %>% filter(M==2 | M==1)
Primary <- primary.up(6,d = RegFMR.Q1.Primary)
Secondary <- secondary.up(6,d = RegFMR.Q1.Secondary)
Non.Shopper <- non.shoppers.up(6,RegFMR.Q1.Non.Shopper)

# Creating a column in the RegFMR that tells which cluster the given client 
# is part of

RegFMR.Q1 <- RegFMR.Q1 %>% mutate(
  Cluster = case_when(M == 4 & F == 4 ~ "Primary", (M == 3 & F <= 4) | (M == 4 & F < 4) ~ "Secondary", TRUE ~ "Non_Shopper")
)

# Questo serve a me 
save(RegFMR.Q1, file = "2022q1.Rdata")

ggplot(RegFMR.Q1, aes(x=Reg)) + geom_bar()
ggplot(RegFMR.Q1, aes(x=F)) + geom_bar()
ggplot(RegFMR.Q1, aes(x=M)) + geom_bar()

# Computation of RegFMR Q4 2021
period.Q41 <- filter_customers(quartered_df$`2021 Q4`) # Select the quarter you prefer
length(unique(period.Q41$customer_id))
period.Q41$date_ticket <- parse_date_time(period.Q41$date_ticket, orders = c("%Y%m%d%H%M%S"))
RegFMR.Q41 <- RegFMR.foo(period.Q41)
avg.frequency.Q41 <- mean(RegFMR.Q41$frequency)
avg.monetary.Q41 <- mean(RegFMR.Q41$monetary)
avg.regularity.Q41 <- mean(RegFMR.Q41$regularity)

RegFMR.Q41<- scoring(RegFMR.Q41,freq_mod =avg.frequency.Q41/avg.frequency,mon_mod = avg.monetary.Q41/avg.monetary, reg_mod = avg.regularity.Q41/avg.regularity )
RegFMR.Q41.Primary <- RegFMR.Q41 %>% filter(M == 4 & F == 4)
RegFMR.Q41.Secondary <- RegFMR.Q41 %>% filter(M == 3 | M ==4) %>% filter(!(M == 4 & F == 4))
RegFMR.Q41.Non.Shopper <- RegFMR.Q41 %>% filter(M==2 | M==1)
Primary <- primary.up(5,d = RegFMR.Q41.Primary)
Secondary <- secondary.up(5,d = RegFMR.Q41.Secondary)
Non.Shopper <- non.shoppers.up(5,RegFMR.Q41.Non.Shopper)

# Creating a column in the RegFMR that tells which cluster the given client 
# is part of

RegFMR.Q41 <- RegFMR.Q41 %>% mutate(
  Cluster = case_when(M == 4 & F == 4 ~ "Primary", (M == 3 & F <= 4) | (M == 4 & F < 4) ~ "Secondary", TRUE ~ "Non_Shopper")
)

ggplot(RegFMR.Q41, aes(x=Reg)) + geom_bar()
ggplot(RegFMR.Q41, aes(x=F)) + geom_bar()
ggplot(RegFMR.Q41, aes(x=M)) + geom_bar()

# Computation of RegFMR Q3 2021
period.Q31 <- filter_customers(quartered_df$`2021 Q3`) # Select the quarter you prefer
length(unique(period.Q31$customer_id))
period.Q31$date_ticket <- parse_date_time(period.Q31$date_ticket, orders = c("%Y%m%d%H%M%S"))
RegFMR.Q31 <- RegFMR.foo(period.Q31)
avg.frequency.Q31 <- mean(RegFMR.Q31$frequency)
avg.monetary.Q31 <- mean(RegFMR.Q31$monetary)
avg.regularity.Q31 <- mean(RegFMR.Q31$regularity)

RegFMR.Q31<- scoring(RegFMR.Q31,freq_mod =avg.frequency.Q31/avg.frequency,mon_mod = avg.monetary.Q31/avg.monetary, reg_mod = avg.regularity.Q31/avg.regularity )
RegFMR.Q31.Primary <- RegFMR.Q31 %>% filter(M == 4 & F == 4)
RegFMR.Q31.Secondary <- RegFMR.Q31 %>% filter(M == 3 | M ==4) %>% filter(!(M == 4 & F == 4))
RegFMR.Q31.Non.Shopper <- RegFMR.Q31 %>% filter(M==2 | M==1)
Primary <- primary.up(4,d = RegFMR.Q31.Primary)
Secondary <- secondary.up(4,d = RegFMR.Q31.Secondary)
Non.Shopper <- non.shoppers.up(4,RegFMR.Q31.Non.Shopper)

# Creating a column in the RegFMR that tells which cluster the given client 
# is part of

RegFMR.Q31 <- RegFMR.Q31 %>% mutate(
  Cluster = case_when(M == 4 & F == 4 ~ "Primary", (M == 3 & F <= 4) | (M == 4 & F < 4) ~ "Secondary", TRUE ~ "Non_Shopper")
)

ggplot(RegFMR.Q31, aes(x=Reg)) + geom_bar()
ggplot(RegFMR.Q31, aes(x=F)) + geom_bar()
ggplot(RegFMR.Q31, aes(x=M)) + geom_bar()

# Computation of RegFMR Q2 2021
period.Q21 <- filter_customers(quartered_df$`2021 Q2`) # Select the quarter you prefer
length(unique(period.Q21$customer_id))
period.Q21$date_ticket <- parse_date_time(period.Q21$date_ticket, orders = c("%Y%m%d%H%M%S"))
RegFMR.Q21 <- RegFMR.foo(period.Q21)
avg.frequency.Q21 <- mean(RegFMR.Q21$frequency)
avg.monetary.Q21 <- mean(RegFMR.Q21$monetary)
avg.regularity.Q21 <- mean(RegFMR.Q21$regularity)

RegFMR.Q21<- scoring(RegFMR.Q21,freq_mod =avg.frequency.Q21/avg.frequency,mon_mod = avg.monetary.Q21/avg.monetary, reg_mod = avg.regularity.Q21/avg.regularity )
RegFMR.Q21.Primary <- RegFMR.Q21 %>% filter(M == 4 & F == 4)
RegFMR.Q21.Secondary <- RegFMR.Q21 %>% filter(M == 3 | M ==4) %>% filter(!(M == 4 & F == 4))
RegFMR.Q21.Non.Shopper <- RegFMR.Q21 %>% filter(M==2 | M==1)
Primary <- primary.up(3,d = RegFMR.Q21.Primary)
Secondary <- secondary.up(3,d = RegFMR.Q21.Secondary)
Non.Shopper <- non.shoppers.up(3,RegFMR.Q21.Non.Shopper)
ggplot(RegFMR.Q21, aes(x=Reg)) + geom_bar()
ggplot(RegFMR.Q21, aes(x=F)) + geom_bar()
ggplot(RegFMR.Q21, aes(x=M)) + geom_bar()

# Creating a column in the RegFMR that tells which cluster the given client 
# is part of

RegFMR.Q21 <- RegFMR.Q21 %>% mutate(
  Cluster = case_when(M == 4 & F == 4 ~ "Primary", (M == 3 & F <= 4) | (M == 4 & F < 4) ~ "Secondary", TRUE ~ "Non_Shopper")
)

# Computation of RegFMR Q1 2021
period.Q11 <- filter_customers(quartered_df$`2021 Q1`) # Select the quarter you prefer
length(unique(period.Q11$customer_id))
period.Q11$date_ticket <- parse_date_time(period.Q11$date_ticket, orders = c("%Y%m%d%H%M%S"))
RegFMR.Q11 <- RegFMR.foo(period.Q11)
avg.frequency.Q11 <- mean(RegFMR.Q11$frequency)
avg.monetary.Q11 <- mean(RegFMR.Q11$monetary)
avg.regularity.Q11 <- mean(RegFMR.Q11$regularity)

RegFMR.Q11<- scoring(RegFMR.Q11,freq_mod =avg.frequency.Q11/avg.frequency,mon_mod = avg.monetary.Q11/avg.monetary, reg_mod = avg.regularity.Q11/avg.regularity )
RegFMR.Q11.Primary <- RegFMR.Q11 %>% filter(M == 4 & F == 4)
RegFMR.Q11.Secondary <- RegFMR.Q11 %>% filter(M == 3 | M ==4) %>% filter(!(M == 4 & F == 4))
RegFMR.Q11.Non.Shopper <- RegFMR.Q11 %>% filter(M==2 | M==1)
Primary <- primary.up(2,d = RegFMR.Q11.Primary)
Secondary <- secondary.up(2,d = RegFMR.Q11.Secondary)
Non.Shopper <- non.shoppers.up(2,RegFMR.Q11.Non.Shopper)
ggplot(RegFMR.Q11, aes(x=Reg)) + geom_bar()
ggplot(RegFMR.Q11, aes(x=F)) + geom_bar()
ggplot(RegFMR.Q11, aes(x=M)) + geom_bar()

# Creating a column in the RegFMR that tells which cluster the given client 
# is part of

RegFMR.Q11 <- RegFMR.Q11 %>% mutate(
  Cluster = case_when(M == 4 & F == 4 ~ "Primary", (M == 3 & F <= 4) | (M == 4 & F < 4) ~ "Secondary", TRUE ~ "Non_Shopper")
)

# Computation of RegFMR Q4 2020

period.Q40 <- filter_customers(quartered_df$`2020 Q4`) # Select the quarter you prefer
length(unique(period.Q40$customer_id))
period.Q40$date_ticket <- parse_date_time(period.Q40$date_ticket, orders = c("%Y%m%d%H%M%S"))
RegFMR.Q40 <- RegFMR.foo(period.Q40)
avg.frequency.Q40 <- mean(RegFMR.Q40$frequency)
avg.monetary.Q40 <- mean(RegFMR.Q40$monetary)
avg.regularity.Q40 <- mean(RegFMR.Q40$regularity)

RegFMR.Q40<- scoring(RegFMR.Q40,freq_mod =avg.frequency.Q40/avg.frequency,mon_mod = avg.monetary.Q40/avg.monetary, reg_mod = avg.regularity.Q40/avg.regularity )
RegFMR.Q40.Primary <- RegFMR.Q40 %>% filter(M == 4 & F == 4)
RegFMR.Q40.Secondary <- RegFMR.Q40 %>% filter(M == 3 | M ==4) %>% filter(!(M == 4 & F == 4))
RegFMR.Q40.Non.Shopper <- RegFMR.Q40 %>% filter(M==2 | M==1)
Primary <- primary.up(1,d = RegFMR.Q40.Primary)
Secondary <- secondary.up(1,d = RegFMR.Q40.Secondary)
Non.Shopper <- non.shoppers.up(1,RegFMR.Q40.Non.Shopper)
ggplot(RegFMR.Q40, aes(x=Reg)) + geom_bar()
ggplot(RegFMR.Q40, aes(x=F)) + geom_bar()
ggplot(RegFMR.Q40, aes(x=M)) + geom_bar()

# Creating a column in the RegFMR that tells which cluster the given client 
# is part of

RegFMR.Q40 <- RegFMR.Q40 %>% mutate(
  Cluster = case_when(M == 4 & F == 4 ~ "Primary", (M == 3 & F <= 4) | (M == 4 & F < 4) ~ "Secondary", TRUE ~ "Non_Shopper")
)

# Deciding how to attribute SuperHot, Hot, Cold and Super Cold
# Notice that the attribution has been already done above in the 
# function RegFMR(). Here we are just showing a glimpse of 
# how we took that decision. 
# Knowing that the distribution is not symmetric and knowing that
# our metric recency_mod suggests us that negative values are "hotter"
# than positive ones and that values near to 0 are "preferred"
# than larger values in absolute terms, we came up with the decision to 
# create 4 clusters using 2 medians: One of the "hot" side and one 
# for the "cold" one. 

lower_median <- median(RegFMR$recency_mod[RegFMR$recency_mod < 0])
upper_median <- median(RegFMR$recency_mod[RegFMR$recency_mod >= 0])

ggplot(RegFMR, aes(x=recency_mod)) + 
  geom_histogram(binwidth=0.5, color="black", fill="black", aes(y = after_stat(count / sum(count))))+
  scale_x_continuous(breaks = round(seq(min(RegFMR$recency_mod), max(RegFMR$recency_mod), by = 1.5),1)) +
  geom_vline(xintercept = median(RegFMR$recency_mod),  col = "yellow")+
  geom_vline(xintercept = c(lower_median, upper_median, 0),  col = "red")


q_super_cold <- length(RegFMR$recency_mod[RegFMR$recency_mod > upper_median])/length(RegFMR$recency_mod)
q_super_hot <- length(RegFMR$recency_mod[RegFMR$recency_mod > lower_median & RegFMR$recency_mod <= 0])/length(RegFMR$recency_mod)
q_cold <- q_super_cold # By construction
q_hot <- q_super_hot # By construction


#########################################################################
############# Creation of the Proper Dataset for MBA ####################
#########################################################################


# Starting from the last full quarter -> Q3 2022
period <- quartered_df$`2022 Q3` # Select the quarter you prefer

MBA.Q3 <- full_join(period, RegFMR.Q3) # Note that some NAs have been created due to the misisng customer that has been removed perviously
table(RegFMR_Outliers$customer_id == "8834496470804") # Example

# Adding a column that tells which quarter it is

MBA.Q3$Quarter <- "2022 Q3"


# Q2 2022
period <- quartered_df$`2022 Q2` # Select the quarter you prefer
MBA.Q2 <- full_join(period, RegFMR.Q2)
MBA.Q2$Quarter <- "2022 Q2"

# Q1 2022
period <- quartered_df$`2022 Q1` # Select the quarter you prefer
MBA.Q1 <- full_join(period, RegFMR.Q1)
MBA.Q1$Quarter <- "2022 Q1"

# Q4 2021
period <- quartered_df$`2021 Q4` # Select the quarter you prefer
MBA.Q41 <- full_join(period, RegFMR.Q41)
MBA.Q41$Quarter <- "2021 Q4"

# Q3 2021
period <- quartered_df$`2021 Q3` # Select the quarter you prefer
MBA.Q31 <- full_join(period, RegFMR.Q31)
MBA.Q31$Quarter <- "2021 Q3"

# Q2 2021
period <- quartered_df$`2021 Q2` # Select the quarter you prefer
MBA.Q21 <- full_join(period, RegFMR.Q21)
MBA.Q21$Quarter <- "2021 Q2"

# Q1 2021
period <- quartered_df$`2021 Q1` # Select the quarter you prefer
MBA.Q11 <- full_join(period, RegFMR.Q11)
MBA.Q11$Quarter <- "2021 Q1"

# Q4 2020
period <- quartered_df$`2020 Q4` # Select the quarter you prefer
MBA.Q40 <- full_join(period, RegFMR.Q40)
MBA.Q40$Quarter <- "2020 Q4"

# Creating the full data set for MBA
str(MBA.Q1)

MBA.TOT <- rbind(MBA.Q1, MBA.Q11, MBA.Q2, MBA.Q21, MBA.Q3, MBA.Q31, MBA.Q40, MBA.Q41)
MBA.TOT <- MBA.TOT %>%  select(-c("store_id", "cashdesk_no", "m", "std", "recency", "recency_mod",
                                 "monetary", "regularity", "frequency")) # Not needed. Removed to render the computation faster

table(is.na(MBA.TOT)) # Takes long to run, BE CAREFUL ! Just checking how many NAs

# IT makes sense since we are joining by customer_id which the given period
# might have that our subset (considering only Q3 2022 customers) has
MBA.TOT <- na.omit(MBA.TOT)
any(is.na(MBA.TOT))

# Saving the data

save(MBA.TOT, file = "MBA.TOT.RData")


###################################
###### LONGITUDINAL ANALYSIS ######
###################################
.

# Primary customers analysis
ggplot(Primary, aes(x=quarters,y=M,group=1))+geom_line()+geom_point()+ggtitle("Monetary")
ggplot(Primary, aes(x=quarters,y=F,group=1))+geom_line()+geom_point()+ggtitle("Frequency")
ggplot(Primary, aes(x=quarters,y=Reg,group=1))+geom_line()+geom_point()+ggtitle("Regularity")
ggplot(Primary, aes(x=quarters,y=distance,group=1))+geom_line()+geom_point()+ggtitle("Distance")
Primary$FScaled <- scales::rescale(Primary$F,to=0:1)
Primary$MScaled <- scales::rescale(Primary$M,to=0:1)
Primary$RegScaled <- scales::rescale(Primary$Reg,to=0:1)
Primary<-na.omit(Primary)
Primary.stability <- sum(Primary$distance)
distance_computing <- function(d){
  d$distance <- 0
  for (i in 2:length(d)){
    d[i,length(colnames(d))] <- sqrt((d[i,5]-d[i-1,5])^2+(d[i,6]-d[i-1,6])^2+(d[i,7]-d[i-1,7])^2)
  }
  return(d)
}
Primary <- distance_computing(Primary)
fig <- plot_ly(Primary, x = ~FScaled, y = ~MScaled, z = ~RegScaled, type="scatter3d",mode="lines", text=~quarters,textposition = 'middle right', textfont = list(color = '#000000', size = 8))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'F'),
                                   yaxis = list(title = 'M'),
                                   zaxis = list(title = 'Reg'))
)
fig

#Secondary customers analysis
ggplot(Secondary, aes(x=quarters,y=M,group=1))+geom_line()+geom_point()+ggtitle("Monetary")
ggplot(Secondary, aes(x=quarters,y=F,group=1))+geom_line()+geom_point()+ggtitle("Frequency")
ggplot(Secondary, aes(x=quarters,y=Reg,group=1))+geom_line()+geom_point()+ggtitle("Regularity")

Secondary$FScaled <- scales::rescale(Secondary$F,to=0:1)
Secondary$MScaled <- scales::rescale(Secondary$M,to=0:1)
Secondary$RegScaled <- scales::rescale(Secondary$Reg,to=0:1)
Secondary<-na.omit(Secondary)
Secondary.stability <- sum(Secondary$distance)

Secondary <- distance_computing(Secondary)
fig <- plot_ly(Secondary, x = ~FScaled, y = ~MScaled, z = ~RegScaled, type="scatter3d",mode="lines", text=~quarters,textposition = 'middle right', textfont = list(color = '#000000', size = 8))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'F'),
                                   yaxis = list(title = 'M'),
                                   zaxis = list(title = 'Reg'))
)
fig

# Non-Shoppers analysis
ggplot(Non.Shopper, aes(x=quarters,y=M,group=1))+geom_line()+geom_point()+ggtitle("Monetary")
ggplot(Non.Shopper, aes(x=quarters,y=F,group=1))+geom_line()+geom_point()+ggtitle("Frequency")
ggplot(Non.Shopper, aes(x=quarters,y=Reg,group=1))+geom_line()+geom_point()+ggtitle("Regularity")
ggplot(Non.Shopper, aes(x=quarters,y=Reg,group=1))+geom_line()+geom_point()+ggtitle("Regularity")

Non.Shopper$FScaled <- scales::rescale(Non.Shopper$F,to=0:1)
Non.Shopper$MScaled <- scales::rescale(Non.Shopper$M,to=0:1)
Non.Shopper$RegScaled <- scales::rescale(Non.Shopper$Reg,to=0:1)

Non.Shopper<-na.omit(Non.Shopper)
Non.Shopper.stability <- sum(Non.Shopper$distance)
Non.Shopper <- distance_computing(Non.Shopper)
fig <- plot_ly(Non.Shopper, x = ~FScaled, y = ~MScaled, z = ~RegScaled, type="scatter3d",mode="lines", text=~quarters,textposition = 'middle right', textfont = list(color = '#000000', size = 8))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'F'),
                                   yaxis = list(title = 'M'),
                                   zaxis = list(title = 'Reg'))
)
fig


# Alluvial Diagram

# From wide to long

Alluvional <- MBA.TOT %>%  
  group_by(customer_id, Quarter, Cluster) %>% 
  summarise(customer_id = unique(customer_id)) %>% 
  group_by(customer_id,Quarter, Cluster) %>% 
  summarise(Freq = n()) %>% 
  ungroup()
  
Alluvional$Cluster <- as.factor(Alluvional$Cluster)
Alluvional$customer_id <- as.factor(Alluvional$customer_id)
Alluvional$Cluster <- gsub("_", " ", Alluvional$Cluster)
colors = c("#003060", "#055C9D","#0E86D4")

plot <- ggplot(Alluvional, aes(x = Quarter, y = Freq, 
                          stratum = Cluster, fill = Cluster,
                          alluvium = customer_id, label = Cluster)) +
  geom_stratum(color = "black")+
  geom_text(stat = "stratum", aes(angle = 90, label = after_stat(stratum)), color = "White")+
  geom_flow() +
  scale_x_discrete()+
  ggtitle("Customer Migration")+
  theme_minimal()+
  theme(plot.title = element_text(size = 30, color = "#003060"),
        axis.title.y=element_blank(),
        axis.text.y = element_blank(),
        theme.background = element_blank())+
  scale_fill_manual(values = colors)
 
plot

