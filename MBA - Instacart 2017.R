library(data.table)
library(tidyverse)

## Order_products_train dataset
head(order_products_train)

order_data <- data.frame(order_products_train$order_id, order_products_train$product_id)
sum(is.na(order_products_train))
names(order_data) <- c("order_id", "product_id")

# Add column for product name
order_data$product_name <- "Product_Name"

# Check NA value
sum(is.na(order_data))

## Products dataset
head(products)
products_data <- data.frame(products$product_id, products$product_name)
head(products_data)
names(products_data) <- c("product_id", "product_name")

# Check NA value
sum(is.na(products_data)) # 3 NA Values
sum(is.na(products_data$product_id)) # 0 NA value
sum(is.na(products_data$product_name)) # 3 NA value
which(is.na(products_data$product_name)) # row 5394, 11908, 22958

products_data$product_name <- as.character(products_data$product_name)
products_data$product_name[is.na(products_data$product_name)] <- "Unnamed Product"

# Rename product_name in order_data dataframe based on product_id in products_data dataframe
setDT(order_data)
setDT(products_data)
order_data[products_data, on = .(product_id), ':=' (product_name= i.product_name)]
head(order_data)

library(arules)
library(arulesViz)
library(knitr)
library(ggplot2)
library(plyr)

class(order_data$product_id) # class = 'numeric' -> perlu diubah jadi factor
order_data$product_id <- as.factor(order_data$product_id)

df <- data.frame(order_data$order_id, order_data$product_id, )
head(df)
colnames(df) <- c("order_id", "product_id")

mba <- ddply(order_data, "order_id", 
             function(df)paste(df$product_name, collapse = ","))
write.csv(mba, "MBA.csv", quote = FALSE, row.names = TRUE)

mba_data = read.transactions(file="mba.csv", rm.duplicates = FALSE, format = "basket", sep = ",", cols = 1)
inspect(head(mba_data))

basket_rules1 <- apriori(mba_data, parameter = list(supp = 0.001, conf = 0.8, 
                         target = "rules"))
inspect(head(basket_rules1))
inspect(tail(basket_rules1))

basket_rules2 <- apriori(mba_data, parameter = list(supp = 0.001, conf = 0.8, target = "rules"), 
                         appearance = list(default="lhs",rhs="Milk"),
                         control = list(verbose=F))
inspect(head(basket_rules2))
inspect(tail(basket_rules2))
