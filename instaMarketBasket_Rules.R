# Load libraries for association rule mining and viz 
library(arules)
library(arulesViz)
library(ggplot2)

# Load instacart transaction and product identifier data 

ordersData = read.csv("Data/ordersTrain.csv", header=TRUE, sep=",")
products = read.csv("Data/products.csv", header = TRUE, sep=",")

# Join the orders data (transactions) with product to get name of the product based on ID 

ordersInfo = merge(x = ordersData, y = products, by = "product_id", all.x = TRUE)

# Prepare the data as transactions as in market basket arules 

instaTransactions = as(split(ordersInfo[,"product_id"], ordersInfo[,"order_id"]), "transactions")

# Get frequent itemsets 

frequentItem_rel = itemFrequency(instaTransactions, type = "relative")
frequentItems_top10 =  itemFrequencyPlot(instaTransactions, topN = 10)

# rules mining 
instaRules_sup5 = apriori(instaTransactions, parameter=list(support=0.05, confidence=0.8, maxlen = 4))
instaRules_sup1 = apriori(instaTransactions, parameter=list(support=0.01, confidence=0.8, maxlen = 4))
instaRules_sup10 = apriori(instaTransactions, parameter=list(support=0.1, confidence=0.8, maxlen = 2))

inspect(instaRules_sup10)
