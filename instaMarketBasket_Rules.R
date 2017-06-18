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
instaRules_sup01 = apriori(instaTransactions, parameter=list(support=0.0001, confidence=0.8))
instaRules_sup10 = apriori(instaTransactions, parameter=list(support=0.1, confidence=0.8, maxlen = 2))

inspect(instaRules_sup01)

# Plot network graph for items 
plot(instaRules_sup01,method="graph",interactive=TRUE,shading=NA)

instaRules_sup01<-sort(instaRules_sup01, by="confidence", decreasing=TRUE)

# Write the rules to csv 
write(instaRules_sup01, "instaSup001Rules.csv", sep=",") 

# Rules with target item 

target47766 <-apriori(instaTransactions, parameter=list(supp=0.001,conf = 0.08), 
               appearance = list(default="lhs",rhs="47766"),
               control = list(verbose=F))

target47766 <- sort(target47766, decreasing=TRUE,by="confidence")

# Show the top 5 rows 
inspect(target47766[1:5])


