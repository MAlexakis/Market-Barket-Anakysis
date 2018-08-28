library(arules) 
library(arulesViz)

# Full dataset and Observations --------------------------------------------------------------------
setwd("~/Dropbox/Ubiqum Code Academy/Module 2/Task 4")
Transactions<-read.transactions("ElectronidexTransactions2017.csv",format="basket",sep=",",
                                rm.duplicates=FALSE)
# Create a new dataframe without the transactions with 0 items -------------------------------------
FullTrans <- Transactions[-c(which(size(Transactions)==0)), ] 
CYB<-subset(FullTrans, items %in% "Acer Desktop" )
length(CYB)

#inspect(FullTrans) # You can view the transactions. 
length (FullTrans) # Number of transactions.
size(FullTrans) # Number of items per transaction
max(size(FullTrans))
min(size(FullTrans))
round(mean(size(FullTrans)))
which(size(FullTrans)==30) #30 items in this transaction
#LIST(FullTrans) # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(FullTrans) # To see the item labels

BasketRules <- apriori(FullTrans, parameter = list(supp = 0.002, conf = 0.8, target = "rules"))
summary(BasketRules)
inspect(BasketRules)
inspect(sort(BasketRules,by="lift")[1:10])
PrunedBasRules<-BasketRules[which(is.redundant(BasketRules)==FALSE)]

BWproducts<-c("HP Wireless Printer","Canon Office Printer","Brother Printer","Brother Printer Toner",
              "ASUS Chromebook","Acer Aspire","Dell Monitor","LG Monitor","HP Desktop","Dell 2 Desktop",                                       
              "Dell Desktop")
PrunedBasRulesBW<-subset(PrunedBasRules, items %in% BWproducts)
inspect(sort(PrunedBasRulesBW,by="lift"))
summary(PrunedBasRules)
plot(PrunedBasRules[1:10], method="graph", control=list(type="items"))


itemFrequencyPlot(FullTrans,type ="absolute", topN=10)
image(sample(FullTrans,50))

# Potential Privaste Customers ---------------------------------------------------------------------
PrCustPr<-c("Eluktronics Pro Gaming Laptop","CYBERPOWER Gamer Desktop","Redragon Gaming Mouse",
            "Backlit LED Gaming Keyboard","Apple Earpods","Monster Beats By Dr Dre",
            "Otium Wireless Sports Bluetooth Headphone","Panasonic In-Ear Headphone",
            "APIE Bluetooth Headphone","Gaming Mouse Professional",
            "Rii LED Gaming Keyboard & Mouse Combo","Zombie Gaming Headset",
            "Philips Flexible Earhook Headphone","PC Gaming Headset","Koss Home Headphones",
            "XIBERIA Gaming Headset","iPhone Charger Cable", "Rokono Mini Speaker",
            "Samsung Charging Cable", "Cambridge Bluetooth Speaker",
            "JBL Splashproof Portable Bluetooth Speaker","DOSS Touch Wireless Bluetooth",
            "Apple TV","Google Home","Smart Light Bulb","Fire TV Stick","Roku Express")

PotPrCust1<-FullTrans[which(size(FullTrans)==1)]
itemFrequencyPlot(PotPrCust1,type ="absolute", topN=10)
PotPrCust1BW<-subset(PotPrCust1, item %in% BWproducts) #There are no items that BlackWell sells

SmallTrans<-FullTrans[which(size(FullTrans)<=6&size(FullTrans)>1)]     
PotPrCust2<-subset(SmallTrans, items %in% PrCustPr)
itemFrequencyPlot(PotPrCust2,type ="absolute", topN=10)
PrCust2Rules<-apriori(PotPrCust2, parameter = list(supp = 0.002, conf = 0.8, target = "rules"))
is.redundant(PrCust2Rules)
PrCust2Rules<-PrCust2Rules[which(is.redundant(PrCust2Rules)==FALSE)]
inspect(sort(PrCust2Rules,by="lift"))
PrCust2RulesBW<-subset(PrCust2Rules, items %in% BWproducts) #There are no products that BlackWell sells
inspect(sort(PrCust2RulesBW,by="lift"))


summary(PrunedPrCust2Rules)
plot(PrunedPrCust2Rules, method="graph", control=list(type="items"))

# Potencial Companies Customers -------------------------------------------------------------------
LargeTrans<-FullTrans[which(size(FullTrans)<=6&size(FullTrans)>1)]
PotCompanies1<- subset(LargeTrans, !(items %in% PrCustPr))
itemFrequencyPlot(PotCompanies1,type ="absolute", topN=10)
PotComp1Rules<-apriori(PotCompanies1, parameter = list(supp = 0.0015, conf = 0.6, target = "rules"))
inspect(sort(PotComp1Rules,by="lift"))
PotComp1RulesBW<-subset(PotComp1Rules, items %in% BWproducts)
inspect(sort(PotComp1RulesBW,by="lift"))

PotCompanies2<- FullTrans[which(size(FullTrans)>6)]
itemFrequencyPlot(PotCompanies2,type ="absolute", topN=11)
PotComp2Rules<-apriori(PotCompanies2, parameter = list(supp = 0.006, conf = 0.8, target = "rules"))
inspect(sort(PotComp2Rules,by="lift")[1:5])
PotComp2RulesBW<-subset(PotComp2Rules, items %in% BWproducts)
Sorted<-sort(PotComp2RulesBW,by="lift")[1:15]
plot(Sorted[8:10], method="graph", control=list(type="items"), )

DellTrans<-subset(PotCompanies2, items %in% "Dell Desktop")
length(DellTrans)

AcerTrans<-subset(PotCompanies2, items %in% "Acer Aspire")
length(AcerTrans)