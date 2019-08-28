# 4-3 연관성 분석(장바구니 분석)
# 지지도



# 신뢰도



# 향상도

#==================================================================
install.packages('arules')
install.packages('arulesViz')
library(arules)
library(arulesViz)

data(package='arules')
inspect(Groceries[1:10])
summary(Groceries)

sort(itemFrequency(Groceries, type='absolute'), decreasing = T)
round(sort(itemFrequency(Groceries, type='absolute'), decreasing = T), 2)
itemFrequencyPlot(Groceries,topN=10, type='absolute')
itemFrequencyPlot(Groceries,topN=10, type='relative')
apriori(Groceries)
rt_rules=apriori(Groceries, parameter = list(support=0.005, confidence=0.5, minlen=2))
summary(rt_rules) #120개의 규칙이 생김
inspect(rt_rules[1:5])

rules_lift=sort(rt_rules, by='lift', decreasing = T)
inspect(rules_lift[1:10]) #lift를 보자 3.69배가 높다

#94page
milk_rule=subset(rt_rules, items %in% 'whole milk')
milk_rule
rhs_milk=subset(rt_rules, rhs %in% 'whole milk')
rhs_milk
inspect(rhs_milk[1:5])
wholemilk_rule<-apriori(Groceries, parameter=list(support=0.005, confidence=0.5, minlen=2),
                        appearance = list(default="1hs", rhs="whole milk"))
wholemilk_rule<-sort(wholemilk_rule, by='lift', decreasing=T)
inspect(wholemilk_rule[1:5])




library(arulesViz)
plot(wholemilk_rule[1:10], method='graph', measure='lift', shading='confidence')
