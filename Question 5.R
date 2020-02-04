my_function <- function(net_income,total_assets,shareholders_equity,net_sales){
  ROA <- net_income/total_assets
  ROE <- net_income/shareholders_equity
  profit_margin <- net_income/net_sales
  ratios <- c(ROA,ROE,profit_margin)
  names <- c("ROA","ROE","Profit Margin")
  final <- cbind(ratios,names)
  print(final)

}
#Company 1
my_function(10000,40000,10000,50000)

#company 2
my_function(8000,45000,20000,35000)
 
 #company 3
my_function(20000,60000,40000,70000)
 
 #company 4
my_function(2000,10000,8000,4000)
 
Avg_roa= (0.177+0.333+0.2+0.25)/4
Avg_roa

#company weighted roa
ROA1=Avg_roa*0.25
ROA1
ROA2=Avg_roa*0.177
ROA2
ROA3=Avg_roa*0.333
ROA3
ROA4=Avg_roa*0.2
ROA4
#Investing 10% from comapany 1 into company 4
my_function(3000,14000,9000,9000)
