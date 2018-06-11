#' this is a function takes two tables as input. One is table with prices of fishes
#' and other is the table with number of fishes per location and a binary variable
#' for producing a graph. 
#' price_table = table with fish prices
#' number_table= table with number of fishes per location
#' graph= a variable with 1 for producing a plot and 0 otherwise. Defaults to 0
summarize<-function(price_table,number_table,graph=0){
rownames(price_table)<-price_table[,1]
price_table[,1]<-NULL
rownames(number_table)<-number_table[,1]
number_table[,1]<-NULL
ret=list()
most_frequent_fish_by_location<-apply(number_table,2,max)
ret[['most_frequent_fish_by_location']]=(most_frequent_fish_by_location)

price_table<-price_table[rownames(number_table),]
revenue_by_location<-apply(number_table*price_table,2,sum)
ret[['revenue by location']]=(revenue_by_location)

if (graph==1){
(plot(revenue_by_location,xaxt='n',type='o',ylim=range(revenue_by_location)))
axis(1,at=1:length(names(revenue_by_location)),labels=names(revenue_by_location))}

total_fishereis_revenue<-sum(revenue_by_location)
ret[["Total fisheries revenue is"]]=total_fishereis_revenue
return(ret)
}

#' The relationship between specific growth rate (G) and temperature in °C (T) 
# 'estimated by a third order#polynomial (G=a+bT+cT**2+d*T**3 ), 
#' where a,b,c,d and are parameters (input of the function).

growth_estimator<-function(a=-0.4970,b=0.1656,c=0.08588,d=-0.004266,temp=10){
	
	growth<-a+b*temp+c*temp^2+d*temp^3
	return (data.frame(temp,growth))
}