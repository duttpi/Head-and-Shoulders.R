
	
options(warn=-1)
Limit = 0.015
Stock = 'IBM'
Data = read.table('NYSE.csv',header=T, sep=',')
HNS = subset(Data, select = c(Close), Ticker==Stock)
HNS = as.data.frame(HNS)
HNS[, c('E1','E2','E3','E4','E5')] = 0

# 5 Extreme points
E1=E2=E3=E4=E5=0
for (i in 2:(nrow(HNS)))
{
	HNS$E1[i] = HNS$Close[i]
	HNS$E2[i] = HNS$E1[i]
	E1 = HNS$E1[i]
	j = i+1
		for (i in j:(nrow(HNS))){
		if (HNS$E2[i-1] != 0 && HNS$Close[i]<=E1)
			{HNS$E2[i] = min(HNS$E2[i-1],HNS$Close[i])}
		else{
			break
			}	}
	E3 = HNS$E3[i-1] = HNS$Close[i-1]
	E2 = HNS$E2[i-1]
	j = i
	for (i in j:(nrow(HNS))) {
		if (HNS$E3[i-1] != 0 && HNS$Close[i]>=E3)
			{HNS$E3[i] = max(HNS$E3[i-1],HNS$Close[i])}
		else{
			break
			}	}
	E4 = HNS$E4[i-1] = HNS$Close[i-1]
	E3 = HNS$E3[i-1]
	j = i
	for (i in j:(nrow(HNS))) {
		if (HNS$E4[i-1] != 0 && HNS$Close[i]<=E4)
			{HNS$E4[i] = min(HNS$E4[i-1],HNS$Close[i])}
		else{
			break
			}	}
	
	E5 = HNS$E5[i-1] = HNS$Close[i-1]
	E4 = HNS$E4[i-1]
	E4.pos = i-1
	j = i
	for (i in j:(nrow(HNS))) {
		if (HNS$E5[i-1] != 0 && HNS$Close[i]>=E5)
			{HNS$E5[i] = max(HNS$E5[i-1],HNS$Close[i])}}
	E5 = max(HNS['E5'])
	
if (E1>E2 && E3>E1 && E3>E5 && +
	E1 <= (1+Limit) * mean(x=c(E1,E5)) && +
	E5 <= (1+Limit) * mean(x=c(E1,E5)) && +
	E2 <= (1+Limit) * mean(x=c(E2,E4)) && +
	E4 <= (1+Limit) * mean(x=c(E2,E4)))
	{
	cat(' E1= ',E1,'\n','E2= ',E2,'\n','E3= ', +
		  E3,'\n','E4= ',E4,'\n','E5= ',E5,'\n')
	break
	}
	else{
	(HNS[, c('E1','E2','E3','E4','E5')] = 0)}
}

# Remove duplicate value from calculation 
for(i in 2:ncol(HNS)){
  HNS[,i][duplicated(HNS[,i])] = 0
}

# X co-ordinate for Extreme points
E1.pos = which(HNS$E1==E1)
E2.pos = which(HNS$E2==E2)
E3.pos = which(HNS$E3==E3)
E4.pos = which(HNS$E4==E4)
E5.pos = which(HNS$E5==E5)

# Neckline computation: y = Slope * x + Intercept
Slope=(E4-E2)/(E4.pos-E2.pos)
Intercept=E2-E2.pos*(E4-E2)/(E4.pos-E2.pos)

# Plot HNS
Plot = HNS$Close
y.min = min(Plot) 
y.max = max(Plot)

par(new=T)   
plot(ts(Plot),ylim=c(y.min,y.max),ylab="Closing",col="blue",lwd=1,main=Stock)
points(E1.pos, E1,col = "red")
points(E2.pos, E2,col = "red")
points(E3.pos, E3,col = "red")
points(E4.pos, E4,col = "red")
points(E5.pos, E5,col = "red")
abline(Intercept, Slope)
segments(E1.pos, E1, E2.pos, E2)
segments(E2.pos, E2, E3.pos, E3)
segments(E3.pos, E3, E4.pos, E4)
segments(E4.pos, E4, E5.pos, E5)