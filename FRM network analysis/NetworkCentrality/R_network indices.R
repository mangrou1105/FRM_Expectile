
rm(list = ls(all = TRUE))
graphics.off()

wdir = "//Users//mangrou//Documents//Expectile//Adj_Matrices3"
setwd(wdir)

install.packages("TTR")
install.packages("tseries")
install.packages("forecast")
library(tseries)
library(ggplot2)
library(TTR)
library(forecast)

lambda = read.csv("lambdas_wide_expectile.csv", header = TRUE)

date = lambda$date

outdegree = matrix(0, length(date), 101)
outdegree[, 1] = date

indegree = matrix(0, length(date), 101)
indegree[, 1] = date

totaldegree = matrix(0, length(date), 2)
totaldegree[, 1] = date

totaldegree2 = matrix(0, length(date), 2)
totaldegree2[, 1] = date

for (t in 1:( length(date) ) ){
  adj0 = read.csv( file=paste0("adj_matix_expectile_", date[t], ".csv"), header = TRUE, sep = ",", row.names = 1)
  adj0 = as.matrix(adj0)[1:100 , 1:100 ] 
  
  for (j in c(1:100)){
    indegree[t,j+1]= sum (adj0[j, 1:100]!=0 )
    outdegree[t,j+1 ] = sum( adj0[1:100, j]!=0 )
  }
  
  # totaldegree == totaldegree2  to show how many financial institutions interact with each other in average
  totaldegree[t,2] =  sum ( indegree[t,2:101])/100
  totaldegree2[t,2] =  sum ( outdegree[t,2:101])/100
}

#Indegree draw pictures by using moving average
totaldegree20 = ma(totaldegree, order = 20)
totaldegree60 = ma(totaldegree, order = 60)
totaldegree120 = ma(totaldegree, order = 120)

a = data.frame(totaldegree20)
b = data.frame(totaldegree60)
c = data.frame(totaldegree120)


# plot(a$X2, type="line", xlab = "Date", ylab = "", xaxt = "n", main = "Network Index Moving Average")
# axis(1,labels = c("201906", "201910", "202003", "202007", "202012"),at=c(0, 100, 200, 300, 400))
# 
# lines(b$X2, col= 2)
# lines(c$X2, col= 3)
# legend("bottomright", legend = c("MA20", "MA60", "MA120"), col = c(1,2,3), lty=c(1,1,1),cex=0.3)

# estimate MA20 at tau 0.01
outdegree001 = matrix(0, length(date), 101)
outdegree001[, 1] = date

indegree001 = matrix(0, length(date), 101)
indegree001[, 1] = date

totaldegree001 = matrix(0, length(date), 2)
totaldegree001[, 1] = date

totaldegree2_001 = matrix(0, length(date), 2)
totaldegree2_001[, 1] = date


# estimate MA20 at tau 0.01
for (t in 1:( length(date) ) ){
  adj001 = read.csv( file=paste0("adj_matix_expectile_0.01_", date[t], ".csv"), header = TRUE, sep = ",", row.names = 1)
  adj001 = as.matrix(adj001)[1:100 , 1:100 ] 
  
  for (j in c(1:100)){
    indegree001[t,j+1]= sum (adj001[j, 1:100]!=0 )
    outdegree001[t,j+1 ] = sum( adj001[1:100, j]!=0 )
  }
  
  # totaldegree == totaldegree2  to show how many financial institutions interact with each other in average
  totaldegree001[t,2] =  sum ( indegree001[t,2:101])/100
  totaldegree2_001[t,2] =  sum ( outdegree001[t,2:101])/100
}

#tau =0.01 Indegree draw pictures by using moving average
totaldegree_001_20 = ma(totaldegree001, order = 20)
a_001 = data.frame(totaldegree_001_20)

# estimate MA20 at tau 0.1
outdegree01 = matrix(0, length(date), 101)
outdegree01[, 1] = date

indegree01 = matrix(0, length(date), 101)
indegree01[, 1] = date

totaldegree01 = matrix(0, length(date), 2)
totaldegree01[, 1] = date

totaldegree2_01 = matrix(0, length(date), 2)
totaldegree2_01[, 1] = date

# estimate MA20 at tau 0.1
for (t in 1:( length(date) ) ){
  adj01 = read.csv( file=paste0("adj_matix_expectile_0.1_", date[t], ".csv"), header = TRUE, sep = ",", row.names = 1)
  adj01 = as.matrix(adj01)[1:100 , 1:100 ] 
  
  for (j in c(1:100)){
    indegree01[t,j+1]= sum (adj01[j, 1:100]!=0 )
    outdegree01[t,j+1 ] = sum( adj01[1:100, j]!=0 )
  }
  
  # totaldegree == totaldegree2  to show how many financial institutions interact with each other in average
  totaldegree01[t,2] =  sum ( indegree01[t,2:101])/100
  totaldegree2_01[t,2] =  sum ( outdegree01[t,2:101])/100
}

#tau =0.1 Indegree draw pictures by using moving average
totaldegree_01_20 = ma(totaldegree01, order = 20)
a_01 = data.frame(totaldegree_01_20)


# estimate MA20 at tau 0.25
outdegree025 = matrix(0, length(date), 101)
outdegree025[, 1] = date

indegree025 = matrix(0, length(date), 101)
indegree025[, 1] = date

totaldegree025 = matrix(0, length(date), 2)
totaldegree025[, 1] = date

totaldegree2_025 = matrix(0, length(date), 2)
totaldegree2_025[, 1] = date

# estimate MA20 at tau 0.25
for (t in 1:( length(date) ) ){
  adj025 = read.csv( file=paste0("adj_matix_expectile_0.25_", date[t], ".csv"), header = TRUE, sep = ",", row.names = 1)
  adj025 = as.matrix(adj025)[1:100 , 1:100 ] 
  
  for (j in c(1:100)){
    indegree025[t,j+1]= sum (adj025[j, 1:100]!=0 )
    outdegree025[t,j+1 ] = sum( adj025[1:100, j]!=0 )
  }
  
  # totaldegree == totaldegree2  to show how many financial institutions interact with each other in average
  totaldegree025[t,2] =  sum ( indegree025[t,2:101])/100
  totaldegree2_025[t,2] =  sum ( outdegree025[t,2:101])/100
}

#tau =0.01 Indegree draw pictures by using moving average
totaldegree_025_20 = ma(totaldegree025, order = 20)
a_025 = data.frame(totaldegree_025_20)

# estimate MA20 at tau 0.5
outdegree05 = matrix(0, length(date), 101)
outdegree05[, 1] = date

indegree05 = matrix(0, length(date), 101)
indegree05[, 1] = date

totaldegree05 = matrix(0, length(date), 2)
totaldegree05[, 1] = date

totaldegree2_05 = matrix(0, length(date), 2)
totaldegree2_05[, 1] = date



# estimate MA20 at tau 0.5
for (t in 1:( length(date) ) ){
  adj05 = read.csv( file=paste0("adj_matix_expectile_0.5_", date[t], ".csv"), header = TRUE, sep = ",", row.names = 1)
  adj05 = as.matrix(adj05)[1:100 , 1:100 ] 
  
  for (j in c(1:100)){
    indegree05[t,j+1]= sum (adj05[j, 1:100]!=0 )
    outdegree05[t,j+1 ] = sum( adj05[1:100, j]!=0 )
  }
  
  # totaldegree == totaldegree2  to show how many financial institutions interact with each other in average
  totaldegree05[t,2] =  sum ( indegree05[t,2:101])/100
  totaldegree2_05[t,2] =  sum ( outdegree05[t,2:101])/100
}

#tau =0.01 Indegree draw pictures by using moving average
totaldegree_05_20 = ma(totaldegree05, order = 20)
a_05 = data.frame(totaldegree_05_20)

plot(a_001$X2, type="line", xlab = "Date", ylab = "", ylim = c(2,11), xaxt = "n", main = "Network Index Moving Average")
axis(1,labels = c("201906", "201910", "202003", "202007", "202012"),at=c(0, 100, 200, 300, 400))
lines(a$X2, col= 2)
lines(a_01$X2, col= 3)

lines(a_025$X2, col= 4)
lines(a_05$X2, col= 5)

legend("bottomright", legend = c("tau=0.01", "tau=0.05", "tau=0.1","tau=0.25","tau=0.5"), col = c(1,2,3,4,5), lty=c(1,1,1,1,1),cex=0.3)
