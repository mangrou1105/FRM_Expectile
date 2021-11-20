install.packages("gridExtra")
library("gridExtra")

install.packages("lattice")
install.packages("latticeExtra")
library(lattice)
library(latticeExtra)

#loading data

adj_mat_19_mean =  read.csv("//Users//mangrou//Documents//Expectile//adj_mat_19.csv", header = T, row.names=1)
adj_mat_20_mean =  read.csv("//Users//mangrou//Documents//Expectile//adj_mat_20.csv", header = T, row.names=1)

adj_mat_19_mean = as.matrix(adj_mat_19_mean)

adj_mat_19_mean = adj_mat_19_mean[1:25, 1:25]
colnames(adj_mat_19_mean) <- paste(c(1:25))

adj_mat_20_mean = as.matrix(adj_mat_20_mean)

adj_mat_20_mean = adj_mat_20_mean[1:25, 1:25]
colnames(adj_mat_20_mean) <- paste(c(1:25))


# Plot 
library(colorRamps)
col=colorRampPalette(matlab.like2(255))

plot_2019=levelplot(adj_mat_19_mean, margin = FALSE, at = seq(-0.2, 0.5, 0.05), xlim =c(0, 25), ylim = c(0, 25), xlab="", ylab="", main = list('2019'),col.regions = col)
plot_2020=levelplot(adj_mat_20_mean, margin = FALSE, at = seq(-0.2, 0.5, 0.05), xlim =c(0, 25), ylim = c(0, 25), xlab="", ylab="", main = list('2020'),col.regions = col)

grid.arrange(plot_2019, plot_2020, ncol=2)

#leverplot the pos and neg
## pos
adj_mat_mean_year_pos = read.csv("//Users//mangrou//Documents//Expectile//adjyear//adj_mat_mean_year_pos.csv", header = FALSE, stringsAsFactors = TRUE)

adj_mat_mean_year_pos = as.matrix(adj_mat_mean_year_pos)

adj_mat_mean_year_pos = adj_mat_mean_year_pos[1:25, 1:25]
colnames(adj_mat_mean_year_pos) <- paste(c(1:25))
adj_pos = levelplot(adj_mat_mean_year_pos, xlim =c(0, 25), ylim = c(0, 25), xlab="", ylab="", main = list('Average+'),col.regions = col)

## neg

adj_mat_mean_year_neg = read.csv("//Users//mangrou//Documents//Expectile//adjyear//adj_mat_mean_year_neg.csv", header = FALSE, stringsAsFactors = TRUE)

adj_mat_mean_year_neg = as.matrix(adj_mat_mean_year_neg)

adj_mat_mean_year_neg = adj_mat_mean_year_neg[1:25, 1:25]
colnames(adj_mat_mean_year_neg) <- paste(c(1:25))
adj_neg = levelplot(adj_mat_mean_year_neg, xlim =c(0, 25), ylim = c(0, 25), xlab="", ylab="", main = list('Average-'),col.regions = col)

grid.arrange(adj_pos, adj_neg, ncol=2)
