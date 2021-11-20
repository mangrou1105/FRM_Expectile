rm(list = ls(all = TRUE))
graphics.off()

install.packages("igraph")
library(igraph)

##loading data

link_pos = read.csv("//Users//mangrou//Documents//Expectile//adjyear//link_pos.csv", header = T, row.names=1)
link_neg = read.csv("//Users//mangrou//Documents//Expectile//adjyear//link_neg.csv", header = T, row.names=1)

colnames(link_pos) <- paste(c(1:25))
link_pos <- as.matrix(link_pos)

net_pos <- graph_from_adjacency_matrix(link_pos) 
plot(net_pos, edge.arrow.size=0.1, vertex.label.cex=.5, main = "Average+")


colnames(link_neg) <- paste(c(1:25))
link_neg <- as.matrix(link_neg)

net_neg <- graph_from_adjacency_matrix(link_neg) 
plot(net_neg, edge.arrow.size=0.1, vertex.label.cex=.5, main = "Average-")

par(mfrow = c(1,2))

plot(net_pos, edge.arrow.size=0.07, vertex.label.cex=.5, main = "Average+")
plot(net_neg, edge.arrow.size=0.07, vertex.label.cex=.5, main = "Average-")


