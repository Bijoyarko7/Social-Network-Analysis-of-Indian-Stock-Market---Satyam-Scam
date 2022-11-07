cor_mat <- cor(`2008.06`)
cor_g <- graph_from_adjacency_matrix(`2008_06_cor`, mode='undirected', weighted = TRUE)
cor_mst <- mst(`2008_06_cor`)
plot(cor_mst)