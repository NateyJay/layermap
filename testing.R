
test.df = as.data.frame(matrix(rnorm(200), 20, 10))
test.df[1:10, seq(1, 10, 2)] = test.df[1:10, seq(1, 10, 2)] + 3
test.df[11:20, seq(2, 10, 2)] = test.df[11:20, seq(2, 10, 2)] + 2
test.df[15:20, seq(2, 10, 2)] = test.df[15:20, seq(2, 10, 2)] + 4
colnames(test) = paste("Test", 1:10, sep = "")
rownames(test) = paste("Gene", 1:20, sep = "")


column_groups <- data.frame(row.names=names(test.df),
                            groupA=rep_len(c('some','group','names','some','some'), ncol(test.df)))


nh = nheatmap(test.df, cluster_cols = T, cluster_rows = T,
              group_cols = column_groups)

nh = nheatmap_dend(nh, side=4, lwd=3, lty=2)
nh = nheatmap_group(nh, 'groupA', 3)

