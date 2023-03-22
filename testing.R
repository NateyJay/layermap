
## I need some test data.

# 1) small and manageable - showing labels.
# 2) large in one axis (many genes).
# 3) large in 2 axes (many genes and samples).



##
test.df = as.data.frame(matrix(rnorm(200), 20, 10))
test.df[1:10, seq(1, 10, 2)] = test.df[1:10, seq(1, 10, 2)] + 3
test.df[11:20, seq(2, 10, 2)] = test.df[11:20, seq(2, 10, 2)] + 2
test.df[15:20, seq(2, 10, 2)] = test.df[15:20, seq(2, 10, 2)] + 4
colnames(test) = paste("Test", 1:10, sep = "")
rownames(test) = paste("Gene", 1:20, sep = "")


column_groups <- data.frame(row.names=colnames(test.df),
                            first_grouping=rep_len(c('some','group','names','some','some'), ncol(test.df)))

row_groups <- data.frame(row.names=rownames(test.df),
                            second_grouping=rep_len(c('this','this','is','a','row','group'), nrow(test.df)))


nh = nheatmap(test.df, cluster_cols = T, cluster_rows = T,
              group_cols = column_groups,
              group_rows = row_groups)
nh = nheatmap_group(nh, 'first_grouping', 3)
nh = nheatmap_group(nh, 'second_grouping', 2)

nh = nheatmap_dend(nh, side=4, lwd=2, lty=1)
nh = nheatmap_dend(nh, side=1, lwd=2, lty=1)
nh = nheatmap_dend(nh, side=3, lwd=2, lty=1)
nheatmap_legend(nh)





# US cities ---------------------------------------------------------------

x = data.matrix(UScitiesD, rownames.force = TRUE)
x
