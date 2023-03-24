
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




# ND data -----------------------------------------------------------------

columns <- read.delim("+Table-test.df.txt")
columns <- columns[columns$treatment %in% c("Nitrate", "Drought", "ABA"),]
row.names(columns) <- columns$experiment
columns$treatment <- factor(columns$treatment, levels=c("Nitrate", "Drought", "ABA"))

rows <- read.delim('+Table-all_sDEGs-lfc0-p0.01.txt')
rows <- rows[rows$Nitrate != "-",]
row.names(rows) <- rows$gene_id
rows = rows[1:30,]

df <- read.delim('+Table-experimental_lfc.txt')
df <- df[row.names(rows), row.names(columns)]
df <- as.matrix(df)
df[is.na(df)] <- 0
df[df > 3] <- 3
df[df < -3] <-3




nh = nheatmap(df, zero_centered_colors = T,
                  columns=columns, rows=rows,
                  column_groups=c('treatment'), row_groups=c("PlantTFDB", 'Nit_GOs'),
              cluster_cols=T,
              group_gap = 0.02,
              plot_margin = c(0.2,0.4,0.2,0.2))

nh = nheatmap_group(nh, 3, 'treatment', labels=F, percent=0.05, label_just = 'left')
nh = nheatmap_annotate(nh, 3, 'tissue', percent=0.05, label_just = 'left')

nh = nheatmap_group(nh, 4, 'PlantTFDB', labels=F, percent=0.05, label_just = 'right')
nh = nheatmap_group(nh, 4, 'Nit_GOs', labels=F, percent=0.05, label_just = 'right')

nh = nheatmap_names(nh, 2)
nh = nheatmap_names(nh, 2, "symbol")
nh = nheatmap_dend(nh, 2, lwd=0.5)




