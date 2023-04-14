#
#
# # ND data -----------------------------------------------------------------
#
# column.df <- read.delim("test/+Table-test.df.txt")
# column.df <- column.df[column.df$treatment %in% c("Nitrate", "Drought", "ABA"),]
# row.names(column.df) <- column.df$experiment
# column.df$treatment <- factor(column.df$treatment, levels=c("Nitrate", "Drought", "ABA"))
#
# row.df <- read.delim('test/+Table-all_sDEGs-lfc0-p0.01.txt')
# row.df <- row.df[row.df$Nitrate != "-",]
# rownames(row.df) <- row.df$gene_id
# row.df = rows[1:30,]
#
# df <- read.delim('test/+Table-experimental_lfc.txt')
# df <- df[row.names(row.df), row.names(column.df)]
# df <- as.matrix(df)
# df[is.na(df)] <- 0
# df[df > 3] <- 3
# df[df < -3] <-3
#
#
# write.table(column.df, file="test/columns.txt", quote=F, sep='\t', row.names=T)
# write.table(row.df, file="test/rows.txt", quote=F, sep='\t', row.names=T)
# write.table(df, file="test/values.txt", quote=F, sep='\t', row.names=T)
#
#
#



# # Nitrogen Drought data ---------------------------------------------------



show_test_plot <- function(save=F) {


  column.df <- read.delim("test/columns.txt")
  column.df$treatment <- factor(column.df$treatment, levels=c("Nitrate", "Drought", "ABA"))

  row.df <- read.delim("test/rows.txt")
  value.df <- read.delim("test/values.txt")



  if (save) {
    file_name = "test/test.svg"
    svglite::svglite(file_name, 7, 5.4)
  }

  par(mar=c(5,7,5,10))
  lp = layermap(value.df, zero_centered_colors = T,
                    column.df=column.df, row.df=row.df,
                    column_groups=c('treatment'), row_groups=c("PlantTFDB", 'Nit_GOs'),
                cluster_cols=T,
                group_gap = 0.1)

  lp_color_legend(lp, 1)

  lp = lp_group(lp, 3, 'treatment', labels=T, label_just = 'left',
                      col=setNames(c('seagreen','tomato'), c('Nitrate','ABA')),
                      show_bounding_box = F)


  lp = lp_annotate(lp, 3, 'tissue', label_just = 'left',
                         col=setNames('red', 'Root'))

  lp = lp_group(lp, 4, 'PlantTFDB', labels=T, label_just = 'left')
  lp = lp_group(lp, 4, 'Nit_GOs', labels=F, label_just = 'left')


  lp = lp_names(lp, 2, cex=0.5)
  lp = lp_names(lp, 2, 'symbol', cex=0.5)
  lp = lp_dend(lp, 4, lwd=1.5, gap=0.4)

  if (save) {
    dev.off()
    ADsvg(file_name)
  }
}
show_test_plot(T)


## testing ADsvg function
file_name='test/ad_test.svg'
svglite::svglite(file_name, 7,7)

par(mfrow=c(2,2))

plot(1:5,1:5)
rect(3,1, 4,2)

plot(ecdf(1:10))


dev.off()
ADsvg(file_name)





# Fixing a problem --------------------------------------------------------
#
# value.df <- read.delim("/Users/jax/Desktop/📚Publications_local/2023-Weiberg_Bci_AGO/+Figures/value.txt", sep='\t')
# row.df <- read.delim("/Users/jax/Desktop/📚Publications_local/2023-Weiberg_Bci_AGO/+Figures/row.txt", sep='\t')
# column.df <- read.delim("/Users/jax/Desktop/📚Publications_local/2023-Weiberg_Bci_AGO/+Figures/column.txt", sep='\t')
#
#
# par(mar=c(2,2,5,5))
# lp <- layermap(value.df, zero_centered_colors = T,
#                cluster_rows = F,
#                column.df= column.df,
#                row.df= row.df,
#                group_gap = 0.1)
# lp <- lp_names(lp, 3)
#
# gap =0
# size=0.8
# border='black'
# col=c(up='red', down='blue', `-`='white')
# lp <- lp_annotate(lp, 4, "ago1", gap=0.5, size=size, col=col, border=border)
# lp <- lp_annotate(lp, 4, "ago2", gap=0, size=size, col=col, border=border)
# lp <- lp_annotate(lp, 4, "ago3", gap=0, size=size, col=col, border=border)
# lp <- lp_annotate(lp, 4, "ago4", gap=0, size=size, col=col, border=border)

