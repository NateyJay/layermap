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

  lp = lp_group_pie(lp,4, 'ND')
  lp = lp_group_pie(lp,4, 'Vidal2020')
  lp = lp_group_pie(lp, 3, 'tissue')

  lp = lp_group_pie(lp,4, 'ND', col=c('mixed'='black'))


  lp = lp_annotate(lp, 3, 'tissue', label_just = 'left',
                         col=setNames('red', 'Root'))

  lp = lp_group(lp, 4, 'PlantTFDB', labels=T, label_just = 'left', srt.label=90)
  lp = lp_group(lp, 4, 'Nit_GOs', labels=F, label_just = 'left')


  lp = lp_names(lp, 2, cex=0.5)
  lp = lp_names(lp, 2, 'symbol', cex=0.5)
  lp = lp_dend(lp, 4, lwd=1.5, gap=0.4)

  if (save) {
    dev.off()
    ADsvg(file_name)
  }
}
# show_test_plot(F)


## testing ADsvg function
# file_name='test/ad_test.svg'
# svglite::svglite(file_name, 7,7)
#
# par(mfrow=c(2,2))
#
# plot(1:5,1:5)
# rect(3,1, 4,2)
#
# plot(ecdf(1:10))
#
#
# dev.off()
# ADsvg(file_name)





# Fixing a problem --------------------------------------------------------
#
# value.df <- read.delim("/Users/jax/Desktop/📚Publications_local/2023-Weiberg_Bci_AGO/+Figures/value.txt", sep='\t')
# row.df <- read.delim("/Users/jax/Desktop/📚Publications_local/2023-Weiberg_Bci_AGO/+Figures/row.txt", sep='\t')
# column.df <- read.delim("/Users/jax/Desktop/📚Publications_local/2023-Weiberg_Bci_AGO/+Figures/column.txt", sep='\t')
#
# row.df$group_layer <- ifelse(row.df$only_ago2, "Yes", "No")
# par(mar=c(2,2,5,5))


# value.df <- read.delim("/Users/jax/Desktop/📚Publications_local/2023-Weiberg_Bci_AGO/+Figures/value.df", sep='\t')
# lp <- layermap(value.df)
# lp <- lp_color_legend(lp, 1)
# lp <- lp_color_legend(lp, 4)
#
# lp <- layermap(value.df, zero_centered_colors = T,
#                cluster_rows = F,
#                column.df= column.df,
#                row.df= row.df,
#                row_groups = 'group_layer',
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




# df <- data.frame(num=c(1,2,3,4,5),
#                  char=c('6','7','8','9','10'),
#                  fac=factor(c('11','12','13','11','13'), levels=c('11','12','13')),
#                  noconvert = c("a",'b','c','d','3'))
#
#
# lapply(df,class)
#
# converted = data.frame(lapply(df,as.numeric))
#
# lapply(converted,class)
# converted
#
#
# lp = layermap(df)



# adding features ---------------------------------------------------------

# value.df <- read.delim("/Users/jax/Desktop/📚Publications_local/2021-NxD_data/+Figures/test.txt")
#

# value.df  <- read.delim("/Users/jax/Desktop/📚Publications_local/2021-NxD_data/+Figures/value.txt")
# row.df    <- read.delim("/Users/jax/Desktop/📚Publications_local/2021-NxD_data/+Figures/row.txt")
# column.df <- read.delim("/Users/jax/Desktop/📚Publications_local/2021-NxD_data/+Figures/column.txt")
#
#
#  zero_centered_colors = T
# # zlim=c(-3,1.1)
# zlim=c(-3,3)
# column.df = column.df
# column_groups='inter'
# row.df = row.df
# cluster_rows = F
# cluster_cols = F
# plot_values = T
# cex.value = 0.6
# round.value = 2
#

# colors=c(up='darkred', down='darkblue', `-`='grey85')
# par(mar=c(6,8,6,8))
# lp <- layermap(value.df, zero_centered_colors = T,
#                # zlim=c(-3,1.1),
#                zlim=c(-3,3),
#                column.df = column.df,
#                column_groups='inter',
#                row.df = row.df,
#                cluster_rows = F,
#                cluster_cols = F,
#                plot_values = T,
#                cex.value = 0.6,
#                round.value = 2)
# lp <- lp_names(lp, 1)
# lp <- lp_blank(lp, 4, size=3)
# lp <- lp_annotate(lp, 4, 'G', size=0.6, type='points', cex.point=2, col=colors)
# lp <- lp_annotate(lp, 4, 'N', size=0.6, type='points', cex.point=2)
# lp <- lp_annotate(lp, 4, 'D', size=0.6, type='points', cex.point=2)
# # lp <- lp_names(lp, 4)
# lp <- lp_dend(lp, 2, size=2, lwd=1.5)
# lp <- lp_dend(lp, 3, size=2, lwd=1.5)
# lp <- lp_color_legend(lp, 4)
# lp <- lp_group(lp, 3, 'inter')











# masking problem with ADsvg ----------------------------------------------

#
# x <- c(sample(1:100, 50, replace=T), sample(34:67, 80, replace=T))
#
# file_name = "test.svg"
# svglite::svglite(file_name, 5,5)
# par(xpd=T)
# plot(density(x), xlim=c(20,80))
# dev.off()
# ADsvg(file_name)


