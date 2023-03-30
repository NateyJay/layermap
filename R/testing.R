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


source(file.path("~/Google Drive/+Code_snippets/R_functions.R"))


column.df <- read.delim("test/columns.txt")
column.df$treatment <- factor(column.df$treatment, levels=c("Nitrate", "Drought", "ABA"))

row.df <- read.delim("test/rows.txt")
value.df <- read.delim("test/values.txt")

# svglite::svglite("test/test.svg", 7, 5.4)

par(mar=c(5,7,5,10))
lr = layermap(value.df, zero_centered_colors = T,
                  column.df=column.df, row.df=row.df,
                  column_groups=c('treatment'), row_groups=c("PlantTFDB", 'Nit_GOs'),
              cluster_cols=T,
              group_gap = 0.1)
# box()

# for (l in 0:10) {
#   mtext(str_glue("line {l}"), side=4, line=l)
#   abline(v= lr$xmax + lr_line_to_coord(lr$xmax, lr$ymax, 4, l))
#   mtext(str_glue("line {l}"), side=3, line=l)
#   abline(h= lr$ymax + lr_line_to_coord(lr$ymax, lr$ymax, 3, l))
# }

lr = lr_group(lr, 3, 'treatment', labels=T, label_just = 'left',
                    col=setNames(c('seagreen','tomato'), c('Nitrate','ABA')),
                    show_bounding_box = F)


lr = lr_annotate(lr, 3, 'tissue', label_just = 'left',
                       col=setNames('red', 'Root'))

lr = lr_group(lr, 4, 'PlantTFDB', labels=T, label_just = 'left')
lr = lr_group(lr, 4, 'Nit_GOs', labels=F, label_just = 'left')


lr = lr_names(lr, 2, cex=0.5)
lr = lr_names(lr, 2, 'symbol', cex=0.5)
lr = lr_dend(lr, 4, lwd=1.5, gap=0.4)

# dev.off()
# fix_svg('test/test.svg')

# svglite::svglite("test/legend.svg", 4.9, 5.4)
# lr_legend(lr)
# dev.off()
# fix_svg('test/legend.svg')



