
# # Nitrogen Drought data ---------------------------------------------------



# show_test_plot <- function(save=F) {
#
#
#   column.df <- read.delim("test/columns.txt")
#   column.df$treatment <- factor(column.df$treatment, levels=c("Nitrate", "Drought", "ABA"))
#
#   row.df <- read.delim("test/rows.txt")
#   value.df <- read.delim("test/values.txt")
#
#   row.df$numeric <- sample(c(1:10, NA), nrow(row.df), replace=T)
#   row.df$norm <- rnorm(nrow(row.df), 1)
#
#
#
#   if (save) {
#     file_name = "test/test.svg"
#     svglite::svglite(file_name, 7, 5.4)
#   }
#
#
#   par(mar=c(7,7,5,10))
#   lp = layermap(value.df, zero_centered_colors = T,
#                     col.df=column.df, row.df=row.df,
#                     column_groups=c('treatment'), row_groups=c("PlantTFDB", 'Nit_GOs'),
#                 cluster_cols=T,
#                 group_gap = 0.1)
#
#
#
#   lp = lp_annotate(lp, 4,'numeric', group.border = 'black')
#   lp = lp_annotate(lp, 4,'norm', group.border = 'black', palette='PuOr', zlim=c(-1,1))
#   # lp = lp_names(lp, 4,'norm')
#   # lp = lp_color_legend(lp, 4)
#
#   lp = lp_annotate(lp, 2, 'ND', label="TEST", group.border = 'black')
#   lp = lp_annotate(lp, 3, 'tissue')
#   lp = lp_names(lp, 2, 'ND')
#   lp = lp_legend(lp, 4, gap=1.5)
#
#
#   # lp = lp_group(lp, 3, 'treatment', labels=F, label_just = 'left',
#   #                     col=setNames(c('seagreen','tomato'), c('Nitrate','ABA')),
#   #                     show_bounding_box = F, group_label = F)
#   # lp = lp_group_names(lp, 3, 'treatment', font=3, gap=0)
#
#
#   # lp = lp_group_pie(lp,4, 'ND')
#   # lp = lp_group_pie(lp,4, 'Vidal2020')
#   lp = lp_group_pie(lp, 3, 'tissue')
#
#   lp = lp_group_pie(lp,4, 'ND', col=c('mixed'='black'), label_just = 'left')
#   lp = lp_group_names(lp,4, 'Nit_GOs')
#   lp = lp_group_pie(lp,4, 'PlantTFDB', col=c('FALSE'='black', 'TRUE'='orange'))
#
#   lp = lp_legend(lp, 1, title.font=4)
#   lp = lp_legend(lp, 2, title.font=4)
#
#   lp = lp_legend(lp, 3, title.font=4)
#   lp = lp_legend(lp, 4, title.font=4)
#
#
#   lp = lp_group(lp, 4, 'PlantTFDB', labels=T, label_just = 'left')
#   lp = lp_group(lp, 4, 'Nit_GOs', labels=F, label_just = 'left')
#
#
#   lp = lp_annotate(lp, 3, 'tissue', label_just = 'left',
#                          col=setNames('red', 'Root'))
#
#   lp = lp_group(lp, 4, 'PlantTFDB', labels=T, label_just = 'left', srt.label=90)
#   lp = lp_group(lp, 4, 'Nit_GOs', labels=F, label_just = 'left')
#
#
#   lp = lp_names(lp, 2, cex=0.5)
#   lp = lp_names(lp, 2, 'symbol', cex=0.5)
#   lp = lp_dend(lp, 4, lwd=1.5, gap=0.4)
#
#   if (save) {
#     dev.off()
#     ADsvg(file_name)
#   }
# }


# Built-in tests ---------------------------------------------------------------

lp_example_1 = function() {
  data('mtcars')

  scaled_mtcars <- scale(mtcars)

  heatmap(scaled_mtcars, scale='row')


  val.df <- scaled_mtcars
  row.df <- mtcars


  par(mar=c(5,8,3,14))
  lp = layermap(val.df, reverse_palette = F,
                palette = 'RdYlBu',
                cluster_cols=T,
                row.df = row.df,
                row_groups = 'am')

  lp = lp_group(lp, 2, 'am')
  lp = lp_annotate(lp, 2, 'mpg', zlim=c(15,30), palette='reds')
  lp = lp_dend(lp, 2)
  lp = lp_dend(lp, 3)
  lp = lp_names(lp, 4)
  lp = lp_names(lp, 1)
  lp_plot_values(lp, alt.df=mtcars)
  lp = lp_color_legend(lp, 1, c('main','mpg'), titles=c('scaled value', 'miles per gallon'))
  lp = lp_legend(lp, 4, 'am', title='Auto?')
}

# data("iris")
#
# i.df <- iris
#
# i.df <- reshape2::dcast(i.df, Species ~ Sepal.Length, )



