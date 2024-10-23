

# Built-in tests ---------------------------------------------------------------

#' Test example 1
#'
#' @description Makes a layermap based on the mtcars dataset
#' @export
#'
lp_example_1 = function() {
  data('mtcars')

  scaled_mtcars <- scale(mtcars)

  heatmap(scaled_mtcars, scale='row')


  val.df <- scaled_mtcars
  row.df <- mtcars


  par(mar=c(5,8,5,14))
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


#' Test example 2
#'
#' @description Makes a layermap based on the a layermap dataset of Nitrogen- and Drought-related genes in these treatments.
#' @export
#'
lp_example_2 <- function(save=F) {

  data(ND)

  value.df  = ND$values
  column.df = ND$columns
  row.df    = ND$rows

  # if (save) {
  #   file_name = "images/test.svg"
  #   svglite::svglite(file_name, 7, 5.4)
  # }


  par(mar=c(7,12,5,15))
  lp = layermap(value.df, zero_centered_colors = T,
                col.df=column.df, row.df=row.df,
                column_groups=c('treatment'), row_groups=c("PlantTFDB", 'Nit_GOs'),
                cluster_cols=T,
                group_gap = 0.1)



  lp = lp_annotate(lp, 4,'numeric', group.border = 'black', label.just = 'left')
  lp = lp_annotate(lp, 4,'norm', group.border = 'black', palette='PuOr', zlim=c(-1,1), label.just = 'left')


  # lp = lp_names(lp, 4,'norm')
  # lp = lp_color_legend(lp, 4)

  lp = lp_annotate(lp, 2, 'ND',
                   col= c(NuDd='orange',
                          mixed='white',
                          NdDd='skyblue',
                          NdDu='green',
                          NuDu='purple',
                          `ND_`='red'),
                   type='points',
                   pch=21,
                   pt.cex=1, label.just='left')
  lp = lp_names(lp, 2, 'ND', cex=0.5)


  lp = lp_annotate(lp, 3, 'tissue', palette='Temps', group.border = 'black')
  lp = lp_group_pie(lp, 3, 'tissue', palette='Temps')

  # lp = lp_group_names(lp,4, 'Nit_GOs')
  lp = lp_group(lp,4, 'PlantTFDB', col=c('FALSE'='black', 'TRUE'='orange'), label.just = 'left')




  lp = lp_names(lp, 2, cex=0.5)
  lp = lp_names(lp, 2, 'symbol', cex=0.5)
  lp = lp_dend(lp, 4, lwd=1.5, gap=0.4)

  lp = lp_legend(lp, 4, "tissue", gap=1.5)

  lp = lp_dend(lp, 1)
  lp = lp_color_legend(lp, 1, titles='Log2 fold change')

  # if (save) {
  #   dev.off()
  #   ADsvg(file_name)
  # }
}

# lp_example_2()



#' Test example 3
#'
#' @description Makes a layermap based on the a state database. This showcases multi-divisions and separated clustering compared to clustered grouping of axes.
#' @export
#'

lp_example_3 <- function() {

  data(state)

  val.df <- as.data.frame(state.x77)
  val.df$latitude = state.center$x
  val.df$longitude = state.center$y

  row.df <- data.frame(row.names=state.name,
                       division=state.division,
                       region=state.region,
                       area=state.area,
                       abb=state.abb)
  col.df <- data.frame(row.names=colnames(val.df),
                       category=c(rep('demographics', 6),
                                  "weather",
                                  rep("geography",3)))

  ## scaling plotable values by columns

  val.df <- scale(val.df)

  par(mar=c(4,16,8,9))
  lp <- layermap(val.df,
                 row.df=row.df,
                 col.df=col.df,
                 palette='blues',
                 column_groups = 'category',
                 row_groups = c('region', 'division'),
                 zlim=c(-2,2))

  lp <- lp_names(lp, 2, 'abb', cex=0.65)
  lp <- lp_names(lp, 2, cex=0.65)
  lp <- lp_group(lp, 2, 'region')
  lp <- lp_group(lp, 2, 'division', plot_names=F, palette='Berlin')
  lp <- lp_group_names(lp, 2, 'division')

  lp <- lp_names(lp, 3)
  lp <- lp_group_names(lp, 3, 'category', srt=0, gap=3)


  lp <- lp_dend(lp, 4)

  lp <- lp_color_legend(lp, 1, titles='z-scaled value')
  lp <- lp_legend(lp, 4, 'region', cex=0.7, gap=1)

  par(mar=c(4,10,10,8))
  lp <- layermap(val.df,
                 row.df=row.df,
                 col.df=col.df,
                 palette='reds',
                 cluster_cols = T,
                 cluster_rows = T)



  lp <- lp_names(lp, 2)
  lp <- lp_names(lp, 3)
  lp <- lp_names(lp, 4, 'abb')


  lp <- lp_dend(lp, 3, gap=3)
  lp <- lp_dend(lp, 4)

  lp <- lp_color_legend(lp, 1, titles='Z-scaled value')


}
# lp_example_3()



#' Test example 4
#'
#' @description Makes a layermap based on the a fivethirtyeight airline_safety dataset. This is meant to show more examples of annotation layers.
#' @export
#'

lp_example_4 <- function() {


  library(fivethirtyeight)

  a.df <- as.data.frame(airline_safety)
  rownames(a.df) <- a.df$airline

  dim(a.df)
  # 50 airlines with several attributes.


  val.df <- a.df[, c("incidents_85_99", "fatal_accidents_85_99", "incidents_00_14", "fatal_accidents_00_14" )]

  row.df <- a.df[,c("incl_reg_subsidiaries", "avail_seat_km_per_week", "fatalities_85_99", "fatalities_00_14")]
  row.df$avail_seat_km_per_week <- row.df$avail_seat_km_per_week / 10^9

  row.df$marked <- NA
  row.df$marked[row.names(row.df) %in% c("LAN Airlines", "TAM", "United / Continental", "American")] <- 'my_airlines'

  col.df <- data.frame(row.names=names(val.df),
                       type=rep(c('incidents', 'fat. accidents'), each=2),
                       window=rep(c('1985-99', '2000-14'), 2))


  par(mar=c(5, 12, 4, 10))
  lp <- layermap(val.df,
                 col.df=col.df,
                 row.df=row.df,
                 palette='agsunset',
                 zlim=c(0,20),
                 column_groups = 'type')

  # top
  lp <- lp_group(lp, 3, 'type', plot_label=F)


  # left
  lp <- lp_annotate(lp, 2, 'marked', col=c(my_airlines='red'), type='points', pch=21, plot_label = F)
  lp <- lp_names(lp, 2)


  # right
  lp <- lp_annotate(lp, 4, "fatalities_85_99", zlim=c(0,500), label='1985-99')
  lp <- lp_annotate(lp, 4, "fatalities_00_14", zlim=c(0,500), label='2000-14')
  lp <- lp_annotate(lp, 4, "avail_seat_km_per_week", label='SKPW', palette='teal', type='points')
  lp <- lp_dend(lp, 4, size=4, lwd=1.5)

  # bottom
  lp <- lp_color_legend(lp, 1, c('main','fatalities_85_99', 'avail_seat_km_per_week'),
                        gap_p=0.2,
                        titles=c('Count', "Fatalities", 'SKPW'))


  # cells
  lp_plot_values(lp)

}
# lp_example_4()


lp_example_5 <- function() {
  air.df <- as.data.frame(matrix(AirPassengers, ncol=12, byrow=T), row.names = 1949:1960)
  names(air.df) <- month.abb

  lp <- layermap(air.df, zlim=c(0, 500))
  lp <- lp_dend(lp, 4)
  lp <- lp_names(lp, 3)
  lp <- lp_names(lp, 2)
  lp <- lp_color_legend(lp, 1, titles ='Passenger count')
}




