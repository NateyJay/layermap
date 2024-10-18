

# Project overview --------------------------------------------------------



### To do:
# [x] allow optional inclusion of a hclust or tree to use instead of standard clustering
# [?] override NA clustering problems by giving an NA.value option
# [ ] column/row select for clustering
# [ ] plotting points inside of boxes (varying size, color, type)
# [x] allow col.df in invocation
# [x] more sensible sizing defaults for color legend (to short, thin)
# [x] auto-require essential tools (stringr, dendextend)
# [ ] cutree and kmeans implementation
# [x] error if you try to make a dend for unclustered axes

# [ ] x and y scaling
# [ ] substitute values in plot.values

### New functionality ideas:



### Function files:
# layermap.R - this description, plus the main plotting function of layermap.
# Layers.R - contains most of the functions which user-facing (layers). These are the basic plotting commands of the tool
# Utilities.R - contains supporting functions used in the plotting domain. Rarely used by the user.
# Dendrogram.R - functions to extract dendrogram structure using dendextend. Also contains plotting utility for this.
# Test.R - Some code for testing out the tool. This changes constantly as I throw new things at it.





# Primary function --------------------------------------------------------


# value.df <- data.frame(A=1:5, B=c(6:9, NA), C=c(NA, NA, 1:3))
# dend_cols = hclust(dist(t(value.df[3:5,])))
# lp = layermap(value.df, dend_cols = dend_cols, cluster_cols=T)
# lp = lp_dend(lp, 3)

# xlim=NULL; ylim=NULL;
# column.df=NULL; row.df=NULL;
# column_groups=c(); row_groups=c();
# palette='PuOr'; reverse_palette=T;
# zero_centered_colors=F;
# cluster_cols=F; cluster_rows=T;
# group_gap=0.02; border='grey25';
# dim_reference="din"; force_numeric=F
# NA.value=NULL


#' Plot layermap
#'
#' @description Backbone function for layermap. This makes the basic heatmap structure and plots it. Requires a dataframe of just numerical values with named rows/columns. Row and column attributes are provided through additional dataframes keyed to the value dataframe names. \n\n Returns a layermap object (usually named lp) which is used in lp_* plotting methods to add layers. Each one of these steps will take lp as an input and return it modified as an output.
#'
#' @param value.df numerical dataframe or matrix with column name and row names.
#' @param row.df attribute dataframe which will be used for layer plotting functions on sides 2 and 4. Rownames correspond to value.df rownames.
#' @param column.df attribute dataframe which will be used for layer plotting functions on sides 1 and 3. Rownames correspond to value.df colnames. Also can be called as col.df
#'
#' @param zlim value limits for color scaling. Defaults to the range of found values.
#' @param column_groups a vector of fields from the column.df which will be used to group across the column axis
#' @param row_groups same as column_groups, except with rows.
#'
#' @param palette option specifying the color palette used for the main heat map. Must be an hcl.pals() defined palette (defaults to PuOr).
#'
#' @param reverse_palette boolean value specifying the the orientation of the color palette. Defaults to T, which is best for PuOr.
#'
#' @param zero_centered_colors an override option to make sure the z-axis (colors) are centered on 0. This is mainly useful for unspecified zlim.
#'
#' @param color_scale an option to specify a custom color set. Accepts an unnamed vector of colors (for numerical data) but can also take named colors if plotting categorical data.
#'
#' @param cluster_cols a boolean value showing whether columns should be hierarchically clustered. Clustering is performed within groups, and may give bugs if you have group which do not respond well to clustering (i.e. size of 1)
#'
#' @param cluster_rows like above, with rows.
#'
#' @param group_gap graphical paramter for the dimension in lines by which groups are separated.
#'
#' @param border color for group box borders
#'
#' @param force_numeric an override value to treat char or factor data like numeric. This may generate NAs or lost data.
#'
#' @param main name for the overall plot.
#'
#'
#'
#' @return lp object
#' @export
#'
#' @examples
#' @examples data('mtcars')
#' @examples
#' @examples scaled_mtcars <- scale(mtcars)
#' @examples
#' @examples heatmap(scaled_mtcars, scale='row')
#' @examples
#' @examples
#' @examples val.df <- scaled_mtcars
#' @examples row.df <- mtcars
#' @examples
#' @examples
#' @examples par(mar=c(5,8,3,14))
#' @examples lp = layermap(val.df, reverse_palette = F,
#' @examples               palette = 'RdYlBu',
#' @examples               cluster_cols=T,
#' @examples               row.df = row.df,
#' @examples               row_groups = 'am')
#' @examples
#' @examples lp = lp_group(lp, 2, 'am')
#' @examples lp = lp_annotate(lp, 2, 'mpg', zlim=c(15,30), palette='reds')
#' @examples lp = lp_dend(lp, 2)
#' @examples lp = lp_dend(lp, 3)
#' @examples lp = lp_names(lp, 4)
#' @examples lp = lp_names(lp, 1)
#' @examples lp_plot_values(lp, alt.df=mtcars)
#' @examples lp = lp_color_legend(lp, 1, c('main','mpg'), titles=c('scaled value', 'miles per gallon'))
#' @examples lp = lp_legend(lp, 4, 'am', title='Auto?')
#'
layermap <- function(value.df,
                     zlim=NULL,
                     col.df=NULL,
                     row.df=NULL,
                     column_groups=c(),
                     row_groups=c(),
                     palette='PuOr',
                     reverse_palette=T,
                     zero_centered_colors=F,
                     color_scale=NULL,
                     cluster_cols=F,
                     cluster_rows=T,
                     group_gap=0.1,
                     border='grey25',
                     force_numeric=F,
                     column.df=col.df,
                     dend_cols=NULL,
                     dend_rows=NULL) {


  par(xpd=T)
  cxy = par()$cxy

  if (class(value.df)[1] == 'table') {
    header = colnames(value.df)
    mat = as.data.frame(matrix(input.df, nrow(input.df)), row.names=row.names(input.df))
    colnames(mat) = header
    value.df <- mat
  }


  if (any(c("factor", "character") %in% lapply(value.df,class))) {
    value_categories = unique(as.vector(as.matrix(value.df)))
    if (force_numeric) {
      message("Categorical data detected, but force_numeric specified. This may generate warnings/errors/NAs.")
      value.df = data.frame(lapply(value.df,as.numeric))
      data_type = 'numerical'

    } else {
      message("Value dataframe columns are not all numeric. Treating the dataframe as categorical data.")
      data_type = 'categorical'

    }
    message(stringr::str_glue("{length(value_categories)} categories found"))
  } else {
    data_type = 'numerical'
  }


  if (data_type == 'categorical' & (cluster_cols | cluster_rows)) {
    message('warning: clustering will not occur with categorical data')
    cluster_cols = F
    cluster_rows = F
  }




  df <- as.data.frame(value.df)




  if (length(column_groups) > 0 & is.null(column.df)) {
    stop("column groups given with no column.df")
  }

  if (length(row_groups) > 0 & is.null(row.df)) {
    stop("row groups given with no row.df")
  }


  if (is.null(row.df)) {
    row.df <- data.frame(row.names=rownames(value.df))
  } else {
    row.df <- as.data.frame(row.df)
  }

  if (is.null(column.df)) {
    column.df <- data.frame(row.names=colnames(value.df))
  } else {
    column.df <- as.data.frame(column.df)
  }



  if (!setequal(row.names(row.df), row.names(value.df))) {
    stop("rownames(row.df) does not match rownames(value.df)")
  }

  if (!setequal(rownames(column.df), colnames(value.df))) {
    stop("rownames(row.df) does not match colnames(value.df)")
  }


  if (any(!column_groups %in% colnames(column.df))) {
    bad_names <- paste(column_groups[which(!column_groups %in% colnames(column.df))], collapse=', ')
    stop(stringr::str_c("column_groups not found in column.df header -> ", bad_names))
  }

  if (any(!row_groups %in% colnames(row.df))) {
    bad_names <- paste(row_groups[which(!row_groups %in% colnames(row.df))], collapse=', ')
    stop(stringr::str_c("row_groups not found in row.df header -> ", bad_names))
  }

  for (g in column_groups) {
    if (class(column.df[[g]]) == 'logical') {
      column.df[[g]][column.df[[g]] == F] <- 'False'
      column.df[[g]][column.df[[g]] == T] <- 'True'
    }
  }

  for (g in row_groups) {
    if (class(row.df[[g]]) == 'logical') {
      row.df[[g]][row.df[[g]] == F] <- 'False'
      row.df[[g]][row.df[[g]] == T] <- 'True'
    }
  }

  row.df <- row.df[row.names(df),, drop=F]
  column.df <- column.df[names(df),, drop=F]

  # column.df[,column_groups] <- as.character(column.df[,column_groups])
  # row.df[,row_groups] <- as.character(row.df[,row_groups])


  groups = list()
  if (!is.null(row.df) & length(row_groups) > 0) {
    groups$rows <- row.df[,row_groups,drop=F]

  } else {
    groups$rows <- data.frame(row.names=rownames(df), group_order = rep(1, nrow(df)))
  }

  if (!is.null(column.df) & length(column_groups) > 0) {
    groups$cols <- column.df[, column_groups,drop=F]

  } else {
    groups$cols <- data.frame(row.names=colnames(df), group_order = rep(1, ncol(df)))

  }




  df <- df[match(rownames(groups$rows), rownames(df)),
           match(rownames(groups$cols), colnames(df))]


  order_by_groups <- function(gr) {
    if ("group_order" %in% colnames(gr)) {
      return(gr)
    }
    for (col in rev(unique(colnames(gr)))) {
      gr <- gr[order(gr[[col]]),, drop=F]
    }

    gr$group_order <- apply(gr, 1, paste, collapse=";")

    gr$group_order <- match(gr$group_order, unique(gr$group_order))

    return(gr)
  }



  order_by_cluster <- function(fun.df, margin, groups, cluster, dend) {
    clusters=list()

    if (margin == 1) {
      gr = groups$rows
      mat = as.matrix(fun.df)
    } else {
      gr = groups$cols
      mat = t(fun.df)
    }

    if (!is.null(dend) & max(gr$group_order) > 1 ) stop("Error: cannot include user-provided dendrograms or clustering when grouping a margin.")

    for (g in unique(gr$group_order)) {
      f = gr$group_order %in% g


      if (sum(f) == 1) {
        clusters[[g]] = F
        gr[,'cluster_order'] <- 1

      } else {
        if (cluster == T) {
          if (is.null(dend)) {
            d = mat[f,, drop=F]
            d = dist(d)
            d = hclust(d)
          } else {
            d = dend
          }
          clusters[[g]] = d
          label_order = d$labels[d$order]

          gr[label_order,'cluster_order'] <- 1:sum(f)
        } else {

          gr[,'cluster_order'] <- 1:length(f)
        }

      }

    }

    if (margin==2) {
      groups$cols = gr
      groups$col_clusters = clusters
    } else {
      groups$rows = gr
      groups$row_clusters = clusters
    }
    return(groups)
  }



  ## finding orders for groups and clusters
  groups$rows <- order_by_groups(groups$rows)
  groups$cols <- order_by_groups(groups$cols)



  groups <- order_by_cluster(df, 1, groups, cluster=cluster_rows, dend_rows)
  groups <- order_by_cluster(df, 2, groups, cluster=cluster_cols, dend_cols)

  ## reordering if clustering is selected
  if (cluster_rows) groups$rows <- groups$rows[order(groups$rows$group_order, groups$rows$cluster_order),]
  if (cluster_cols) groups$cols <- groups$cols[order(groups$cols$group_order, groups$cols$cluster_order),]


  ## getting plotting dimensions

  # get_max <- function(base, gi, g) {
  #   # base = nrow(df)
  #   # gi = max(groups$rows$group_order)
  #   # g = group_gap
  #
  #   ymax = base / (1-(gi * g))
  #   return(ymax)
  # }



  convertInches <- function(df, x, y){
    per_inch = c(ncol(df), nrow(df)) / par()$pin

    return(c(x,y) / per_inch)

  }

  inchesToCoords <- function(df, x=NULL, y=NULL) {
    per_inch = c(ncol(df), nrow(df)) / par()$pin

    if (!is.null(x)) {
      out = x * per_inch[1]

    } else if (!is.null(y)) {
      out = y * per_inch[2]

    } else {
      stop('must give x or y')
    }
    return(out)
  }


  gap.x <- inchesToCoords(df, x=group_gap)
  gap.y <- inchesToCoords(df, y=group_gap)

  ymax = nrow(df) + gap.y * (max(groups$rows$group_order)-1)
  xmax = ncol(df) + gap.x * (max(groups$cols$group_order)-1)

  #
  #   ymax <- get_max(nrow(df),
  #                   max(groups$rows$group_order)-1,
  #                   gap.y)
  #
  #   xmax <- get_max(ncol(df),
  #                   max(groups$cols$group_order)-1,
  #                   gap.x)

  # ymax <- (nrow(df)-1) + max(groups$rows$group_order)-1 * group_gap
  # xmax <- ncol(df) + max(groups$cols$group_order)-1 * group_gap * din_ratio


  # gap.x = xmax * group_gap
  # gap.y = lp_convert_visible(df, x=group_gap)
  # gap.y = ymax * group_gap
  # gap.x = xmax * group_gap*din_ratio


  ## getting xy coordinates
  groups$rows$yi <- (1:nrow(groups$rows))-1
  groups$rows$y <- groups$rows$yi + (groups$rows$group_order-1)*gap.y

  groups$cols$xi <- (1:nrow(groups$cols))-1
  groups$cols$x <- groups$cols$xi + (groups$cols$group_order-1)*gap.x


  ## matching value data frame to group/cluster orders
  df = df[match(rownames(groups$rows), rownames(df)),]
  df = df[, match(rownames(groups$cols), colnames(df))]



  ## colorizing heatmap

  m.df <- df
  m.df$rows <- rownames(m.df)
  m.df <- reshape2::melt(m.df, id.vars='rows')
  colnames(m.df)[2] <- 'cols'

  if (data_type == "numerical") {
    message('numeric coloring')

    if (is.null(color_scale)){
      color_n = 100
      color_scale = hcl.colors(color_n, palette=palette, rev=reverse_palette)

    } else {
      color_n = length(color_scale)
    }


    if (!is.null(zlim)) {
      if (zero_centered_colors) {
        message("warning: zero_centered_colors overrided due to zlim inclusion")
      }
    } else if (zero_centered_colors) {

      max_dist_from_zero = max(abs(m.df$value), na.rm=T)
      zlim = c(max_dist_from_zero * -1, max_dist_from_zero)
    } else {
      zlim = c(min(m.df$value, na.rm=T), max(m.df$value, na.rm=T))
    }

    message(stringr::str_glue("zlim: {zlim[1]} to {zlim[2]}"))




    m.df$color_i <- round((m.df$value - zlim[1]) / (zlim[2]-zlim[1]) * (color_n-1)) +1
    m.df$color_i[m.df$color_i < 1] <- 1
    m.df$color_i[m.df$color_i > color_n] <- color_n


    m.df$color <- color_scale[m.df$color_i]

  } else if (data_type == 'categorical') {

    message('categorical coloring')

    not_in_colorscale = value_categories[which(!value_categories %in% names(color_scale))]

    if (is.null(color_scale)) {
      color_scale = c()
    }

    color_scale <- c(color_scale,
                     setNames(hcl.colors(length(not_in_colorscale),palette=palette, rev=reverse_palette), not_in_colorscale))
    m.df$color <- color_scale[m.df$value]

  }





  m.df$y <- groups$rows$y[match(m.df$rows, rownames(groups$rows))]
  m.df$x <- groups$cols$x[match(m.df$cols, rownames(groups$cols))]


  ## getting id's for each box
  m.df$box.x = groups$cols$group_order[match(m.df$cols, rownames(groups$cols))]
  m.df$box.y = groups$rows$group_order[match(m.df$rows, rownames(groups$rows))]
  m.df$box <- stringr::str_c(m.df$box.x, m.df$box.y, sep=',')
  m.df$box.y <- NULL
  m.df$box.x <- NULL




  # xlim = c(0 - xmax * plot_margin[2], xmax + xmax * plot_margin[4])
  # ylim = c(0 - ymax * plot_margin[1], ymax + ymax * plot_margin[3])
  #
  #   xlim = c(0, max(groups$cols$x)+1)
  #   ylim = c(0, max(groups$rows$y)+1)
  # xlim = c(0, xmax)
  # ylim = c(0, ymax)

  gutter_dim = 0.0365 ## this is a magic number which removes the box gap around the plotting area... I'm not sure how it's derived or how to remove that gap any other way.
  xlim = c(xmax * gutter_dim, xmax * (1-gutter_dim))
  ylim = c(ymax * gutter_dim, ymax * (1-gutter_dim))

  ## plotting data

  plot(1,1, type='n', xlim=xlim, ylim=ylim, xlab='', ylab='', axes=F)

  rect(m.df$x, m.df$y, m.df$x+1, m.df$y+1, col=m.df$color, border=NA)
  #
  #   if (plot_values) {
  #
  #     plotval.df <- m.df
  #     plotval.df$l <- schemr::hex_to_lab(plotval.df$color)[,1]
  #     plotval.df$text_col <- 'black'
  #     plotval.df$text_col <- ifelse(plotval.df$l < 50, 'white','black')
  #
  #     row.df$y <- plotval.df$y[match(row.names(row.df), plotval.df$rows)]
  #     text(plotval.df$x+0.5,
  #          plotval.df$y+0.5,
  #          round(plotval.df$value,round.value),
  #          cex=cex.value,
  #          col=plotval.df$text_col)
  #     # text(m.df$x+0.5, m.df$y+0.5, round(m.df$value, round.value), adj=c(0.5,0.5), cex=cex.value)
  #   }

  for (box in unique(m.df$box)) {
    box.df <- m.df[m.df$box == box,]
    rect(min(box.df$x), min(box.df$y), max(box.df$x)+1, max(box.df$y)+1, border='black', col=NA)
  }

  # points(0,ymax)
  # points(xmax,ymax)
  # points(0,0)
  # points(xmax, 0)


  boundaries <- c(0, 0, ymax, xmax)

  # lines= c(0,0,0,0) - 0.5

  leg = list()
  if (class(m.df$value) == 'character') {
    leg[['main']] <- setNames(unique(m.df$color), unique(m.df$value))
  }

  out = list(xlim=xlim,
             ylim=ylim,
             zlim=zlim,
             xmax=xmax,
             ymax=ymax,
             color_scale = color_scale,
             column.df=column.df,
             row.df=row.df,
             # xrange=xrange,
             # yrange=yrange,
             gap.x=gap.x,
             gap.y=gap.y,
             cluster_rows=cluster_rows,
             cluster_cols=cluster_cols,
             groups=groups,
             plotting.df=m.df,
             boundaries = boundaries,
             legend = leg,
             color_legend = list())

  end_point = c(
    ifelse(zlim[1] > min(m.df$value, na.rm=T), "≤", ""),
    ifelse(zlim[2] < max(m.df$value, na.rm=T), "≥", "")
  )

  out$color_legend[['main']] = list(colors=color_scale, zlim=zlim, end_point=end_point)
  return(out)
}




