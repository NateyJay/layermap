

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
#' @description Backbone function for layermap. This makes the basic heatmap structure and plots it. Requires a dataframe of just numerical values with named rows/columns. Row and column attributes are provided through additional dataframes keyed to the value dataframe names.
#'
#' @param value.df numerical dataframe or matrix with column name and row names.
#' @param row.df attribute dataframe which will be used for layer plotting functions on sides 2 and 4. Rownames correspond to value.df rownames.
#' @param column.df attribute dataframe which will be used for layer plotting functions on sides 1 and 3. Rownames correspond to value.df colnames.
#'
#' @param zlim value limits for color scaling. Defaults to the range of found values.
#' @param column_groups a vector of fields from the column.df which will be used to group accross the column axis
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
#' @param main
#'
#'
#'
#' @return lp object
#' @export
#'
#' @examples
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

  # require(stringr)
  # require(dendextend)

  # par(mar=c(0.3,0.3,0.3,0.3))

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
  }

  if (is.null(column.df)) {
    column.df <- data.frame(row.names=colnames(value.df))
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

  get_max <- function(base, gi, g) {
    # base = nrow(df)
    # gi = max(groups$rows$group_order)
    # g = group_gap

    ymax = base / (1-(gi * g))
    return(ymax)
  }



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
  colnames(m.df)[2] <- 'column.df'

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
  m.df$x <- groups$cols$x[match(m.df$column.df, rownames(groups$cols))]


  ## getting id's for each box
  m.df$box.x = groups$cols$group_order[match(m.df$column.df, rownames(groups$cols))]
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
             legend = list(),
             color_legend = list())

  end_point = c(
    ifelse(zlim[1] > min(m.df$value, na.rm=T), "≤", ""),
    ifelse(zlim[2] < max(m.df$value, na.rm=T), "≥", "")
  )

  out$color_legend[['']] = list(palette=palette, reverse_palette=reverse_palette, zlim=zlim, end_point=end_point)
  return(out)
}





# col= NULL; palette="Zissou 1"; size=1; gap=0.4; cex=0.8; show_bounding_box=F; label_just='right'; labels=T; cex.label=0.8; group_label=T


#' Plot group layer
#'
#' @description Function for plotting a group layer based on column or row attributes.
#'
#' @param lp - layermap object .
#' @param side - value for which side of the plot to apply the layer (1-bottom, 2-left, 3-top, 4-right).
#' @param attribute - name for the attribute which will be plotted in the layer. For group, this must be defined in column_groups or row_groups.
#' @param col - named color vector, where the names are conditions found in the attribute.
#' @param palette - hcl.colors palette to fill in unnamed conditions colors.
#' @param prop - the proportion of plotting space reserved for this layer
#' @param layer_just - the justification side for a layer label (right or left).
#' @param cex_label - cex value for label characters.
#' @param labels - logical for whether group labels should be plotted.
#'
#' @return layermap object
#' @export
#'
#' @examples
lp_group <- function(lp, side, attribute, col= NULL, palette="Zissou 1", size=1, gap=0.4, cex=0.8,
                     show_bounding_box=F, label_just='right', labels=T, cex.label=0.8, group_label=T) {

  if (labels) {
    str_multiplier = 2

    text.gap = strheight("G", cex=cex.label) * str_multiplier * cex.label

    if (side %in% c(2,4)) {
      # text.gap = lp_rotate(text.gap)
      text.gap = text.gap * par()$cxy[1] / par()$cxy[2] * 1.33
    }

  } else {text.gap = 0}


  list2env(lp_boundaries(lp, side, size, gap, text.gap, show_bounding_box = show_bounding_box), environment())

  if (side == 1) {
    box.y1 = xy0
    box.y2 = xy1
    text.y = xy0 - text.gap * 0.6
    gr = lp$groups$cols

  } else if (side == 2) {
    box.x1 = xy0
    box.x2 = xy1
    text.x = xy0 + text.gap * 0.6
    gr = lp$groups$rows

  } else if (side == 3) {
    box.y1 = xy0
    box.y2 = xy1
    text.y = xy0 - text.gap * 0.6
    gr = lp$groups$cols

  } else if (side == 4) {
    box.x1 = xy0
    box.x2 = xy1
    text.x = xy0 + text.gap * 0.6
    gr = lp$groups$rows

  }



  conditions = unique(gr[[attribute]])


  col = lp_colorize(col, conditions, palette)

  ## Finding groups which are identical and consecutive
  last_cond = ''
  gis = c()
  clumped_groups = list()
  for (gi in unique(gr$group_order)) {
    cond = unique(gr[gr$group_order == gi, attribute])

    if (cond == last_cond) {
      gis = c(gis, gi)

    } else {
      clumped_groups[[length(clumped_groups)+1]] <- gis
      gis = c(gi)

    }
    last_cond = cond
  }
  clumped_groups[[length(clumped_groups)+1]] <- gis


  ## Plotting
  if (side %in% c(1,3)) {
    x_vec = gr$x
    y_vec = c(box.y1, box.y2)
    half.y1 = mean(c(box.y1, box.y2)) + abs(box.y2-box.y1)*0.2
    half.y2 = mean(c(box.y1, box.y2)) - abs(box.y2-box.y1)*0.2

    # half.y = mean(c(box.y1, box.y2))
    for (clump_i in seq_along(clumped_groups)) {
      clump = clumped_groups[[clump_i]]

      g.df <- gr[gr$group_order %in% clump,]
      cond = g.df[[attribute]][1]
      # segments(min(g.df$x), half.y, max(g.df$x)+1, half.y, col='black', lwd=3, lend=1)
      # segments(min(g.df$x), half.y, max(g.df$x)+1, half.y, col=col[cond], lwd=2.5, lend=1)
      rect(min(g.df$x, na.rm=T), box.y1, max(g.df$x, na.rm=T)+1, box.y2, col=col[cond])
      if (labels) { text(mean(c(min(g.df$x), max(g.df$x)+1)), text.y, cond, cex=cex.label) }

      # for (gi in clump){
      #   g.df <- gr[gr$group_order == gi,]
      #   rect(min(g.df$x), box.y1, max(g.df$x)+1, box.y2, col=col[as.vector(cond)])
      # abline(h=c(box.y1, box.y2))
      # }
    }

  } else if (side %in% c(2,4)) {
    y_vec = gr$y
    x_vec = c(box.x1, box.x2)
    half.x1 = mean(c(box.x1, box.x2)) + abs(box.x2-box.x1)*0.2
    half.x2 = mean(c(box.x1, box.x2)) - abs(box.x2-box.x1)*0.2
    # half.x = mean(c(box.x1, box.x2))

    for (clump_i in seq_along(clumped_groups)) {
      clump = clumped_groups[[clump_i]]

      g.df <- gr[gr$group_order %in% clump,]
      cond = g.df[[attribute]][1]
      # segments(half.x, min(g.df$y), half.x, max(g.df$y)+1, col='black', lwd=3, lend=1)
      # segments(half.x, min(g.df$y), half.x, max(g.df$y)+1, col=col[cond], lwd=2.5, lend=1)
      rect(box.x1, min(g.df$y, na.rm=T), box.x2, max(g.df$y, na.rm=T)+1, col=col[cond])
      if (labels) { text(text.x, mean(c(min(g.df$y), max(g.df$y)+1)), srt=90, cond, cex=cex) }

      # for (gi in clump){
      #   g.df <- gr[gr$group_order == gi,]
      #   rect(box.x1, min(g.df$y), box.x2, max(g.df$y)+1, col=col[cond])
      #   # abline(v=c(box.x1, box.x2))
      # }
    }
  }

  if (group_label) {
    lp_label(lp, x_vec, y_vec, side=side, text=attribute, just=label_just, cex=cex.label)
  }

  lp$legend[[attribute]] = col
  lp$boundaries <- boundaries
  return(lp)
}



# a.df=NULL; col=NULL; size=1; gap=0.4;
# palette='Viridis';
# show_bounding_box=F; type='rect';
# label_just='right'; cex.label=0.8; border=NA;
# cex.point=1; pch=19; lwd=1;
# show_label=T
# zlim=NULL; reverse_palette = F; zero_centered_colors = F


# a.df=NULL; col=NULL; size=1; gap=0.4; palette='Viridis'
# show_bounding_box=F; type='rect'; label_just='right'; cex.label=0.8

#' Plot annotation layer
#'
#' @description Function for plotting an annotation layer based on column or row attributes.
#'
#' @param lp - layermap object .
#' @param side - value for which side of the plot to applp the layer (1-bottom, 2-left, 3-top, 4-right).
#' @param attribute - name for the attribute which will be plotted in the layer.
#' @param col - named color vector, where the names are conditions found in the attribute.
#' @param type - what to plot as the annotation ('rect' or 'points).
#' @param palette - hcl.colors palette to fill in unnamed conditions colors.
#' @param prop - the proportion of plotting space reserved for this layer
#' @param layer_just - the justification side for a layer label (right or left).
#' @param cex.label - cex value for label characters.
#'
#' @return layermap object
#' @export
#'
#' @examples
lp_annotate <- function(lp, side, attribute, a.df=NULL, col=NULL, size=1, gap=0.4,
                        palette='Viridis', colors=NULL,
                        show_bounding_box=F, type='rect', label=attribute,
                        label_just='right', cex.label=0.8, border=NA, group.border=NA,
                        cex.point=1, pch=19, bg=NA, lwd=1,
                        show_label=T, zlim=NULL, reverse_palette=F, zero_centered_colors=F) {

  list2env(lp_boundaries(lp, side, size, gap, show_bounding_box = show_bounding_box), environment())

  message(attribute)

  if (is.null(a.df)) {
    if (side %in% c(1,3)) {
      a.df <- lp$column.df
    } else if (side %in% c(2,4)) {
      a.df <- lp$row.df
    }
  }

  # attribute=names(a.df)[1]


  if (side %in% c(2,4)) {
    gr = lp$groups$rows

    gr$y0=gr$y
    gr$y1=gr$y+1
    gr$x0=xy1
    gr$x1=xy0
    gr$ymean=gr$y+0.5
    gr$xmean=mean(c(xy1,xy0))

  } else if (side %in% c(1,3)) {
    gr = lp$groups$cols

    gr$x0=gr$x
    gr$x1=gr$x+1
    gr$y0=xy1
    gr$y1=xy0
    gr$xmean=gr$x+0.5
    gr$ymean=mean(c(xy1,xy0))
  }


  print(head(a.df))

  conditions = unique(a.df[[attribute]])

  if (!is.null(colors)) {

    gr$col <- a.df[match(rownames(gr), rownames(a.df)), colors]

  } else {

    if (is.numeric(conditions)) {
      if (is.null(zlim)) {
        zlim=c(min(a.df[[attribute]], na.rm = T), max(a.df[[attribute]], na.rm = T))
      }
      col = vector_to_colors(a.df[[attribute]], zlim=zlim, palette=palette,
                             reverse_palette = reverse_palette,
                             zero_centered_colors = zero_centered_colors)

      gr$col <- col[match(rownames(gr), rownames(a.df))]


    } else {
      if (is.null(col)) {
        col = lp_colorize(col, conditions, palette)
      }

      gr$attribute_value <- a.df[match(rownames(gr), rownames(a.df)),attribute]
      gr$col <- col[gr$attribute_value]
      #
      #     print(head(a.df))
      #     print(head(gr))
      #     print(col)

    }
  }



  if (type == 'points' & pch %in% c(21:25)) {
    gr$bg = gr$col
    gr$col = 'black'

  }


  if (type == 'rect') {
    rect(gr$x0, gr$y0, gr$x1, gr$y1, col=gr$col, border=border,
         lwd=lwd)
  } else if (type == 'points') {
    points(gr$xmean, gr$ymean,
           bg=gr$bg, col=gr$col, pch=pch, cex=cex.point)
  }

  if (!is.na(group.border)) {
    for (group in gr$group_order) {
      g = gr[gr$group_order == group,]
      rect(min(g$x0), min(g$y0), max(g$x1), max(g$y1), border=group.border, col=NA)
    }
  }


  if (show_label) {lp_label(lp, gr$xmean, gr$ymean, side=side, text=label, just=label_just, cex=cex.label)
  }
  lp$boundaries <- boundaries

  if (is.numeric(conditions)) {

    val = a.df[[attribute]]
    end_point = c(
      ifelse(zlim[1] > min(val, na.rm=T), "≤", ""),
      ifelse(zlim[2] > max(val, na.rm=T), "≥", "")
    )

    lp$color_legend[[attribute]] = list(palette=palette, reverse_palette=reverse_palette, zlim=zlim, end_point=end_point)
  } else {
    lp$legend[[attribute]] = col
  }
  return(lp)
}









#' Plot color gradient legend
#'
#' @description Makes a simple color gradient legend corresponding to the main heatmap.
#'
#' @param lp - layermap object
#' @param add - logical for whether this should plot a new window. Experimental.
#'
#' @return
#' @export
#'
#' @examples

lp_color_legend <- function(lp, side, attributes=NULL, size=0.5, gap=1.5, size_p = 0.4, gap_p=0.05, ratio=4, adj=0, round=1,
                            cex=0.6, title.cex=NULL,
                            main='') {

  if (is.null(title.cex)) {
    title.cex = cex
  }

  col_n = 50
  leg <- lp$color_legend
  # lines = length(leg) *2

  list2env(lp_boundaries(lp, side, size, gap, show_bounding_box = F), environment())

  if (is.null(attributes)){
    attributes = names(leg)
  }

  if (side %in% c(1,3)) {

    y0 = xy0
    y1 = xy1

    w = lp$xmax * size_p
    g = lp$xmax * gap_p

    leg.df <- data.frame(name=attributes, y0=y0, y1=y1, w = w)
    # leg.df$x.text = cumsum(leg.df$w) - leg.df$w
    leg.df$x0 = cumsum(leg.df$w) - leg.df$w + g
    leg.df$x1 = leg.df$x0 + leg.df$w - g - g
    leg.df$x.text <- apply(leg.df[,c('x0','x1')], 1, mean)


  } else if (side %in% c(2,4)) {

    x0 = xy0
    x1 = xy1

    w = lp$ymax * size_p
    g = lp$ymax * gap_p

    leg.df <- data.frame(name=attributes, x0=x0, x1=x1, w = w)
    leg.df$y.text = lp$ymax - cumsum(leg.df$w) + w
    leg.df$y0 = leg.df$y.text - g
    leg.df$y1 = leg.df$y0 - leg.df$w + g + g


  }

  leg.df$name[1] <- main
  names(leg)[1] <- main

  for (n in leg.df$name) {
    leg_i = match(names(leg), n)
    df = leg.df[leg.df$name == n,]
    c = 1:col_n
    col = hcl.colors(col_n, leg[[leg_i]]$palette, rev=!leg[[leg_i]]$reverse_palette)
    zlim = leg[[leg_i]]$zlim
    ep   = leg[[leg_i]]$end_point


    if (side %in% c(2,4)) {
      text(df$x1, df$y.text, n, adj=c(0,1), cex=title.cex, font=3)
      rect(df$x0, df$y0 - (df$y0 - df$y1)*(c-1)/length(c), df$x1, df$y1,
           col=rev(col), border=NA)
      rect(df$x0, df$y0, df$x1, df$y1, lwd=1)

      text(df$x0, c(df$y0,df$y1),
           stringr::str_c(ep, round(zlim, round)),
           pos=side, cex=cex, offset=0.25)

    } else if (side %in% c(1,3)) {


      text(df$x.text, df$y1, n, adj=c(0.5,-0.5), cex=title.cex, font=3)
      rect(df$x0 - (df$x0 - df$x1)*(c-1)/length(c), df$y0, df$x1, df$y1,
           col=rev(col), border=NA)
      rect(df$x0, df$y0, df$x1, df$y1, lwd=1)

      text(c(df$x0,df$x1), df$y0, stringr::str_c(ep, round(zlim, round)), pos=side, cex=cex, offset=0.25)

    }

  }

  lp$boundaries <- boundaries
  return(lp)
}




# attributes=NULL; cex=0.6; gap=0.4
# title.col='black'; title.cex=NULL; title.font=3

#' Plot simple categorical legend
#'
#' @description Makes a simple color legend for a layermap object.
#'
#' @param lp - layermap object
#' @param side - value for which side of the plot to apply the layer (1-bottom, 2-left, 3-top, 4-right).
#' @param attribute - name for the attributes which will be plotted in the layer. Defaults to plotting all of them.
#'
#' @return
#' @export
#'
#' @examples
lp_legend <- function(lp, side, attributes=NULL, cex=0.6, gap=0.4,
                      title.col='black', title.cex=NULL, title.font=3) {

  if (is.null(title.cex)) {
    title.cex = cex
  }

  leg <- lp$legend
  lines = length(unlist(leg)) + length(leg) + 3


  list2env(lp_boundaries(lp, side, size=0, gap, text.gap=0, show_bounding_box = F), environment())

  # par(mar=rep(0.3,4))

  if (side == 1) {
    y=xy1
    x=0
    xjust=0
    yjust=1

  } else if (side == 2) {
    x=xy0
    xjust=1
    yjust=1
    y=lp$ymax

  } else if (side == 3) {
    y=xy1
    xjust=0
    yjust=0
    x=0

  } else if (side == 4) {
    x=xy1
    xjust=0
    yjust=1
    y=lp$ymax

  }

  if (is.null(attributes)) {
    attributes = names(leg)
  }

  extent = lp$boundaries[side]

  for (a in attributes) {

    l = legend(x,y, names(leg[[a]]), fill=leg[[a]], cex=cex, xjust=xjust, yjust=yjust,
               title=a,
               title.col=title.col, title.cex=title.cex, title.font=title.font)


    if (side == 1) {
      if (l$rect$top - l$rect$h < extent) extent = l$rect$top - l$rect$h

    } else if (side == 2) {
      if (l$rect$left < extent) extent = l$rect$left

    } else if (side == 3) {
      if (l$rect$top > extent) extent = l$rect$top

    } else if (side == 4) {
      if (l$rect$left + l$rect$w > extent) extent = l$rect$left + l$rect$w

    }

    if (side %in% c(1,3)) {
      x = x + l$rect$w + lp$gap.x
    } else {
      y = y - l$rect$h - lp$gap.y
    }
  }


  boundaries[side] = extent
  lp$boundaries <- boundaries
  return(lp)
}


# names=NULL; size=1; gap=0.4; autobox=T; cex=0.8
# show_bounding_box = F; just='auto'; col='black'; font=1

#' Plot name layer
#'
#' @description Function for plotting an name layer based on column or row attributes.
#'
#' @param lp - layermap object .
#' @param side - value for which side of the plot to apply the layer (1-bottom, 2-left, 3-top, 4-right).
#' @param attribute - name for the attribute which will be plotted in the layer. Default is F, which plots the rownames.
#'
#' @return layermap object
#' @export
#'
#' @examples
lp_names <- function(lp, side, attribute=F, names=NULL, size=1, gap=0.4, autobox=T, cex=0.8,
                     show_bounding_box = F, just='auto', col='black', font=1) {


  if (side %in% c(1,3)) {
    gr = lp$groups$cols
    ar = lp$column.df
  } else if (side %in% c(2,4)) {
    gr = lp$groups$rows
    ar = lp$row.df
  }

  if (!is.null(names)) {
    gr[[attribute]] <- names[match(row.names(gr), row.names(names)), attribute]
  }


  if (attribute == F) {
    labels = row.names(gr)

    # }  else if (!attribute %in% colnames(names)) {
    #   stop(paste(attribute, 'not found in column names'))

  } else {
    labels = ar[match(row.names(gr), row.names(ar)), attribute]
  }


  if (autobox) {
    if (side %in% c(2,4)) {
      srt=0
    } else {
      srt=90
    }
    text.restriction=max(strwidth(labels, cex=cex), na.rm=T)
    if (srt == 90) {
      # text.restriction = lp_rotate(text.restriction)
      text.restriction = text.restriction * par()$cxy[1] / par()$cxy[2] * 1.33
    }

  } else {
    text.restriction=F

  }

  list2env(lp_boundaries(lp, side, size, gap, show_bounding_box = show_bounding_box, text.restriction=text.restriction), environment())


  if (just == 'auto') {
    if (side %in% c(1,2)) {
      just = 'right'
    } else {
      just = 'left'
    }
  }


  if (just == 'right') {
    if (side %in% c(1,3)) {
      text(gr$x+0.5, xy1, labels, cex=cex, srt=90, adj=c(1,0.5), col=col, font=font)

    } else if (side %in% c(2,4)) {
      text(xy0, gr$y+0.5, labels, cex=cex, srt=0, adj=c(1,0.5), col=col, font=font)

    }
  } else if (just == 'left') {
    if (side %in% c(1,3)) {
      text(gr$x+0.5, xy0, labels, cex=cex, srt=90, adj=c(0,0.5), col=col, font=font)

    } else if (side %in% c(2,4)) {
      text(xy1, gr$y+0.5, labels, cex=cex, srt=0, adj=c(0,0.5), col=col, font=font)

    }
  }



  lp$boundaries <- boundaries
  return(lp)

}








#' Plot dendrogram layer
#'
#' @description Function for plotting an dendrogram layer. This function will only work on sides which have been clustered by hclust in the initial layermap call.
#'
#' @param lp - layermap object.
#' @param side - value for which side of the plot to apply the layer (1-bottom, 2-left, 3-top, 4-right).
#' @param prop - the proportion of plotting space reserved for this layer
#'
#' @return layermap object
#' @export
#'
#' @examples
lp_dend <- function(lp, side, size=2, gap=0.2, cutoff=T, cex=0.8,
                    show_bounding_box = F, ...) {


  if (side %in% c(1,3) & !lp$cluster_cols) {
    stop("Error: cannot make dendrogram for unclustered columns (try making the layermap with cluster_cols=T)")
  } else if (side %in% c(2,4) & !lp$cluster_rows) {
    stop("Error: cannot make dendrogram for unclustered rows (try making the layermap with cluster_rows=T)")
  }


  list2env(lp_boundaries(lp, side, size, gap, show_bounding_box = show_bounding_box), environment())

  if (side %in% c(1,3)) {
    gr = lp$groups$cols
  } else if (side %in% c(2,4)) {
    gr = lp$groups$rows
  }


  for (gi in unique(gr$group_order)) {
    g.df <- gr[gr$group_order == gi,]

    if (side %in% c(2,4)) {
      cl = lp$groups$row_clusters[[gi]]

    } else if (side %in% c(1,3)) {
      cl = lp$groups$col_clusters[[gi]]
    }


    if (class(cl) == 'hclust') {


      scale.xy = max(g.df$x, g.df$y) - min(g.df$x, g.df$y) + 1
      pos.xy = min(g.df$x, g.df$y)
      width = abs(xy1-xy0)

      d = as.ndendrogram(cl)

      if (side == 1) {
        plot.ndendrogram(d, add=T,
                         pos.y = xy0, pos.x=pos.xy-0.5,
                         scale.x=scale.xy,
                         scale.y=width,
                         flip.y=T,
                         horiz=F, ...)

      } else if (side == 3) {
        plot.ndendrogram(d, add=T,
                         pos.y = xy0, pos.x=pos.xy-0.5,
                         scale.x=scale.xy,
                         scale.y=width,
                         flip.y=F,
                         horiz=F, ...)

      } else if (side == 2) {
        plot.ndendrogram(d, add=T,
                         pos.x = xy1, pos.y=pos.xy-0.5,
                         scale.y=scale.xy,
                         scale.x=width, horiz=T, flip.x=T, ...)

      } else if (side == 4) {
        plot.ndendrogram(d, add=T,
                         pos.x = xy1, pos.y=pos.xy-0.5,
                         scale.y=scale.xy,
                         scale.x=width, horiz=T, flip.y=F, ...)
      }
    }
  }

  lp$boundaries <- boundaries
  return(lp)

}



#' Plot group pie layer
#'
#' @description Function for plotting a group as a pie chart based on column or row attributes.
#'
#' @param lp - layermap object .
#' @param side - value for which side of the plot to apply the layer (1-bottom, 2-left, 3-top, 4-right).
#' @param attribute - name for the attribute which will be plotted in the layer. For group, this must be defined in column_groups or row_groups.
#' @param col - named color vector, where the names are conditions found in the attribute.
#' @param palette - hcl.colors palette to fill in unnamed conditions colors.
#' @param cex_label - cex value for label characters.
#' @param labels - logical for whether group labels should be plotted.
#'
#' @return layermap object
#' @export
#'
#' @examples
lp_group_pie <- function(lp, side, attribute, col= NULL, palette="Zissou 1", size=1, gap=0.4, cex=1, show_bounding_box=F, label_just='right', group_label=T, cex.label=0.8,
                         trace=T, trace.lty=1, trace.lwd=0.8, trace.gap=1.5, trace.col='black') {

  text.gap = 0


  list2env(lp_boundaries(lp, side, size, gap, text.gap, show_bounding_box = show_bounding_box), environment())


  if (side %in% c(1,3)) {
    pie.y = mean(c(xy0,xy1))
    gr = lp$groups$cols
    df = column.df
    lp_4 = lp_boundaries(lp, side=4, size=size, gap=gap)
    radius = abs((lp_4$xy1 - lp_4$xy0)/2) * cex
    x_vec = gr$x
    y_vec = c(xy1, xy0)

  } else if (side %in% c(2,4)) {
    pie.x = mean(c(xy0,xy1))
    gr = lp$groups$rows
    df = row.df
    radius = abs((xy1-xy0) /2) * cex
    y_vec = gr$y
    x_vec = c(xy1, xy0)
  }



  conditions = unique(df[[attribute]])
  if (is.logical(conditions)) conditions <- as.character(conditions)



  col = lp_colorize(col, conditions, palette)

  # if (trace) {
  #   if (side %in% c(1,3)) {
  #     segments(min(x_vec), pie.y, max(x_vec)+1, pie.y, lty=3)
  #   } else if (side %in% c(2,4)) {
  #     segments(pie.x, min(y_vec), pie.x, max(y_vec)+1, lty=3)
  #   }
  #
  # }

  if (side %in% c(1,3)) {
    trace.gap = radius * trace.gap
  } else if (side %in% c(2,4)) {
    lp_3 = lp_boundaries(lp, side=3, size=size, gap=gap)
    trace.gap = abs((lp_3$xy1 - lp_3$xy0)/2) * cex * trace.gap
  }
  last = -1 * trace.gap

  for (group in unique(gr$group_order)) {
    keys = row.names(gr[gr$group_order == group,])
    tab = table(df[keys,attribute])
    # print(tab)

    if (side %in% c(1,3)) {
      pie.x = median(gr[keys,'x'])+0.5
      if (trace & abs(pie.x-last)/2 > trace.gap) {
        segments(last+trace.gap, pie.y, pie.x-trace.gap, pie.y, lty=trace.lty, lwd=trace.lwd, col=trace.col)
      }
      last = pie.x

    } else if (side %in% c(2,4)) {
      pie.y = median(gr[keys,'y'])+0.5
      if (trace & abs(pie.y-last)/2 > trace.gap) {
        segments(pie.x, last+trace.gap, pie.x, pie.y-trace.gap, lty=trace.lty, lwd=trace.lwd, col=trace.col)
      }
      last = pie.y

    }

    plotrix::floating.pie(pie.x, pie.y, tab, radius=radius, col=col[names(tab)])


  }

  if (trace) {
    if (side %in% c(1,3)) {
      if (abs((max(x_vec)+1)-last)/2 > trace.gap) {
        segments(last+trace.gap, pie.y, max(x_vec)+1, pie.y, lty=trace.lty, lwd=trace.lwd, col=trace.col)
      }

    } else if (side %in% c(2,4)) {
      if (abs((max(y_vec)+1)-last)/2 > trace.gap) {
        segments(pie.x, last+trace.gap, pie.x, max(y_vec)+1, lty=trace.lty, lwd=trace.lwd, col=trace.col)
      }

    }
  }

  if (group_label) {
    lp_label(lp, x_vec, y_vec, side=side, text=attribute, just=label_just, cex=cex.label)
  }
  lp$legend[[attribute]] = col
  lp$boundaries <- boundaries
  return(lp)
}



# col= 'black'; font=1; adj=NULL; family=''; srt=NULL; size=1; gap=0; cex=0.8; show_bounding_box=F; group_label=F





#' Plot group name layer
#'
#' @description Function for plotting a name label for a group.
#'
#' @param lp - layermap object .
#' @param side - value for which side of the plot to apply the layer (1-bottom, 2-left, 3-top, 4-right).
#' @param col - named color vector, where the names are conditions found in the attribute.
#' @param cex - cex value for label characters.
#' @param font
#' @param adj
#' @param srt
#'
#'
#' @return layermap object
#' @export
#'
#' @examples
lp_group_names <- function(lp, side, attribute, col= 'black', font=1, adj=NULL, family='', srt=NULL, size=1, gap=0, cex=0.8, show_bounding_box=F, group_label=F) {

  text.gap = 0
  list2env(lp_boundaries(lp, side, size, gap, text.gap, show_bounding_box = show_bounding_box), environment())

  if (side %in% c(1,3)) {
    text.y = mean(c(xy0,xy1))
    gr = lp$groups$cols
    x_vec = gr$x
    y_vec = c(xy1, xy0)
    gr = aggregate(gr$x ~ gr$group_order + gr[[attribute]], gr, median)
    names(gr) <- c("group_order", attribute,'x')
    text.x = gr$x + 0.5

  } else if (side %in% c(2,4)) {
    text.x = mean(c(xy0,xy1))
    gr = lp$groups$rows
    y_vec = gr$y
    x_vec = c(xy1, xy0)
    gr = aggregate(gr$y ~ gr$group_order + gr[[attribute]], gr, median)
    names(gr) <- c("group_order", attribute,'y')
    text.y = gr$y + 0.5
  }

  print(srt)
  if (is.null(srt)) {
    if (side %in% c(1,3)) {
      srt = 90
    } else {
      srt = 0
    }
  }


  if (is.null(adj)) {
    if (side %in% c(1,2)) {
      adj=c(1,0.5)
    } else if (side %in% c(3,4)) {
      adj=c(0,0.5)
    }
  }

  # print(text.x)
  # print(text.y)
  # print(gr[[attribute]])

  text(text.x, text.y, gr[[attribute]], srt=srt, adj=adj, font=font, col=col, cex=cex, family=family)


  if (group_label) {
    lp_label(lp, x_vec, y_vec, side=side, text=attribute, just=label_just, cex=cex.label)
  }

  lp$boundaries <- boundaries
  return(lp)
}



#' Plot values
#'
#' @description Function for plotting values over main heatmap
#'
#' @param lp - layermap object .
#'
#'
#' @return layermap object
#' @export
#'
#' @examples
lp_plot_values <- function(lp, l_threshold = 50, round.n=2, cex=0.6) {

  p.df <- lp$plotting.df
  colors = p.df$color
  colors[is.na(colors)] <- '#FFFFFF'
  p.df$l <- schemr::hex_to_lab(colors)[,1]
  p.df$text_col <- 'black'
  p.df$text_col <- ifelse(p.df$l < l_threshold, 'white','black')

  row.df <- lp$row.df
  row.df$y <- p.df$y[match(row.names(row.df), p.df$rows)]
  text(p.df$x+0.5, p.df$y+0.5, round(p.df$value, round.n),
       cex=cex,
       col=p.df$text_col)

}

