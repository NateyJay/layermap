


# size=1; gap=0.4; palette="Zissou 1"; col= NULL; show_bounding_box=F; plot_names=T; names.cex=0.8; plot_label=T; label.cex=0.8; label.just='right'

#' Plot group layer
#'
#' @description Function for plotting a group layer based on column or row attributes.
#'
#' @param lp layermap object.
#' @param side value for which side of the plot to apply the layer (1-bottom, 2-left, 3-top, 4-right).
#' @param attribute name for the attribute which will be plotted in the layer. For group, this must be defined in column_groups or row_groups.
#' @param size the space (in lines) for this layer in the margin.
#' @param gap the space (in lines) between this layer and the prior layer.
#' @param palette hcl.colors palette to fill in unnamed conditions colors.
#' @param col a named vector, where colors are the values and names are conditions that are defined in the attribute/group that is plotted.
#' @param show_bounding_box shows the bounding space for this layer (useful for troubleshooting)
#'
#' @param plot_names logical for whether names should be plotted for groups.
#' @param names.cex cex value relating to name text size.
#'
#' @param plot_label logical for whether layer labels should be plotted.
#' @param label.cex cex value relating to layer label text size.
#' @param label.just the justification side for a layer label (right or left).
#'
#' @return layermap object
#' @export
#'
#' @examples
lp_group <- function(lp,
                     side,
                     attribute,
                     size=1,
                     gap=0.4,
                     palette="Zissou 1",
                     col= NULL,
                     show_bounding_box=F,
                     plot_names=T,
                     names.cex=0.8,
                     plot_label=T,
                     label.cex=0.8,
                     label.just='right') {

  if (plot_names) {
    str_multiplier = 2

    text.gap = strheight("G", cex=names.cex) * str_multiplier * names.cex

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



  conditions = unique(as.character(gr[[attribute]]))


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
      if (plot_names) { text(mean(c(min(g.df$x), max(g.df$x)+1)), text.y, cond, cex=label.cex) }

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
      cond = as.character(g.df[[attribute]][1])
      # segments(half.x, min(g.df$y), half.x, max(g.df$y)+1, col='black', lwd=3, lend=1)
      # segments(half.x, min(g.df$y), half.x, max(g.df$y)+1, col=col[cond], lwd=2.5, lend=1)
      rect(box.x1, min(g.df$y, na.rm=T), box.x2, max(g.df$y, na.rm=T)+1, col=col[cond])
      if (plot_names) { text(text.x, mean(c(min(g.df$y), max(g.df$y)+1)), srt=90, cond, cex=names.cex) }

      # for (gi in clump){
      #   g.df <- gr[gr$group_order == gi,]
      #   rect(box.x1, min(g.df$y), box.x2, max(g.df$y)+1, col=col[cond])
      #   # abline(v=c(box.x1, box.x2))
      # }
    }
  }

  if (plot_label) {
    lp_label(lp, x_vec, y_vec, side=side, text=attribute, just=label.just, cex=label.cex)
  }

  lp$legend[[attribute]] = col
  lp$boundaries <- boundaries
  return(lp)
}



# a.df=NULL; col=NULL; size=1; gap=0.4;
# palette='Viridis';
# show_bounding_box=F; type='rect';
# label.just='right'; label.cex=0.8; border=NA;
# point.cex=1; pch=19; lwd=1;
# show_label=T
# zlim=NULL; reverse_palette = F; zero_centered_colors = F


# a.df=NULL; col=NULL; size=1; gap=0.4; palette='Viridis'
# show_bounding_box=F; type='rect'; label.just='right'; label.cex=0.8

#' Plot annotation layer
#'
#' @description Function for plotting an annotation layer based on column or row attributes.
#'
#' @param lp layermap object.
#' @param side value for which side of the plot to apply the layer (1-bottom, 2-left, 3-top, 4-right).
#' @param attribute name for the attribute which will be plotted in the layer. For group, this must be defined in column_groups or row_groups.
#' @param size the space (in lines) for this layer in the margin.
#' @param gap the space (in lines) between this layer and the prior layer.
#' @param palette hcl.colors palette to fill in unnamed conditions colors.
#' @param reverse_palette logical setting the order for the palette.
#' @param col a named vector, where colors are the values and names are conditions that are defined in the attribute/group that is plotted.
#'
#' @param zlim vector for the c(min, max) values for the z-scale (colors). Values over or under will be truncated to the marginal colors.
#' @param zero_centered_colors an option to force the center of the palette/colors to coincide with zero. Useful for data fold-change data or other data where zero is neutral.
#'
#'
#' @param show_bounding_box shows the bounding space for this layer (useful for troubleshooting)
#'
#' @param type option for the plotting form: c('rect','point'). rect shows a bordered box and point is a pch-defined shape.
#' @param border color for rect border. Default is to not plot the border ("NA").
#' @param lwd border line width.
#' @param group.border color for border around whole group of attribute boxes. Default is to not plot the border ("NA").
#' @param group.lwd group border line width.
#' @param pt.cex relating to the size of the point plotted.
#' @param pch character type for point.
#'
#' @param plot_label logical for whether layer labels should be plotted.
#' @param label text to be used as the label name.
#' @param label.cex cex value relating to layer label text size.
#' @param label.just the justification side for a layer label (right or left).
#'
#' @return layermap object
#' @export
#'
#' @examples
lp_annotate <- function(lp, side, attribute,
                        size=1,
                        gap=0.4,
                        palette='Viridis',
                        reverse_palette=F,
                        col=NULL,

                        zlim=NULL,
                        zero_centered_colors=F,

                        show_bounding_box=F,

                        type='rect',

                        label.cex=0.8,
                        border=NA,
                        group.border=NA,
                        group.lwd = 1,

                        lwd=1,
                        pt.cex=1,
                        pch=19,

                        plot_label=T,
                        label=attribute,
                        label.just='right') {

  list2env(lp_boundaries(lp, side, size, gap, show_bounding_box = show_bounding_box), environment())

  message(attribute)

  type = match.arg(type, c('rect','points'))


  if (side %in% c(2,4)) {
    gr = lp$groups$rows

    gr$y0=gr$y
    gr$y1=gr$y+1
    gr$x0=xy1
    gr$x1=xy0
    gr$ymean=gr$y+0.5
    gr$xmean=mean(c(xy1,xy0))
    a.df = lp$row.df

  } else if (side %in% c(1,3)) {
    gr = lp$groups$cols

    gr$x0=gr$x
    gr$x1=gr$x+1
    gr$y0=xy1
    gr$y1=xy0
    gr$xmean=gr$x+0.5
    gr$ymean=mean(c(xy1,xy0))
    a.df = lp$column.df
  }



  conditions = unique(a.df[[attribute]])



  if (is.numeric(conditions)) {
    if (is.null(zlim)) {
      zlim=c(min(a.df[[attribute]], na.rm = T), max(a.df[[attribute]], na.rm = T))
    }
    ls = vector_to_colors(a.df[[attribute]], zlim=zlim, palette=palette,
                         reverse_palette = reverse_palette,
                         zero_centered_colors = zero_centered_colors)
    col         = ls$color
    color_scale = ls$color_scale

    gr$col <- col[match(rownames(gr), rownames(a.df))]


  } else {

    col = lp_colorize(col, conditions, palette)

    gr$attribute_value <- a.df[match(rownames(gr), rownames(a.df)),attribute]
    gr$col <- col[gr$attribute_value]

  }




  if (type == 'points' & pch %in% c(21:25)) {
    gr$bg = gr$col
    gr$col <- NA
    gr$col[!is.na(gr$bg)] <- 'black'

  }


  if (type == 'rect') {
    rect(gr$x0, gr$y0, gr$x1, gr$y1, col=gr$col, border=border,
         lwd=lwd)
  } else if (type == 'points') {
    points(gr$xmean, gr$ymean,
           bg=gr$bg, col=gr$col, pch=pch, cex=pt.cex)
  }

  if (!is.na(group.border)) {
    for (group in unique(gr$group_order)) {
      g = gr[gr$group_order == group,]
      rect(min(g$x0), min(g$y0), max(g$x1), max(g$y1), border=group.border, col=NA, lwd=group.lwd)
    }
  }


  if (plot_label) {lp_label(lp, gr$xmean, gr$ymean, side=side, text=label, just=label.just, cex=label.cex)
  }
  lp$boundaries <- boundaries

  if (is.numeric(conditions)) {

    val = a.df[[attribute]]
    end_point = c(
      ifelse(zlim[1] > min(val, na.rm=T), "≤", ""),
      ifelse(zlim[2] > max(val, na.rm=T), "≥", "")
    )

    lp$color_legend[[attribute]] = list(colors=rev(color_scale), zlim=zlim, end_point=end_point)
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
#' @param side value for which side of the plot to apply the layer (1-bottom, 2-left, 3-top, 4-right).
#' @param attributes name or names of numeric attributes to form a color legend. Multiples will be plotted in the same layer beside each other with a gap.
#' @param size the space (in lines) for this layer in the margin.
#' @param gap the space (in lines) between this layer and the prior layer.
#'
#' @param size_p the proportion of the total axis which the legend should occupy (1 equates to the whole side)
#' @param gap_p the proportion of the total axis which would be spaced between multiple legends.
#'
#' @param round the number of decimals included in the legend labels.
#' @param cex legend label size.
#' @param title.cex legend title size.
#' @param main legend title.
#'
#'
#' @return
#' @export
#'
#' @examples

lp_color_legend <- function(lp,
                            side,
                            attributes=c('main'),
                            size=0.5,
                            gap=1.5,
                            size_p = 0.4,
                            gap_p=0.05,
                            round=1,
                            cex=0.6,
                            title.cex=NULL,
                            titles=attributes) {

  if (is.null(title.cex)) {
    title.cex = cex
  }

  # lines = length(leg) *2

  list2env(lp_boundaries(lp, side, size, gap, show_bounding_box = F), environment())


  # leg <- lp$color_legend[[attribute]]


  if (side %in% c(1,3)) {

    y0 = xy0
    y1 = xy1

    w = lp$xmax * size_p
    g = lp$xmax * gap_p

    leg.df <- data.frame(name=attributes, y0=y0, y1=y1, w = w)

    gs = cumsum(c(0, rep(g, nrow(leg.df)-1)))

    # leg.df$x.text = cumsum(leg.df$w) - leg.df$w
    leg.df$x0 = cumsum(leg.df$w) - leg.df$w + gs
    leg.df$x1 = leg.df$x0 + leg.df$w
    leg.df$x.text <- apply(leg.df[,c('x0','x1')], 1, mean)


  } else if (side %in% c(2,4)) {

    x0 = xy0
    x1 = xy1

    w = lp$ymax * size_p
    g = lp$ymax * gap_p

    gs = cumsum(c(0, rep(g, nrow(leg.df)-1)))

    leg.df <- data.frame(name=attributes, x0=x0, x1=x1, w = w)
    leg.df$y.text = lp$ymax - cumsum(leg.df$w) + w - gs
    leg.df$y0 = leg.df$y.text - gs
    leg.df$y1 = leg.df$y0 - leg.df$w + gs + gs


  }

#   leg.df$name[1] <- main
#   names(leg)[1] <- main

  for (leg_i in 1:nrow(leg.df)) {
    n <- leg.df$name[leg_i]

    leg = lp$color_legend[[n]]

    df = leg.df[leg.df$name == n,]

    # col = hcl.colors(col_n, leg[[leg_i]]$palette, rev=!leg[[leg_i]]$reverse_palette)
    col  = rev(leg$colors)
    zlim = leg$zlim
    ep   = leg$end_point
    c    = 1:length(col)


    if (side %in% c(2,4)) {
      text(df$x1, df$y.text, titles[leg_i], adj=c(0,1), cex=title.cex, font=3)
      rect(df$x0, df$y0 - (df$y0 - df$y1)*(c-1)/length(c), df$x1, df$y1,
           col=rev(col), border=NA)
      rect(df$x0, df$y0, df$x1, df$y1, lwd=1)

      text(df$x0, c(df$y0,df$y1),
           stringr::str_c(ep, round(zlim, round)),
           pos=side, cex=cex, offset=0.25)

    } else if (side %in% c(1,3)) {
      text(df$x.text, df$y1, titles[leg_i], adj=c(0.5,-0.5), cex=title.cex, font=3)
      rect(df$x0 - (df$x0 - df$x1)*(c-1)/length(c), df$y0, df$x1, df$y1,
           col=rev(col), border=NA)
      rect(df$x0, df$y0, df$x1, df$y1, lwd=1)

      text(c(df$x0,df$x1), df$y0, stringr::str_c(ep, round(zlim, round)), pos=side, cex=cex, offset=0.25)

    }

  }


  lp$boundaries <- boundaries
  return(lp)
}



#
# lp <- layermap(data.frame(A=c('a','a','j','k'), B=c('k','k','j','z')))
# lp <- lp_legend(lp, 3)
# lp <- lp_legend(lp, 4)


# side=1; attributes=NULL; cex=0.6; gap=0.4;
# title.col='black'; title.cex=NULL; title.font=3

# attributes=NULL; cex=0.6; gap=0.4
# title.col='black'; title.cex=NULL; title.font=3

#' Plot simple categorical legend
#'
#' @description Makes a simple color legend for a layermap object.
#'
#' @param lp layermap object
#' @param side value for which side of the plot to apply the layer (1-bottom, 2-left, 3-top, 4-right).
#' @param attribute name for the attribute which is used for the legend. Defaults to "main", the main colors if categorical
#' @param size the space (in lines) for this layer in the margin.
#'
#' @param cex total legend size.
#'
#' @param title.col title color.
#' @param title.cex title size.
#' @param title.font title font.
#'
#' @param ncol number of columns for the legend. Defaults to single columns for the sides and 3 for the top/bottom.
#'
#' @return
#' @export
#'
#' @examples
lp_legend <- function(lp,
                      side,
                      attribute,
                      cex=0.8,
                      gap=0.2,
                      title=attribute,
                      title.col='black',
                      title.cex=NULL,
                      title.font=3,
                      ncol='auto') {

  if (is.null(title.cex)) {
    title.cex = cex
  }

  leg <- lp$legend[[attribute]]
  lines = length(unlist(leg)) + length(leg) + 3


  list2env(lp_boundaries(lp, side, size=0, gap, text.gap=0, show_bounding_box = F), environment())

  # par(mar=rep(0.3,4))


  if (ncol == 'auto') {
    if (side %in% c(1,3)) {
      if (length(leg) >= 3) {
        ncol = 3
      } else {
        ncol = length(leg)
      }
    } else {
      ncol=1
    }

  }


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


  l = legend(x,y, names(leg), fill=leg, cex=cex, xjust=xjust, yjust=yjust,
             title=title,
             title.col=title.col, title.cex=title.cex, title.font=title.font,
             ncol=ncol)


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
#' @param lp layermap object.
#' @param side value for which side of the plot to apply the layer (1-bottom, 2-left, 3-top, 4-right).
#' @param attribute name for the attribute which will be plotted in the layer. For group, this must be defined in column_groups or row_groups.
#' @param size the space (in lines) for this layer in the margin.
#' @param gap the space (in lines) between this layer and the prior layer.
#'
#' @param show_bounding_box shows the bounding space for this layer (useful for troubleshooting)
#' @param autobox logical for to try to automatically determine the "size" value. This is sometimes challenging as text can be very long.
#'
#' @param cex relating to text size for names.
#' @param just text justification, automatically justifies in relation to the heatmap.
#' @param col text color.
#' @param font text font.
#'
#'
#' @param names an optional vector to plot only some attributes, based on row or column (names).
#'
#'
#'
#' @return layermap object
#' @export
#'
#' @examples
lp_names <- function(lp,
                     side,
                     attribute=F,
                     names=NULL,
                     size=1,
                     gap=0.4,
                     show_bounding_box = F,
                     autobox=T,
                     cex=0.8,
                     just='auto',
                     col='black',
                     font=1) {


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

  just = match.arg(just, c('auto','left','right'))
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
#' @param lp layermap object.
#' @param side value for which side of the plot to apply the layer (1-bottom, 2-left, 3-top, 4-right).
#' @param size the space (in lines) for this layer in the margin.
#' @param gap the space (in lines) between this layer and the prior layer.
#'
#' @param col line color.
#' @param lwd line width.
#'
#' @param show_bounding_box shows the bounding space for this layer (useful for troubleshooting).
#'
#'
#'
#'
#'
#' @return layermap object
#' @export
#'
#' @examples
lp_dend <- function(lp,
                    side,
                    size=2,
                    gap=0.2,
                    show_bounding_box = F,
                    ...) {


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
#' @param lp layermap object.
#' @param side value for which side of the plot to apply the layer (1-bottom, 2-left, 3-top, 4-right).
#' @param attribute name for the attribute which will be plotted in the layer. For group, this must be defined in column_groups or row_groups.
#'
#' @param size the space (in lines) for this layer in the margin.
#' @param gap the space (in lines) between this layer and the prior layer.
#' @param palette hcl.colors palette to fill in unnamed conditions colors.
#' @param col a named vector, where colors are the values and names are conditions that are defined in the attribute/group that is plotted.
#'
#' @param show_bounding_box shows the bounding space for this layer (useful for troubleshooting)
#'
#' @param cex size value for pie.
#' @param plot_label logical for whether labels should be plotted.
#' @param label.just justification side for layer label.
#' @param label.cex label size.
#'
#' @param trace outline of pie.
#' @param trace.lty trace line type.
#' @param trace.lwd
#' @param trace.gap
#' @param trace.col
#'
#'
#'
#'
#' @param cex_label - cex value for label characters.
#' @param labels - logical for whether group labels should be plotted.
#'
#' @return layermap object
#' @export
#'
#' @examples
lp_group_pie <- function(lp,
                         side,
                         attribute,
                         size=1,
                         gap=0.4,
                         palette="Zissou 1",
                         col= NULL,

                         show_bounding_box=F,

                         cex=1,
                         label.just='right',
                         plot_label=T,
                         label.cex=0.8,

                         trace=T,
                         trace.lty=1,
                         trace.lwd=0.8,
                         trace.gap=1.5,
                         trace.col='black') {

  text.gap = 0


  list2env(lp_boundaries(lp, side, size, gap, text.gap, show_bounding_box = show_bounding_box), environment())


  if (side %in% c(1,3)) {
    pie.y = mean(c(xy0,xy1))
    gr = lp$groups$cols
    df = lp$column.df
    lp_4 = lp_boundaries(lp, side=4, size=size, gap=gap)
    radius = abs((lp_4$xy1 - lp_4$xy0)/2) * cex
    x_vec = gr$x
    y_vec = c(xy1, xy0)

  } else if (side %in% c(2,4)) {
    pie.x = mean(c(xy0,xy1))
    gr = lp$groups$rows
    df = lp$row.df
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

  if (plot_label) {
    lp_label(lp, x_vec, y_vec, side=side, text=attribute, just=label.just, cex=label.cex)
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
#' @param lp layermap object.
#' @param side value for which side of the plot to apply the layer (1-bottom, 2-left, 3-top, 4-right).
#' @param attribute name for the attribute which will be plotted in the layer. For group, this must be defined in column_groups or row_groups.
#'
#' @param size the space (in lines) for this layer in the margin.
#' @param gap the space (in lines) between this layer and the prior layer.
#' @param show_bounding_box shows the bounding space for this layer (useful for troubleshooting)
#'
#' @param cex size value for names.
#' @param col text color.
#' @param font text font.
#' @param adj text justification.
#' @param srt text angle. Defaults as perpendicular to axis.
#' @param family text family.
#'
#' @return layermap object
#' @export
#'
#' @examples
lp_group_names <- function(lp,
                           side,
                           attribute,
                           size=1,
                           gap=0,

                           col= 'black',
                           font=1,
                           adj=NULL,
                           family='',
                           srt=NULL,
                           cex=0.8,
                           show_bounding_box=F,
                           plot_label=F) {

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

    if (srt == 0) adj=switch(side,
                             `1`= c(0.5,1),
                             `2`= c(1, 0.5),
                             `3`= c(0.5, 0),
                             `4`= c(0, 0.5))

    if (srt == 90) adj=switch(side,
                             `1`= c(1, 0.5),
                             `2`= c(1, 0.5),
                             `3`= c(0, 0.5),
                             `4`= c(0, 0.5))

  }

  # print(text.x)
  # print(text.y)
  # print(gr[[attribute]])

  text(text.x, text.y, gr[[attribute]], srt=srt, adj=adj, font=font, col=col, cex=cex, family=family)


  if (plot_label) {
    lp_label(lp, x_vec, y_vec, side=side, text=attribute, just=label.just, cex=label.cex)
  }

  lp$boundaries <- boundaries
  return(lp)
}




# lp
# alt.df=NULL
# round.n=2
# cex=0.6
# light_color='white'
# dark_color='black'
# l_threshold = 50

#' Plot values
#'
#' @description Function for plotting values over main heatmap
#'
#' @param lp layermap object.
#' @param alt.df an alternative data.frame which will provide the values. Useful if the figure is colored with a different value than you want to show inside (for example scaled data). Should be in the same format as the value.df. NAs are not plotted
#'
#' @param cex text size.
#' @param round.n number of decimals included in value.
#'
#' @param l_threshold lightness value in lab of background color to choose if values are plotted as the light color or dark color.
#' @param light_color text color for dark backgrounds.
#' @param dark_color text color for light backgrounds.
#'
#' @return layermap object
#' @export
#'
#' @examples
lp_plot_values <- function(lp, alt.df=NULL,
                           round.n=2,
                           cex=0.6,
                           light_color='white',
                           dark_color='black',
                           l_threshold = 50) {


  p.df <- lp$plotting.df
  colors = p.df$color
  colors[is.na(colors)] <- '#FFFFFF'
  named_colors = colors %in% colors()

  name2hex <- function(c) {
    c = col2rgb(c)
    c = c[,1]
    c = rgb(c[1], c[2], c[3], maxColorValue = 255)
    return(c)
  }

  colors[named_colors] <- sapply(colors[named_colors], name2hex)


  rgb2col = function(rgbmat){
    # function to apply to each column of input rgbmat
    ProcessColumn = function(col){
      rgb(rgbmat[1, col],
          rgbmat[2, col],
          rgbmat[3, col],
          maxColorValue = 255)
    }
    # Apply the function
    sapply(1:ncol(rgbmat), ProcessColumn)
  }



  p.df$l <- schemr::hex_to_lab(colors)[,1]
  p.df$text_col <- dark_color
  p.df$text_col <- ifelse(p.df$l < l_threshold, light_color, dark_color)

  if (!is.null(alt.df)) {
    a.df <- alt.df
    a.df$rownames <- rownames(a.df)
    a.df <- reshape2::melt(a.df, id.vars='rownames')
    names(a.df) <- c('rownames','colnames','values')
    p.df$value <- a.df$values[match(stringr::str_c(p.df$rows,p.df$cols), stringr::str_c(a.df$rownames, a.df$colnames))]
  }

  message(class(lp$plotting.df$value))
  if (class(lp$plotting.df$value) == 'numeric') p.df$value = round(p.df$value, round.n)

  text(p.df$x+0.5, p.df$y+0.5, p.df$value,
       cex=cex,
       col=p.df$text_col)

}

