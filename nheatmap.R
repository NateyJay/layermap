
# To-do

# [ ] Allow to accept manual color vectors. Perhaps this will be a named list of groups/annotations representing named vectors for colors
# [ ] Accept annotation as a top-level attribute. This can be entered at the main-plot or an annotation method.
# [ ] Colorize and label legend better. Allow for placement in main plotting space. Maybe sides 1.5, 2.5, 3.5, 4.5/0.5?



as.ndendrogram <- function(d) {
  require(dendextend)
  d = as.dendrogram(d)
  
  
  
  ## getting node xy coords
  xy.df <- as.data.frame(get_nodes_xy(d))
  colnames(xy.df)=c("x","y")
  
  
  ## getting node attributes
  xy.df$members <- get_nodes_attr(d, 'members')
  # xy.df$nodePar <- get_nodes_attr(d, 'nodePar')
  xy.df$height  <- get_nodes_attr(d, 'height')
  xy.df$leaf    <- get_nodes_attr(d, 'leaf')
  xy.df$leaf[is.na(xy.df$leaf)] <- F
  xy.df$label   <- get_nodes_attr(d, 'label')
  
  xy.df$claimed = F; xy.df$to1 <- NA; xy.df$to2 <- NA
  xy.df$claimed[1] <- T
  
  i=1
  # loop_count = 0
  
  while (T){
    # loop_count = loop_count + 1
    
    if (!xy.df$leaf[i] & is.na(xy.df$to2[i])) {
      ## this is a non leaf node that is incomplete
      if (is.na(xy.df$to1[i])) {
        to_name = 'to1'
      } else {
        to_name = 'to2'
      }
      
      
      j=1
      while (T) {
        
        if (!xy.df$claimed[i+j]) {
          
          xy.df[i,to_name] <- i+j
          xy.df$claimed[i+j] <- T
          
          if (!xy.df$leaf[i+j]) { 
            i = i+j
          }
          
          break
        }
        j = j + 1
      }
      
      
    } else {
      i = i - 1
    }
    
    # if (xy.df$claimed[i+1])
    
    # print(xy.df)
    # invisible(readline(prompt="Press [enter] to continue"))
    
    if (!any(!xy.df$claimed)) {
      break
    }
    
    # message(str_glue("{loop_count}\ti={i}\tj={j}"))
    
  }
  return(list(nodes=xy.df, nnodes=nrow(xy.df), nleaves=sum(xy.df$leaf)) )
}


plot.ndendrogram <- function(d, horiz=T, flip.x=F, flip.y=F, type='square', add=F, 
                             scale.x=NULL, scale.y=NULL,
                             pos.x=0, pos.y=0) {
  
  if (horiz) {
    d$nodes$xx <- d$nodes$x
    d$nodes$x  <- d$nodes$y
    d$nodes$y  <- d$nodes$xx
    d$nodes$xx <- NULL
  }
  
  xy.df <- d$nodes
  
  if (add) {
    
    if (is.null(scale.x)) {
      scale.x = max(xy.df$x)
    }
    
    if (is.null(scale.y)) {
      scale.y = max(xy.df$y)
    }
    
    if (flip.x) {
      xy.df$x = max(xy.df$x) - xy.df$x
    }
    
    if (flip.y) {
      xy.df$y = max(xy.df$y) - xy.df$y
    }
    
    xy.df$x = xy.df$x / max(xy.df$x) * scale.x + pos.x
    xy.df$y = xy.df$y / max(xy.df$y) * scale.y + pos.y
    
  } else {
    
    plot(xy.df$x, xy.df$y, type='n')
  }
  
  
  
  
  if (type == 'triangle') {
    
    f = !xy.df$leaf
    segments(xy.df$x[f], xy.df$y[f], xy.df$x[xy.df$to1[f]], xy.df$y[xy.df$to1[f]])
    segments(xy.df$x[f], xy.df$y[f], xy.df$x[xy.df$to2[f]], xy.df$y[xy.df$to2[f]])
    
  } else if (type == 'square') {
    
    f = !xy.df$leaf
    
    for (to_name in c("to1", "to2")) {
      
      x1 = xy.df$x[f]
      x2 = xy.df$x[xy.df[f,to_name]]
      y1 = xy.df$y[f]
      y2 = xy.df$y[xy.df[f,to_name]]
      
      if (horiz) {
        segments(x1, y2, x2, y2)
        segments(x1, y1, x1, y2)
      } else {
        segments(x1, y1, x2, y1)
        segments(x2, y1, x2, y2)
      }    
    }
    
    
  }
  
}



nheatmap <- function(input_df, xlim=NULL, ylim=NULL, color_scale=NULL, 
                     cluster_cols=F, cluster_rows=T,
                     group_cols=NULL, group_rows=NULL,
                     ann_cols=NULL, ann_rows=NULL,
                     group_gap=0.02, border='grey25',
                     plot_margin=c(0.2,0.2,0.2,0.2),
                     dim_reference="din") {
  
  par(mar=c(0.3,0.3,0.3,0.3), xpd=T)
  
  if (class(input_df) == 'table') {
    header = colnames(input_df)
    mat = as.data.frame(matrix(input_df, nrow(input_df)), row.names=row.names(input_df))
    colnames(mat) = header
    input_df <- mat
  }
  
  df <- as.data.frame(input_df)
  
  groups <- list()
  if (!is.null(group_rows)) {
    groups$rows <- group_rows
  } else {  
    groups$rows <- data.frame(row.names=rownames(df), group_order = rep(1, nrow(df)))
  }
  
  if (!is.null(group_cols)) {
    groups$cols <- group_cols
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
  
  
  
  order_by_cluster <- function(fun.df, margin, groups) {
    
    clusters=list()
    
    if (margin == 1) {
      gr = groups$rows
      mat = as.matrix(fun.df)
    } else {
      gr = groups$cols
      mat = t(fun.df) 
    }
    
    for (g in unique(gr$group_order)) {
      f = gr$group_order %in% g
      
      if (sum(f) == 1) {
        clusters[[g]] = F
        gr[label_order,'cluster_order'] <- 1
        
      } else {
        d = mat[f,, drop=F]
        d = dist(d)
        d = hclust(d)
        clusters[[g]] = d
        label_order = d$labels[d$order]
        gr[label_order,'cluster_order'] <- 1:sum(f)
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
  
  
  groups <- order_by_cluster(df, 1, groups)
  groups <- order_by_cluster(df, 2, groups)
  
  ## reordering if clustering is selected
  if (cluster_rows) groups$rows <- groups$rows[order(groups$rows$group_order, groups$rows$cluster_order),]
  if (cluster_cols) groups$cols <- groups$cols[order(groups$cols$group_order, groups$cols$cluster_order),]
  
  
  ## getting plotting dimensions
  
  get_max <- function(base, gi, g) {
    # base = nrow(df)
    # gi = max(groups$rows$group_order)
    # g = group_gap
    
    ymax = base / (1-(gi * g))
    # ymax * (1-(gi*g))
    return(ymax)
  }
  
  
  din_ratio = par()$din[2] / par()$din[1]
  
  ymax <- get_max(nrow(df), 
                  max(groups$rows$group_order)-1,
                  group_gap)
  
  xmax <- get_max(ncol(df), 
                  max(groups$cols$group_order)-1,
                  group_gap*din_ratio)
  
  
  gap.y = ymax * group_gap
  gap.x = xmax * group_gap*din_ratio
  
  
  ## getting xy coordinates
  groups$rows$yi <- (1:nrow(groups$rows))-1
  groups$rows$y <- groups$rows$yi + (groups$rows$group_order-1)*gap.y
  
  groups$cols$xi <- (1:nrow(groups$cols))-1
  groups$cols$x <- groups$cols$xi + (groups$cols$group_order-1)*gap.x
  
  
  ## matching value data frame to group/cluster orders
  df = df[match(rownames(groups$rows), rownames(df)),]
  df = df[, match(rownames(groups$cols), colnames(df))]
  
  
  
  ## colorizing heatmap
  
  if (is.null(color_scale)) {
    color_n = 100
    color_scale = hcl.colors(color_n, palette='Blue-Red 2')
  } else {
    color_n = length(color_scale)
  }
  
  m.df <- df
  m.df$rows <- rownames(m.df)
  m.df <- reshape2::melt(m.df, id.vars='rows')
  colnames(m.df)[2] <- 'columns'
  
  
  
  m.df$color_i <- round((m.df$value - min(m.df$value, na.rm=T)) / (max(m.df$value, na.rm=T)-min(m.df$value, na.rm=T)) * (color_n-1)) +1
  m.df$color <- color_scale[m.df$color_i]
  
  m.df$y <- groups$rows$y[match(m.df$rows, rownames(groups$rows))]
  m.df$x <- groups$cols$x[match(m.df$columns, rownames(groups$cols))]
  
  
  ## getting id's for each box
  m.df$box.x = groups$cols$group_order[match(m.df$columns, rownames(groups$cols))]
  m.df$box.y = groups$rows$group_order[match(m.df$rows, rownames(groups$rows))]
  m.df$box <- str_c(m.df$box.x, m.df$box.y, sep=',')
  m.df$box.y <- NULL
  m.df$box.x <- NULL
  
  
  
  xlim = c(0 - xmax * plot_margin[2], xmax + xmax * plot_margin[4])
  ylim = c(0 - ymax * plot_margin[1], ymax + ymax * plot_margin[3])
  
  
  ## plotting data
  
  plot(1,1, type='n', xlim=xlim, ylim=ylim, xlab='', ylab='', axes=F)
  
  
  rect(m.df$x, m.df$y, m.df$x+1, m.df$y+1, col=m.df$color, border=NA)
  
  for (box in unique(m.df$box)) {
    box.df <- m.df[m.df$box == box,]
    rect(min(box.df$x), min(box.df$y), max(box.df$x)+1, max(box.df$y)+1, border='black', col=NA)
  }
  
  # points(0,ymax)
  # points(xmax,ymax)
  # points(0,0)
  # points(xmax, 0)
  
  
  last_positions <- c(0, 0, ymax, xmax)
  
  
  out = list(xlim=xlim,
             ylim=ylim,
             xmax=xmax,
             ymax=ymax,
             # xrange=xrange,
             # yrange=yrange,
             gap.x=gap.x,
             gap.y=gap.y,
             cluster_rows=cluster_rows,
             cluster_cols=cluster_cols,
             groups=groups,
             plotting.df=m.df,
             boundaries = last_positions,
             legend = list(),
             din_ratio=din_ratio)
  
}




nheatmap_boundaries <- function(nh, side, percent, show_bounding_box=F, din_adjusted=T) {
  
  if (din_adjusted) {
    din_ratio = nh$din_ratio
  } else {
    din_ratio = 1
  }
  # din_ratio = 1
  
  if (side == 2) {
    ## left
    
    xy0 = nh$boundaries[2]
    xy1 = xy0 - nh$xmax * percent * din_ratio
    xy0 = xy0 - nh$gap.x
    
    if (show_bounding_box) {rect(xy1, min(nh$plotting.df$y), xy0, max(nh$plotting.df$y)+1)}
    
    nh$boundaries[2] = xy1
    
    sign = -1
    
    
  } else if (side == 1) {
    ## bot
    
    xy0 = nh$boundaries[1]
    xy1 = xy0 - nh$ymax * percent
    xy0 = xy0 - nh$gap.y
    
    if (show_bounding_box) {rect(min(nh$plotting.df$x), xy1, max(nh$plotting.df$x)+1, xy0)}
    
    nh$boundaries[1] = xy1
    
    sign = -1
    
    
  } else if (side == 4) {
    ## right
    
    xy0 = nh$boundaries[4]
    xy1 = xy0 + nh$xmax * percent * din_ratio
    xy0 = xy0 + nh$gap.x
    
    if (show_bounding_box) {rect(xy0, min(nh$plotting.df$y), xy1, max(nh$plotting.df$y)+1)}
    
    nh$boundaries[4] = xy1
    
    sign = 1
    
    
  } else if (side == 3) {
    ## top
    
    xy0 = nh$boundaries[3]
    xy1 = xy0 + nh$ymax * percent
    xy0 = xy0 + nh$gap.y
    
    if (show_bounding_box) {rect(min(nh$plotting.df$x), xy1, max(nh$plotting.df$x)+1, xy0)}
    
    nh$boundaries[3] = xy1
    
    sign = 1
  }
  
  ls = list(xy0=xy0,
            xy1=xy1,
            boundaries=nh$boundaries,
            sign=sign)
  return(ls)
}

# gname='cat'; side= 2;palette= 'terrain'; label_just = 'left'
nheatmap_group <- function(nh, gname, side, col= NULL, palette="Zissou 1", percent=0.1, cex=0.8, show_bounding_box=F, label_just='right') {
  box.p = 0.5
  text.p = 0.25
  
  list2env(nheatmap_boundaries(nh, side, percent=percent, show_bounding_box = show_bounding_box), environment())
  
  if (side == 1) {
    box.y1 = xy0
    box.y2 = xy0 - abs(xy1-xy0) * box.p
    text.y = xy1 + abs(xy1-xy0) * text.p
    gr = nh$groups$cols
    
  } else if (side == 3) {
    box.y1 = xy1
    box.y2 = xy0 + abs(xy1-xy0) * box.p
    text.y = xy0 + abs(xy1-xy0) * text.p
    gr = nh$groups$cols
    
  } else if (side == 2) {
    box.x1 = xy0 - abs(xy1-xy0) * box.p
    box.x2 = xy1 
    text.x = xy0 - abs(xy1-xy0) * text.p
    gr = nh$groups$rows
    
  } else if (side == 4) {
    box.x1 = xy0 + abs(xy1-xy0) * box.p
    box.x2 = xy0 
    text.x = xy1 - abs(xy1-xy0) * text.p
    gr = nh$groups$rows
    
  }
    
    
  conditions = unique(gr[[gname]])
  
  if (!is.null(col)) {
    if (!any(names(col) %in% conditions)) {
      message("Warning: no color names are found in group conditions... (all will be assigned from default palette)")
    }
  
  } else {
    col = c()
  }
    
  missing = conditions[!conditions %in% names(col)]
  
  col = c(col, setNames(hcl.colors(length(missing), palette), missing))
  col = col[names(col) %in% conditions]
    
    
  
    
  
  if (side %in% c(1,3)) {
    
    for (gi in unique(gr$group_order)) {
      g.df <- gr[gr$group_order == gi,]
      cond = g.df[[gname]][1]
      
      rect(min(g.df$x), box.y1, max(g.df$x)+1, box.y2, col=col[cond])
      text(mean(c(min(g.df$x), max(g.df$x)+1)), text.y, cond, cex=cex)
    }
    
    if (label_just == 'right') {
      x1 = max(gr$x)+nh$gap.x+1; y1 = mean(c(box.y1,box.y2))
      text(x1, y1, gname, adj=c(0,0.5), font=2, cex=cex)
      points(x1-nh$gap.x*0.5, y1, pch=-9668, cex=0.5)
      
    } else if (label_just == 'left') {
      x1 = min(gr$x)-nh$gap.x; y1=mean(c(box.y1,box.y2))
      text(x1, y1, gname, adj=c(1,0.5), font=2, cex=cex)
      points(x1+nh$gap.x*0.5, y1, pch=-9658, cex=0.5)
    }
    
  } else if (side %in% c(2,4)) {
    
    for (gi in unique(gr$group_order)) {
      g.df <- gr[gr$group_order == gi,]
      cond = g.df[[gname]][1]
      
      rect(box.x1, min(g.df$y), box.x2, max(g.df$y)+1, col=col[cond])
      text(text.x, mean(c(min(g.df$y), max(g.df$y)+1)), srt=90, cond, cex=cex)
    }
    
    if (label_just == 'right') {
      x1 = mean(c(box.x1,box.x2)); y1 = max(gr$y)+nh$gap.y
      text(x1, y1, gname, adj=c(0,0.5), srt=90, font=2, cex=cex)
      points(x1, y1-nh$gap.y*0.5, pch=-9660, cex=0.5)
      
    } else if (label_just == 'left') {
      x1 = mean(c(box.x1,box.x2)); y1 = min(gr$y)-nh$gap.y
      text(x1, y1, gname, adj=c(1,0.5), srt=90, font=2, cex=cex)
      points(x1, y1+nh$gap.y*0.5, pch=-9650, cex=0.5)
    }
  }
  
  
  nh$legend[[gname]] = col
  nh$boundaries <- boundaries
  return(nh)
}


nheatmap_annotate <- function(nh, side, a.df, col=NULL, percent=0.05, palette='Viridis', 
                              show_bounding_box=F, type='rect', label_just='right') {
  
  list2env(nheatmap_boundaries(nh, side, percent=percent, show_bounding_box = show_bounding_box), environment())
  
  
  aname=names(a.df)[1]
  
  conditions = unique(a.df[[aname]])
  
  if (!is.null(col)) {
    if (!any(names(col) %in% conditions)) {
      message("Warning: no color names are found in group conditions... (all will be assigned from default palette)")
    }
    
  } else {
    col = c()
  }
  
  missing = conditions[!conditions %in% names(col)]
  col = c(col, setNames(hcl.colors(length(missing), palette), missing))
  col = col[names(col) %in% conditions]
  
  
  if (side %in% c(2,4)) {
    
    gr = nh$groups$rows
    col_ordered <- col[a.df[match(rownames(gr), rownames(a.df)),1]]
    
    xy_mean = mean(c(xy1,xy0))
    
    if (type == 'rect') {
      rect(xy0, gr$y, xy1, gr$y + 1, col=col_ordered, border=NA)
    } else if (type == 'points') {
      points(rep(xy_mean, nrow(gr)), gr$y + 0.5, col=col_ordered, pch=19)
    }
    
    if (label_just == 'right') {
      x1 = mean(c(xy0,xy1)); y1 = max(gr$y)+nh$gap.y
      text(x1, y1, aname, adj=c(0,0.5), font=2, srt=90, cex=cex)
      points(x1, y1-nh$gap.y*0.5, pch=-9660, cex=0.5)
      
    } else if (label_just == 'left') {
      x1 = mean(c(xy0,xy1)); y1 = min(gr$y)-nh$gap.y
      text(x1, y1, aname, adj=c(1,0.5), font=2, srt=90, cex=cex)
      points(x1, y1+nh$gap.y*0.5, pch=-9650, cex=0.5)
    }
    
  } else if (side %in% c(1,3)) {
    
    gr = nh$groups$cols
    col_ordered <- col[a.df[match(rownames(gr), rownames(a.df)),1]]
    
    xy_mean = mean(c(xy1,xy0))
    
    if (type == 'rect') {
      rect(gr$x, xy0, gr$x + 1, xy1, col=col_ordered, border=NA)
    } else if (type == 'points') {
      points(gr$x + 0.5, rep(xy_mean, nrow(gr)), col=col_ordered, pch=19)
      
    }
    
    if (label_just == 'right') {
      x1 = max(gr$x)+nh$gap.x+1; y1 = mean(c(xy0,xy1))
      text(x1, y1, aname, adj=c(0,0.5), font=2, cex=cex)
      points(x1-nh$gap.x*0.5, y1, pch=-9668, cex=0.5)
      
    } else if (label_just == 'left') {
      x1 = min(gr$x)-nh$gap.x; y1 = mean(c(xy0,xy1))
      text(x1,y1, aname, adj=c(1,0.5), font=2, cex=cex)
      points(x1+nh$gap.x*0.5, y1, pch=-9658, cex=0.5)
    }
  }
  nh$boundaries <- boundaries
  nh$legend[[aname]] = col
  return(nh)
}

nheatmap_legend <- function(nh, add=F) {
  
  leg <- nh$legend
  lines = length(unlist(leg)) + length(leg) + 3
  
  par(mar=rep(0.3,4))
  
  
  leg.df <- data.frame()
  i = 0
  for (grp in names(leg)) {
    i = i + 1
    for (col_i in 1:length(leg[[grp]])) {
      i = i + 1
      col = leg[[grp]][col_i]
      cond = names(leg[[grp]])[col_i]
      
      leg.df <- rbind(leg.df, data.frame(group=grp, color=col, condition=cond, y=i))
      
      
      # print(paste(grp, col_i, col, cond, sep='  '))
      # text(1,i, grp)
      # text(3,i,cond)
      # points(2,i,pch=22, bg=col, cex=3)
    }
  }
  
  
  
  if (!add) {
    plot(1,1,type='n', xlim=c(0,lines), ylim=c(0,lines), xlab='', ylab='', axes=F)
  }
  
  
  grp_width = max(strwidth(names(leg)))*1.2
  
  for (grp in unique(leg.df$group)) {
    df <- leg.df[leg.df$group == grp,]
    
    x1 = 1
    x2 = x1 + grp_width
    y1 = min(df$y)
    y2 = max(df$y) + 1
    
    rect(x1, y1, x2, y2, col='grey90')
    text(mean(c(x1,x2)), mean(c(y1,y2)), grp)
    
    for (i in 1:nrow(df)) {
      rect(x2+1.1, df$y[i]+0.1, x2+1.9, df$y[i]+0.9, col=df$color[i])
      text(x2+3, df$y[i]+0.5, df$condition[i], adj=c(0,0.5))
      
    }
    
  }
  
}

nheatmap_names <- function(nh, side, percent=0.1, cutoff=T, cex=0.8, 
                           show_bounding_box = F) {
  
  list2env(nheatmap_boundaries(nh, side, percent=percent, show_bounding_box = show_bounding_box), environment())
  
  if (side == 1) {
    gr = nh$groups$cols
    labels = row.names(gr)
    text(gr$x+0.5, xy0, labels, cex=cex, srt=90, adj=c(1,0.5))
    
  } else if (side == 3) {
    gr = nh$groups$cols
    labels = row.names(gr)
    text(gr$x+0.5, xy0, labels, cex=cex, srt=90, adj=c(0,0.5))
    
  } else if (side == 2) {
    gr = nh$groups$rows
    labels = row.names(gr)
    text(xy0, gr$y+0.5, labels, cex=cex, srt=0, adj=c(1,0.5))
    
  } else if (side == 4) {
    gr = nh$groups$rows
    labels = row.names(gr)
    text(xy0, gr$y+0.5, labels, cex=cex, srt=0, adj=c(0,0.5))
    
  }
  
  
  nh$boundaries <- boundaries
  return(nh)
  
}





nheatmap_dend <- function(nh, side, percent=0.1, cutoff=T, cex=0.8, 
  show_bounding_box = F) {
  
  
  list2env(nheatmap_boundaries(nh, side, percent=percent, show_bounding_box = show_bounding_box), environment())
  
  if (side %in% c(1,3)) {
    gr = nh$groups$cols
  } else if (side %in% c(2,4)) {
    gr = nh$groups$rows
  }
  
  
  for (gi in unique(gr$group_order)) {
    g.df <- gr[gr$group_order == gi,]
    
    if (side %in% c(2,4)) {
      cl = nh$groups$row_clusters[[gi]]
      
    } else if (side %in% c(1,3)) {
      cl = nh$groups$col_clusters[[gi]]
    }
    
    
    if (class(cl) == 'hclust') {
    
    
      scale.xy = max(g.df$x, g.df$y) - min(g.df$x, g.df$y) + 1
      pos.xy = min(g.df$x, g.df$y)
      width = abs(xy1-xy0)
      
      d = as.ndendrogram(cl)
      
      if (side == 1) {
        plot.ndendrogram(d, add=T, 
                         pos.y = xy1, pos.x=pos.xy-0.5, 
                         scale.x=scale.xy, 
                         scale.y=width, 
                         flip.y=T,
                         horiz=F)
        
      } else if (side == 3) {
        plot.ndendrogram(d, add=T, 
                         pos.y = xy0, pos.x=pos.xy-0.5, 
                         scale.x=scale.xy, 
                         scale.y=width, 
                         flip.y=F,
                         horiz=F)
        
      } else if (side == 2) {
        plot.ndendrogram(d, add=T, 
                         pos.x = xy1, pos.y=pos.xy-0.5, 
                         scale.y=scale.xy, 
                         scale.x=width, horiz=T, flip.x=T)
        
      } else if (side == 4) {
        plot.ndendrogram(d, add=T, 
                         pos.x = xy0, pos.y=pos.xy-0.5, 
                         scale.y=scale.xy, 
                         scale.x=width, horiz=T, flip.y=F)
      }
    }
  }
  
  nh$boundaries <- boundaries
  return(nh)
  
}




# nh <- nheatmap(e.df, 
#                group_rows=data.frame(row.names=rownames(e.df), 
#                                      GroupA=rep_len(c("these","are","tests",'tests','tests'), nrow(e.df)), 
#                                      GroupB=rep_len(c("GroupB2","GroupB1"), nrow(e.df))),
#                group_cols=data.frame(row.names=colnames(e.df), 
#                                      ColGroupA=rep_len(c("ColA1","ColA1", "ColA2"), ncol(e.df))))

# nh <- nheatmap_group(nh, 'GroupA', 4)
# nh <- nheatmap_group(nh, 'GroupA', 2)
# nh <- nheatmap_group(nh, 'ColGroupA', 1, 'Inferno')
# nh <- nheatmap_group(nh, 'ColGroupA', 3, 'Inferno')
# 
# nh <- nheatmap_names(nh, 2, cex=0.3)
# nh <- nheatmap_names(nh, 3, cex=0.3)
# nh <- nheatmap_names(nh, 4, cex=0.3)
# nh <- nheatmap_names(nh, 1, cex=0.3)
# 
# nh <-nheatmap_dend(nh, 1, show_bounding_box = F)
# nh <-nheatmap_dend(nh, 2, show_bounding_box = F)
# nh <-nheatmap_dend(nh, 3, show_bounding_box = F)
# nh <-nheatmap_dend(nh, 4, show_bounding_box = F)

# nh <- nheatmap_annotate(nh, 1,
#                   a.df = data.frame(row.names=colnames(e.df),
#                                         annA=rep_len(c("A","B","C","C"), ncol(e.df))),
#                   show_bounding_box = F)
# 
# nh <- nheatmap_annotate(nh, 2,
#                   a.df = data.frame(row.names=rownames(e.df),
#                                     annA=rep_len(c("A","B","C","C"), nrow(e.df))),
#                   show_bounding_box = F)
# 
# nh <- nheatmap_annotate(nh, 3,
#                   a.df = data.frame(row.names=colnames(e.df),
#                                     annA=rep_len(c("A","B","C","C"), ncol(e.df))),
#                   show_bounding_box = F)
# 
# nh <- nheatmap_annotate(nh, 4,
#                         a.df = data.frame(row.names=rownames(e.df),
#                                           annA=rep_len(c("A","B","C","C"), nrow(e.df))),
#                         show_bounding_box = F)






