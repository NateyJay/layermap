

#' ndendrogram object converter
#'
#' @description utility to make a usable dendrogram for more complex plotting
#'
#' @param d - Base or dendextend Dendrogram object from R
#'
#' @return ndendrogram object
#' @export
#'
#' @examples
as.ndendrogram <- function(d) {
  d = as.dendrogram(d)



  ## getting node xy coords
  xy.df <- as.data.frame(dendextend::get_nodes_xy(d))
  colnames(xy.df)=c("x","y")


  ## getting node attributes
  xy.df$members <- dendextend::get_nodes_attr(d, 'members')
  # xy.df$nodePar <- dendextend::get_nodes_attr(d, 'nodePar')
  xy.df$height  <- dendextend::get_nodes_attr(d, 'height')
  xy.df$leaf    <- dendextend::get_nodes_attr(d, 'leaf')
  xy.df$leaf[is.na(xy.df$leaf)] <- F
  xy.df$label   <- dendextend::get_nodes_attr(d, 'label')

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


#' ndendrogram plotting function
#'
#' @description plot.ndendrogram plots an ndendgrogram object in specific locations
#'
#' @param d - an ndendrogram object
#' @param horiz - logical assigns whether the dendrogram is plotted vertical or horizontal.
#'
#' @return
#' @export
#'
#' @examples
plot.ndendrogram <- function(d, horiz=T, flip.x=F, flip.y=F, type='square', add=F,
                             scale.x=NULL, scale.y=NULL,
                             pos.x=0, pos.y=0, ...) {

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
    segments(xy.df$x[f], xy.df$y[f], xy.df$x[xy.df$to1[f]], xy.df$y[xy.df$to1[f]], ...)
    segments(xy.df$x[f], xy.df$y[f], xy.df$x[xy.df$to2[f]], xy.df$y[xy.df$to2[f]], ...)

  } else if (type == 'square') {

    f = !xy.df$leaf

    for (to_name in c("to1", "to2")) {

      x1 = xy.df$x[f]
      x2 = xy.df$x[xy.df[f,to_name]]
      y1 = xy.df$y[f]
      y2 = xy.df$y[xy.df[f,to_name]]

      if (horiz) {
        segments(x1, y2, x2, y2, ...)
        segments(x1, y1, x1, y2, ...)
      } else {
        segments(x1, y1, x2, y1, ...)
        segments(x2, y1, x2, y2, ...)
      }
    }


  }

}
