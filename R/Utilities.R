


#' Layer boundaries
#'
#' @description Utility for defining layer boundaries
#'
#'
#' @return
#' @export
#'
#' @examples
lp_boundaries <- function(lp, side, size, gap, text.gap=0, text.restriction=F, show_bounding_box=F) {

  # if (side %in% c(2,4)) {
  #   cxy = par()$cxy[1] * 1.33
  #
  # } else {
  #   cxy = par()$cxy[2]
  #
  # }

  size     = lp_line_to_coord(lp$xmax, lp$ymax, side, size)
  gap      = lp_line_to_coord(lp$xmax, lp$ymax, side, gap)



  # text.gap = lp_line_to_coord(lp$xmax, lp$ymax, side, text.gap)

  # rect(10,10,
  #        10+lp_line_to_coord(lp$xmax, lp$ymax, side=4, line=10),
  #        10+lp_line_to_coord(lp$xmax, lp$ymax, side=3, line=10), lwd=2)


  if (side == 1) {
    ## bot

    xy1 = lp$boundaries[side] - gap
    xy0 = lp$boundaries[side] - size - gap

    lp$boundaries[side] = xy0 - text.gap

    if (show_bounding_box) {rect(min(lp$plotting.df$x), xy1, max(lp$plotting.df$x)+1, xy0)}

    sign = -1

  } else if (side == 2) {
    ## left

    xy1 = lp$boundaries[side] - size - gap - text.gap
    xy0 = lp$boundaries[side] - gap - text.gap

    lp$boundaries[side] = xy1

    if (show_bounding_box) {rect(xy1, min(lp$plotting.df$y), xy0, max(lp$plotting.df$y)+1)}

    sign = -1

  } else if (side == 3) {
    ## top

    xy1 = lp$boundaries[side] + size + gap + text.gap
    xy0 = lp$boundaries[side] + gap + text.gap

    lp$boundaries[side] = xy1

    if (show_bounding_box) {rect(min(lp$plotting.df$x), xy1, max(lp$plotting.df$x)+1, xy0)}

    sign = 1
  } else if (side == 4) {
    ## right

    xy1 = lp$boundaries[side] + gap
    xy0 = lp$boundaries[side] + size + gap

    lp$boundaries[side] = xy0 + text.gap

    if (show_bounding_box) {rect(xy0, min(lp$plotting.df$y), xy1, max(lp$plotting.df$y)+1)}

    sign = 1


  }


  if (text.restriction) {
    if (side == 1) {
      xy0 = xy1 - text.restriction
      lp$boundaries[side] = xy0

    } else if (side == 2) {
      xy1 = xy0 - text.restriction
      lp$boundaries[side] = xy1

    } else if (side == 3) {
      xy1 = xy0 + text.restriction
      lp$boundaries[side] = xy1

    } else if (side == 4) {
      xy0 = xy1 + text.restriction
      lp$boundaries[side] = xy0

    }
  }


  ls = list(xy0=xy0,
            xy1=xy1,
            boundaries=lp$boundaries,
            sign=sign)
  return(ls)
}







#' Plot label on annotate or group layers
#'
#' @description Utility for plotting labels on layers
#'
#'
#' @export
#'
#' @examples
lp_label <- function(lp, x_vec, y_vec, side, text, just, offset=0.9, cex) {
  if (side %in% c(2,4)) {
    # offset = strheight("G", font=2, cex=cex) * offset * lp$din_ratio
    offset = lp$gap.y

    x = mean(x_vec)


    srt= 90

    if (just == 'left') {
      y = 0 - offset
      adj = c(1, 0.5)
    } else if (just == 'right') {
      y = lp$ymax + offset
      adj = c(0, 0.5)
    }

  } else if (side %in% c(1,3)) {
    # offset = strwidth("G", font=2, cex=cex) * offset
    offset = lp$gap.x
    y = mean(y_vec)
    srt= 0

    if (just == 'left') {
      x = 0 - offset
      adj = c(1, 0.5)
    } else if (just == 'right') {
      x = lp$xmax + offset
      x = x + 1
      adj = c(0, 0.5)
    }
  }

  text(x, y, text, srt=srt, adj=adj, font=2, cex=cex)


}






#' Convert plot lines to coordinates
#'
#' @description Utility for converting plotting coordinate systems.
#'
#'
#' @return value in coordinates
#' @export
#'
#' @examples
lp_line_to_coord <- function(xmax, ymax, side, line) {

  inches_per_line = par()$mai / par()$mar
  coords_per_inch = c(xmax, ymax) / par()$pin
  coords_per_inch = c(coords_per_inch[2], coords_per_inch[1], coords_per_inch[2], coords_per_inch[1])

  out = line * inches_per_line[side] * coords_per_inch[side]

  return(out)

}




# lp_convert_visible <- function(df, x=NULL, y=NULL) {
#   per_inch = c(ncol(df), nrow(df)) / par()$pin
#
#   if (!is.null(x)) {
#     out = x * per_inch[2] / per_inch[1]
#
#   } else if (!is.null(y)) {
#     out = y / per_inch[2] * per_inch[1]
#
#   } else {
#     stop("must include an x  or a y")
#
#   }
#   return(out)
# }



#' Build a named color vector
#'
#' @description Utility for getting named color vector used in layermap layer functions.
#'
#' @param col named color vector
#' @param conditions list of conditions to colorize
#' @param palette hcl.colors palette used to fill in unnamed colors
#'
#' @return named color vector
#' @export
#'
#' @examples
lp_colorize <- function(col, conditions, palette) {

  if (!is.null(col)) {
    if (!any(names(col) %in% conditions)) {
      message("Warning: no color names are found in group conditions... (all will be assigned from default palette)")
    }

  } else {
    col = c()
  }

  missing = conditions[!conditions %in% names(col)]

  if (length(missing) == 1) {
    col = c(col, setNames(hcl.colors(2, palette)[1], missing))
  } else {
    col = c(col, setNames(hcl.colors(length(missing), palette), missing))
  }
  col = col[names(col) %in% conditions]
  return(col)
}



#' Utility to repair svg header
#'
#' @description Affinity Designer (and possibly other) vector graphics softwares do not import svgs produced through svglite correctly. As this tool produces superior (small) svgs, this utility is meant to fix the file by adding a more explicit header. Overwrites the input file.
#'
#' @param file an svg file produced as the output of svglite::svglite()
#'
#'
#' @export
#'
#' @examples
ADsvg = function(file) {

  fixed_style = "<defs>
  <style>
    rect {
      fill: none;
      stroke: #000000;
        stroke-linecap: round;
      stroke-linejoin: round;
      stroke-miterlimit: 10.00;
    }
  polygon {
    fill: none;
    stroke: #000000;
      stroke-linecap: round;
    stroke-linejoin: round;
    stroke-miterlimit: 10.00;
  }
  line {
    fill: none;
    stroke: #000000;
      stroke-linecap: round;
    stroke-linejoin: round;
    stroke-miterlimit: 10.00;
  }
  polyline {
    fill: none;
    stroke: #000000;
      stroke-linecap: round;
    stroke-linejoin: round;
    stroke-miterlimit: 10.00;
  }
  path {
    fill: none;
    stroke: #000000;
      stroke-linecap: round;
    stroke-linejoin: round;
    stroke-miterlimit: 10.00;
  }
  circle {
    fill: none;
    stroke: #000000;
      stroke-linecap: round;
    stroke-linejoin: round;
    stroke-miterlimit: 10.00;
  }
  text {
    white-space: pre;
  }
  </style>
</defs>"
  temp_value = stringr::str_replace(as.character(as.numeric(Sys.time())), "\\.", "_")
  temp_file = stringr::str_glue("temp_{temp_value}.svg")

  writeLines("", con=temp_file, sep='')

  line_i = 0
  con = file(file, "r")
  while ( TRUE ) {
    line_i = line_i + 1

    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }

    if (line_i == 3) {

      cat(fixed_style, file=temp_file, append=T)

    }

    cat(line, file=temp_file, append=T, sep='\n')

  }
  close(con)
  file.remove(file)
  file.rename(temp_file, file)
}



#' Utility to produce a color palette
#'
#' @description A simple utility used by many lp functions to define color scales for a vector using an hcl.palette.
#'
#' @return a vector of colors
#' @export
#'
#' @examples
vector_to_colors <- function(values, zlim=NULL, na.color='grey', zero_centered_colors=F, palette='viridis', reverse_palette=F) {

  color_n = 100
  color_scale = hcl.colors(color_n, palette=palette, rev=reverse_palette)

  if (!is.null(zlim)) {
    if (zero_centered_colors) {
      message("warning: zero_centered_colors overrided due to zlim inclusion")
    }
  } else if (zero_centered_colors) {

    max_dist_from_zero = max(abs(values), na.rm=T)
    zlim = c(max_dist_from_zero * -1, max_dist_from_zero)
  } else {
    zlim = c(min(values, na.rm=T), max(values, na.rm=T))
  }

  message(stringr::str_glue("zlim: {zlim[1]} to {zlim[2]}"))

  color_i <- round((values - zlim[1]) / (zlim[2]-zlim[1]) * (color_n-1)) +1
  color_i[color_i < 1] <- 1
  color_i[color_i > color_n] <- color_n

  color <- color_scale[color_i]
  if (!isFALSE(na.color)) {
    color[is.na(color)] <- na.color
  }
  return(list(color=color, color_scale=color_scale))
}
