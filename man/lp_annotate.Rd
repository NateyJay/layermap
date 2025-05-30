% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Layers.R
\name{lp_annotate}
\alias{lp_annotate}
\title{Plot annotation layer}
\usage{
lp_annotate(
  lp,
  side,
  attribute,
  size = 1,
  gap = 0.4,
  palette = "Viridis",
  reverse_palette = F,
  col = NULL,
  zlim = NULL,
  zero_centered_colors = F,
  show_bounding_box = F,
  type = "rect",
  label.cex = 0.8,
  border = NA,
  group.border = NA,
  group.lwd = 1,
  lwd = 1,
  pt.cex = 1,
  pch = 19,
  plot_label = T,
  label = attribute,
  label.just = "right"
)
}
\arguments{
\item{lp}{layermap object.}

\item{side}{value for which side of the plot to apply the layer (1-bottom, 2-left, 3-top, 4-right).}

\item{attribute}{name for the attribute which will be plotted in the layer. For group, this must be defined in column_groups or row_groups.}

\item{size}{the space (in lines) for this layer in the margin.}

\item{gap}{the space (in lines) between this layer and the prior layer.}

\item{palette}{hcl.colors palette to fill in unnamed conditions colors.}

\item{reverse_palette}{logical setting the order for the palette.}

\item{col}{a named vector, where colors are the values and names are conditions that are defined in the attribute/group that is plotted.}

\item{zlim}{vector for the c(min, max) values for the z-scale (colors). Values over or under will be truncated to the marginal colors.}

\item{zero_centered_colors}{an option to force the center of the palette/colors to coincide with zero. Useful for data fold-change data or other data where zero is neutral.}

\item{show_bounding_box}{shows the bounding space for this layer (useful for troubleshooting)}

\item{type}{option for the plotting form: c('rect','point'). rect shows a bordered box and point is a pch-defined shape.}

\item{label.cex}{cex value relating to layer label text size.}

\item{border}{color for rect border. Default is to not plot the border ("NA").}

\item{group.border}{color for border around whole group of attribute boxes. Default is to not plot the border ("NA").}

\item{group.lwd}{group border line width.}

\item{lwd}{border line width.}

\item{pt.cex}{relating to the size of the point plotted.}

\item{pch}{character type for point.}

\item{plot_label}{logical for whether layer labels should be plotted.}

\item{label}{text to be used as the label name.}

\item{label.just}{the justification side for a layer label (right or left).}
}
\value{
layermap object
}
\description{
Function for plotting an annotation layer based on column or row attributes.
}
