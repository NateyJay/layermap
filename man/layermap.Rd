% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/layermap.R
\name{layermap}
\alias{layermap}
\title{Plot layermap}
\usage{
layermap(
  value.df,
  zlim = NULL,
  col.df = NULL,
  row.df = NULL,
  column_groups = c(),
  row_groups = c(),
  palette = "PuOr",
  reverse_palette = T,
  zero_centered_colors = F,
  zero_as_na = F,
  na_color = NA,
  color_scale = NULL,
  cluster_cols = F,
  cluster_rows = T,
  group_gap = 0.1,
  border = "grey25",
  force_numeric = F,
  column.df = col.df,
  dend_cols = NULL,
  dend_rows = NULL,
  reorder = F
)
}
\arguments{
\item{value.df}{numerical dataframe or matrix with column name and row names. Everything that is included in the dataframe will be plotted as the heatmap, meaning that including any non-numeric values will lead layermap to interpret this as a categorical heatmap.}

\item{zlim}{value limits for color scaling. Defaults to the range of found values.}

\item{row.df}{attribute dataframe which will be used for layer plotting functions on sides 2 and 4. Rownames correspond to value.df rownames.}

\item{column_groups}{a vector of fields from the column.df which will be used to group across the column axis}

\item{row_groups}{same as column_groups, except with rows.}

\item{palette}{option specifying the color palette used for the main heat map. Must be an hcl.pals() defined palette (defaults to PuOr).}

\item{reverse_palette}{boolean value specifying the the orientation of the color palette. Defaults to T, which is best for PuOr.}

\item{zero_centered_colors}{an override option to make sure the z-axis (colors) are centered on 0. This is mainly useful for unspecified zlim.}

\item{color_scale}{an option to specify a custom color set. Accepts an unnamed vector of colors (for numerical data) but can also take named colors if plotting categorical data.}

\item{cluster_cols}{a boolean value showing whether columns should be hierarchically clustered. Clustering is performed within groups, and may give bugs if you have group which do not respond well to clustering (i.e. size of 1)}

\item{cluster_rows}{like above, with rows.}

\item{group_gap}{graphical paramter for the dimension in lines by which groups are separated.}

\item{border}{color for group box borders}

\item{force_numeric}{an override value to treat char or factor data like numeric. This may generate NAs or lost data.}

\item{column.df}{attribute dataframe which will be used for layer plotting functions on sides 1 and 3. Rownames correspond to value.df colnames. Also can be called as col.df}

\item{main}{name for the overall plot.}
}
\value{
lp object
}
\description{
Backbone function for layermap. This makes the basic heatmap structure and plots it. Requires a dataframe of just numerical values with named rows/columns. Row and column attributes are provided through additional dataframes keyed to the value dataframe names.

Returns a layermap object (usually named lp) which is used in lp_* plotting methods to add layers. Each one of these steps will take lp as an input and return it modified as an output.
}
\examples{
data('mtcars')
scaled_mtcars <- scale(mtcars)
heatmap(scaled_mtcars, scale='none')
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
