<p align="center"><img src="images/Logo.png" alt="main logo" width="500" /></p>

A modular heatmap library for R-base graphics
 
# But why?
There are many excellent fully featured heatmap/plot libraries. While those may be excellent for data exploration (notably, [pheatmap](https://github.com/raivokolde/pheatmap)), they often produce images which are difficult to get to **"publication quality"**. Figure quality is *essential* for demonstration of your data and this is no more relevant than with heatmaps, which can be notorious for being inscruitably dense. 

This package is meant to address a couple of what I see as a couple common problems which are **hard to fix in other tools:**

* **Color control poorly tuned** to the data type (zero-centered data vs magnitude data).
* **Unlabeled annotations**, usually relying on matching games with indistinct colors.
* **No flexiblity in the orientation**, or manner of annotated attributes (sides, different formats).
* **No tiers of organization**, usually relying on clustering/not clustering as the only form of ordering.

# Installation

Installing can be done simply using devtools. 

```
install.packages("devtools") #Installing from CRAN if you don't have devtools

devtools::install_github("nateyjay/nheatmap")
require(nheatmap)
```


# Basics of nheatmap 

### Structure

<p align="center"><img src="images/DataStructures.png" alt="data_structures" width="700" /></p>
Heatmaps are complicated, there is no avoiding that. Instead, nheatmap tries to fit all essential data for plotting into 3 tables.  

These are:

* `value.df` - a matrix or dataframe which contains only numerical data. This makes up the "heat" portion of the plot, with **rownames** and **colnames** as essential keys to connect to attribute tables.
* `row.df` - a dataframe which contains all of the attributes for the rows of the plot. **Rows** of this dataframe correspond to the **rows** of the `value.df`. This can include an arbitrary number of columns, which make up the attributes. Usually, these are categorical data but it's not required in case your dataframe has other superfluous columns in it.
* `column.df` - Just like the `row.df`, except matching the columns of the plot. **Rows** of this dataframe correspond to the **columns** of the `value.df`

You can easily test the assumptions for naming like this:
```
## setequal() can tell if they have the same values (order is unimportant)
setequal(row.names(row.df), row.names(value.df))
[1] TRUE
setequal(row.names(column.df), col.names(value.df))
[1] TRUE

## reordering the value.df based on names can also do this.
value.df[row.names(row.df), row.names(column.df)] ## should return no errors.
```

*Note:* `row.df` and `column.df` are not required, but are almost certainly necessary to annotate the plot with layers (where the magic lives).

Invocation
----------

This function is modular, so it relies on multiple commands to build a complete plot. The main command is `nheatmap()`, which will produce the unannotated plot. The output of this will be saved and passed into subsequent commands to add layers to the annotation.

```
nh <- nheatmap(nh, value.df=value.df, column.df=column.df, row.df=row.df, column_groups=c("groupA"))

nh <- nheatmap_group(nh, side=3, gname='groupA')
nh <- nheatmap_dend(nh, side=2, lwd=2)
nh <- nheatmap_annotate(nh, side=1, aname='anotherColumn')
```


Common plotting principles
--------------------------

<p align="center"><img src="images/Margins.png" alt="margins" width="500" /></p>

#### Sides
Layer-functions add a layer feature next to the plot, with the side corresponding to r-plotting sides:
`(1-bottom, 2-left, 3-top, 4-right)`  
Sides make it important to give the correct attribute names for the sides:
`(sides 1,3 ~ column.df, sides 2,4 ~ row.df)`

#### Sizes
The general plotting area and layers are spatially defined in terms of the margin lines. In terms of R base graphics, the plotting area is where the colored "heats" are shown, while the margins include the layers.

Margin sizes can be changed using `par(mar)`, as always. Example: `par(mar=c(2,3,3,10))`

Layer dimensions are decided by predominantly 2 attributes: the size of the layer and the gap between this layer and the prior, both in terms of lines. The defaults are `size=4` and `gap=0.4`, meaning that most layers will be 1.5x the size of the gap.

#### Export
Proportion in export is always difficult to work out. Since layer dimensions are based on margins, if you change the plotting area the layers will be dramatically affected. This means, that you will need to replot all the layers. A recommended workflow using R-studio is to reform the plotting window and margin sizes until they meet they look good and then export that same plotting window size. `svglite` is an excellent package for exporting vector graphics.

Example for plotting window size:
```
#once you find a good window, you can save using svglite to save

svglite("Outputplot.svg", par()$din[1], par()$din[2]) #uses the inch dimensions of the current plotting window.

#par("mar") call here
#nheatmap plotting code here

dev.off()

```

Grouping
--------

Sometimes, you want to show heatmaps where some attributes are grouped together and won't inter-mingle with other groups upon clustering. This is a core-feature of nheatmap, and allows the user to highlight multiple layers of groups in the data for each axis.

Groups are derived from attribute columns in the `row.df` and `column.df`. As these affect the actual plotting order and dimensions of the heatmap, these must be specified in the initial invocation of `nheatmap()` using the following options:
* `row_groups` and/or `col_groups` - vectors of which columns for the respective dataframes should be grouped. These values must be found in the attributes columns of those dataframes.
* `group_gap` - the distance (in proportion of plotting area) by which groups should be separated. Default = 0.02.

Group layers can be added using the `nheatmap_group()` function. Multiple group layers may be plotted, if multiple group attributes were specified.

Other layers
------------

Several other layer functions allow building a custom plot.

`nheatmap_dend()` adds a dendrogram for sides that have been hierarchically clustered. This accepts many line-related options from r-base plotting.

`nheatmap_annotate()` adds colored category annotations. These are unlabeled and generally useful to see how clustering relates to categories.

`nheatmap_text()` adds text labels to an axis. Useful to show gene names, symbols (or both!). A r-base character related options.

Colors
------
Colors will automatically be provided from default hcl.colors palettes. For all layers, colors can be specified as a named vector, with the names corresponding to attribute conditions. This can be easily made with `setNames()`:
```
color_vector = setNames(colors, conditions)
```
Any unnamed colors will be substituted from palettes.


# Examples

```
column.df <- read.delim("test/columns.txt")
column.df$treatment <- factor(column.df$treatment, levels=c("Nitrate", "Drought", "ABA"))

row.df <- read.delim("test/rows.txt")
value.df <- read.delim("test/values.txt")

par(mar=c(5,7,5,10))
nh = nheatmap(value.df, zero_centered_colors = T,
                  column.df=column.df, row.df=row.df,
                  column_groups=c('treatment'), row_groups=c("PlantTFDB", 'Nit_GOs'),
              cluster_cols=T,
              group_gap = 0.02)

nh = nheatmap_group(nh, 3, 'treatment', labels=T, label_just = 'left',
                    col=setNames(c('seagreen','tomato'), c('Nitrate','ABA')),
                    show_bounding_box = F)


nh = nheatmap_annotate(nh, 3, 'tissue', label_just = 'left',
                       col=setNames('red', 'Root'))

nh = nheatmap_group(nh, 4, 'PlantTFDB', labels=T, label_just = 'left')
nh = nheatmap_group(nh, 4, 'Nit_GOs', labels=F, label_just = 'left')

nh = nheatmap_names(nh, 2, cex=0.5)
nh = nheatmap_names(nh, 2, 'symbol', cex=0.5)
nh = nheatmap_dend(nh, 4, lwd=1.5, gap=0.4)
```


<p align="center"><img src="images/AnnotatedTest.png" alt="left" width="800" />


To-do list
----------

[x] Make a simple install guide.  
[ ] Complete help documents within R.  
[ ] Complete more reproducible examples.

