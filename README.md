<p align="left"><img src="images/Logo.png" alt="main logo" width="400" /></p>


# nheatmap
A modular heatmap library for R-base graphics
 
## But why?
There are many excellent fully featured heatmap/plot libraries. While those may be excellent for data exploration, they often produce images which are difficult to get to **"publication quality"**. Figure quality is *essential* for demonstration of your data and this is no more relevant than with heatmaps, which can be notorious for being inscruitably dense. 

This package is meant to address a couple of what I see as a couple common problems which are **hard to fix in other tools:**

* **Color control poorly tuned** to the data type (zero-centered data vs magnitude data).
* **Unlabeled annotations**, usually relying on matching games with indistinct colors.
* **No flexiblity in the orientation**, or manner of annotated attributes (sides, different formats).
* **No tiers of organization**, usually relying on clustering/not clustering as the only form of ordering.

# Basics of nheatmap 

### structure

<p align="center"><img src="images/DataStructures.png" alt="data_structures" width="800" /></p>
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

### invocation
This function is modular, so it relies on multiple commands to build a complete plot. The main command is `nheatmap()`, which will produce the unannotated plot. The output of this will be saved and passed into subsequent commands to add layers to the annotation.

```
nh <- nheatmap(nh, value.df=value.df, column.df=column.df, row.df=row.df, column_groups=c("groupA"))

nh <- nheatmap_group(nh, side=3, gname='groupA')
nh <- nheatmap_dend(nh, side=2, lwd=2)
nh <- nheatmap_annotate(nh, side=1, aname='anotherColumn')
```

Layer-functions add a layer feature next to the plot, with the side corresponding to r-plotting sides `(1-bottom, 2-left, 3-top, 4-right)`. Sides make it important to give the correct attribute names for the sides `(sides 1,3 ~ column.df, sides 2,4 ~ row.df)`.


### grouping

Sometimes, you want to show heatmaps where some attributes are grouped together and won't inter-mingle with other groups upon clustering. This is a core-feature of nheatmap, and allows the user to highlight multiple layers of groups in the data for each axis. 

Groups are derived from attribute columns in the `row.df` and `column.df`. As these affect the actual plotting order and dimensions of the heatmap, these must be specified in the initial invocation of `nheatmap()` using the following options:
* `row_groups` and/or `col_groups` - vectors of which columns for the respective dataframes should be grouped. These values must be found in the attributes columns of those dataframes.
* `group_gap` - the distance (in proportion of plotting area) by which groups should be separated. Default = 0.02.

Group layers can be added using the `nheatmap_group()` function. Multiple group layers may be plotted, if multiple group attributes were specified.

### 




