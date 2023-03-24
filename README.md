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

## Basic structure

<p align="center"><img src="images/DataStructures.png" alt="data_structures" width="800" /></p>
Heatmaps are complicated - **there is no avoiding that**. Instead, nheatmap tries to fit all essential data for plotting into 3 tables. These are:

* **value.df** - a matrix or dataframe which contains only numerical data. This makes up the "heat" portion of the plot, with **rownames** and **colnames** as essential keys to connect to attribute tables.
* **row.df** - a dataframe which contains all of the attributes for the rows of the plot. **Rows** of this dataframe correspond to the **rows** of the value.df. This can include an arbitrary number of columns, which make up the attributes. Usually, these are categorical data. 
* **column.df** - Just like the row.df, except matching the columns of the plot. **Rows** of this dataframe correspond to the **columns** of the value.df

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


