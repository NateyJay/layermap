
# R course - Session 6

#### *Advanced heatmap generation using the Layermap library*

------------------------------------------------------------------------

## Introduction

R has many packages for producing heatmaps. Last session, you learned about pheatmap - an excellent package for great heatmaps.

![](https://github.com/NateyJay/layermap/raw/main/images/Logo.png){width="436"}

<https://github.com/NateyJay/layermap>

Here, i introduce my R package "Layermap", which allows for simple generation of heatmaps with many layers of information. These improve on pheatmap in several ways:

1.  Simple syntax, utilizing 3 input data structures.

2.  Flexible formatting, allow for unique and customized views of your heatmap.

3.  Advanced methods, including isolation of pre-determined groupings.

4.  Uses R base-graphics, allowing it flexibility with some other tools.

5.  Creates plots sequentially, allowing the user to see the direct affect of commands one-by-one.

6.  Clean, high-quality image output, ready for publication.

It should also be mentioned that many other tools exist for heatmaps, though few have the ability to make figures like this. ComplexHeatmap is a well used tool that can make very complicated images (very powerful, very well cited). Layermap is considerably more modest in it's scope, though it can produce very complex heatmaps in it's own right.

## Practical session

#### 1) Installing the package and loading it

``` r
install.packages("devtools") #Installing from CRAN if you don't have devtools

devtools::install_github("nateyjay/layermap", upgrade='n')
library(layermap)
```

#### 2) Basics of plotting in R/layermap

We use `data.frames` as the base data structure of layermaps. Ideally, this includes 3 objects:

The values `val.df` (this is akin to a data matrix). All data must be the same format (numeric), otherwise data will be interpreted as a categorical heatmap.

`row.df` and `column.df` refer to the attributes for rows and columns of the `val.df` . These can be numeric, characters, factors, or whatever. Essentially: names for the rows in these data.frames must match the row and column names of the `val.df`.

[![](https://github.com/NateyJay/layermap/raw/main/images/DataStructures.png){alt="data_structures"}](https://github.com/NateyJay/layermap/blob/main/images/DataStructures.png)

Lets look at some data in this format:

``` r
# opening the object ND - a "list" containing 3 data frames

## object ND is loaded when you load the layermap library.

names(ND)

value.df  = ND$values
column.df = ND$columns
row.df    = ND$rows

## what do they look like?
head(value.df)
head(column.df)
head(row.df)
```

Here are a couple of very powerful and common R-functions to help with data frames.

``` r
## tools for peeking at the data
head()
tail()

## making data.frames and converting objects to them
data.frame(col_a = vector_a, col_b = vector_b)
data.frame(row.names=..., col_a = vector_a, col_b = vector_b) # with defined rownames
as.data.frame(other_type_of_object)

## checking/adding dimension names in a data frame
colnames()
names() # the same as colnames with df's
rownames()

## adding a new column with R standard syntax
my_data_frame$new_row <- vector_to_add

## changing rownames for a dataframe
rownames(my_data_frame) <- rownames_to_add

## deleting a data frame column
my_data_frame$column_name <- NULL
```

Since this plots in the base graphics environment, this means we need to make the plotting space using the `par()` function. We can set the margin space using command `par(mar=c(1,2,3,4))` where those values refer to the bottom (1), left (2), top (3) and right (4) sides of the plotting space.

Numbers for these margins refer to "lines" when printed, and layermap also uses this language. In a basic sense, a margin of `2.5` on the bottom means you can (generally) plot 2.5 layers there.

[![](https://github.com/NateyJay/layermap/raw/main/images/Margins.png){alt="margins" width="527"}](https://github.com/NateyJay/layermap/blob/main/images/Margins.png)

Above all, [*remember the mighty question mark*]{.underline} `?`.

if you do not know what a function does or remember how to use it, the help files will tell you:

``` r
?layermap
?lp_annotate
?lp_dend

?par
```

#### 3) Simple example

Lets first try with a common R data set: AirPassengers

``` r
AirPassengers
class(AirPassengers)
```

You can see this is time-course data. It looks like a `data.frame`, but the class shows us it is different.

We will do a little R *jiu jitsu* to convert this to a `data.frame` object.

``` r
air.df <- as.data.frame(matrix(AirPassengers, ncol=12, byrow=T), row.names = 1949:1960)
names(air.df) <- month.abb
head(air.df)
```

Now, we can use layermap to make a really simple representation.

``` r
## making the initial layermap object
lp <- layermap(air.df, zlim=c(0, 500))

# adding layers. This process re-writes the lp object over and over again (which saves coordinates).

# making a dendrogram on the right (side-4)
lp <- lp_dend(lp, 4)

# plotting column names on top (side-3)
lp <- lp_names(lp, 3)

# plotting row names on left (side-2)
lp <- lp_names(lp, 2)

# plotting a legend on the bottom (side-1)
lp <- lp_color_legend(lp, 1, titles ='Passenger count')
```

#### 4) More advanced plotting

The real power from layermap is making custom plots, with custom groupings and visualization. Following layermap example 3, we can compare demographics from the states dataset.

First, we read in the dataset and assemble our constituent data.frames. The first, is a `data.frame` full of different statistical data, given by state.x77.

``` r

data(state)

val.df <- as.data.frame(state.x77)

## adding someo other values from the dataset to this data.frame
val.df$latitude = state.center$x
val.df$longitude = state.center$y
```

Next, we'll for the row and column attribute data.frames.

``` r
## this is built using data from supporting vectors in this dataset. Area information, regional information, etc.
row.df <- data.frame(row.names=state.name,
                     division=state.division,
                     region=state.region,
                     area=state.area,
                     abb=state.abb)

## this is constructed manually. I split the columns into groups by type of variable.
col.df <- data.frame(row.names=colnames(val.df),
                     category=c(rep('demographics', 6),
                                "weather",
                                rep("geography",3)))
```

Finally, considering all of these data are different ranges, meanings, and profiles, we will scale the columns with a z-scale filter (values represent standard deviations away from the mean).

``` r
val.df <- scale(val.df)
```

Plotting this is more complicated. We here, we will provide the `row.df` and `col.df` in the invocation of the plot. We will also give a different color palette. We will also tell it some pre-determined categories by which it can separate the rows/columns (groups).

``` r
par(mar=c(4,16,8,9))
lp <- layermap(val.df,
               row.df=row.df,
               col.df=col.df,
               palette='blues',
               column_groups = 'category',
               row_groups = c('region', 'division'),
               zlim=c(-2,2))


## lp_names() will give the row/col names by default, but you can also specify an attribute to use ('abb')

lp <- lp_names(lp, 2, 'abb', cex=0.65)
lp <- lp_names(lp, 2, cex=0.65)

## making boxes and labels for the groups. For the 'division' layer, we omit labels in the lp_group() step, and add them manually after with lp_group_names()
lp <- lp_group(lp, 2, 'region')
lp <- lp_group(lp, 2, 'division', plot_names=F, palette='Berlin')
lp <- lp_group_names(lp, 2, 'division')

## here we just use the group_names, turning them flat with srt=0
lp <- lp_names(lp, 3)
lp <- lp_group_names(lp, 3, 'category', srt=0, gap=3)


lp <- lp_dend(lp, 4)

lp <- lp_color_legend(lp, 1, titles='z-scaled value')
lp <- lp_legend(lp, 4, 'region', cex=0.7, gap=1)
```

#### 4) Advanced graphing considerations

**Color palettes**

Its a good idea to customize your color scheme for your data, if only to give it some uniqueness. Layermap uses the `hcl` library (standard library), which has many choices for color palettes. Look through them when choosing colors for layers:

<https://colorspace.r-forge.r-project.org/articles/hcl_palettes.html>

These are stunningly easy to use.

``` r
# getting 10 sequential colors in the 'blues' palette
colors = hcl.palette('blues', 10) 
colors
```

**Named colors for categories**

We can use vectors of colors with names referring to their attributes. For an data.frame attribute which contains the values `c('tall', 'short', 'fat')`, we could provide the following colors in an `lp_annotate()` layer: `c(tall='red', short='green', fat='blue2')`.

**Category limits and colorblind friendly**

Be careful. The human eye can only differentiate between 8-10 colors. This is less if you have reduced color perception. Try to use libraries that favor colorblind accesibility and remind yourself that colors are terrible for matching many categories.

**Saving**

It's essential to save your work as you go. A standard recommendation is to save images as an svg, written into the code. This allows you to reproducibly set the plotting dimensions. *Careful* if you have very large data frames, you might be better off saving a raster image like `.png`.

``` r
library(svglite)

par()$din # this secret function will give the dimensions of your current plotting window in r-studio. Perfect if you want to save *exactly* how a plot looks on your screen.

svglite("my_output_file.svg", height=8, width=5)
layermap(test_data)
dev.off()
```

#### 5) Practice - can you reproduce the following plot?

This image was produced using layermap.

[![](https://github.com/NateyJay/layermap/raw/main/images/example.png){alt="left"}](https://github.com/NateyJay/layermap/blob/main/images/example.png)

I have included this test dataset in the layermap installation, so you should easily be able to access it using. Try using layermap to produce the aspects of this in R code.

**Remember:**

1)  you can always us `?` to be told how to use a function

2)  if you can't see a layer plotted in the margin, you probably need to make your margins bigger. Perhaps try `par(mar=c(5,5,5,5))`.

3)  remember, sides are `1-bottom`, `2-left`, `3-top`, `4-right`. `lp` must be returned from each function and called in the next one.

**Getting the data from before:**

``` r
# using the ND dataset

value.df  = ND$values
column.df = ND$columns
row.df    = ND$rows
```

Here is a list of layer plotting functions in layermap... which do you need?

``` r
lp_names()
lp_annotate()
lp_group()
lp_group_names()
lp_group_pie()
lp_dend()
lp_color_legend()
lp_legend()
lp_plot_values()
```

#### 6) More examples

So far, we have tried out examples 5 (airpassengers), 3 (states), and 2 (ND).

Try out all of them to see visualization examples!

``` r

lp_example_1() ## mtcars dataset
lp_example_2() ## ND dataset
lp_example_3() ## states
lp_example_4() ## fivethirtyeight airaccidents
lp_example_5() ## airpassengers (simple)
```
