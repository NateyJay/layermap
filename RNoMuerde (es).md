# Curso R No Muerde - Sesión 5

#### *Generación de mapas de calor avanzadas con la librería Layermap*

------------------------------------------------------------------------

## Introduction

R tiene muchos paquetes para producir mapas de calor. En la última sesión, conociste pheatmap, un paquete excelente para crear mapas de calor.

![](https://github.com/NateyJay/layermap/raw/main/images/Logo.png){width="436"}

<https://github.com/NateyJay/layermap>

Aquí presento mi paquete R "Layermap", que permite generar de forma sencilla mapas de calor con muchas capas de información. Esto mejora pheatmap de varias maneras:

1.  Sintaxis simple, utilizando 3 estructuras de datos de entrada.

2.  Formato flexible, permite vistas únicas y personalizadas de su mapa de calor.

3.  Métodos avanzados, incluyendo el aislamiento de agrupaciones predeterminadas.

4.  Utiliza gráficos de base R, lo que le permite flexibilidad con algunas otras herramientas.

5.  Crea gráficos secuencialmente, permitiendo al usuario ver el efecto directo de los comandos uno a uno.

6.  Salida de imagen limpia y de alta calidad, lista para su publicación.

También hay que mencionar que existen muchas otras herramientas para hacer mapas de calor, aunque pocas tienen la capacidad de hacer figuras como ésta. ComplexHeatmap es una herramienta muy utilizada que puede hacer imágenes muy complicadas (muy potente, muy bien citada). Layermap es considerablemente más modesto en su alcance, aunque puede producir mapas de calor muy complejos por sí mismo.

## Sesión práctica

#### 1) Instalación y carga del paquete

``` r
install.packages("devtools") #Installing from CRAN if you don't have devtools

devtools::install_github("nateyjay/layermap", upgrade='n')
library(layermap)
```

#### 2) Conceptos básicos de trazado en R/layermap

Utilizamos `data.frames` como estructura de datos base de layermaps. Idealmente, esto incluye 3 objetos:

Los valores `val.df` (esto es parecido a una matriz de datos). Todos los datos deben tener el mismo formato (numérico), de lo contrario los datos se interpretarán como un mapa de calor categórico.

`row.df` y `column.df` se refieren a los atributos de las filas y columnas del val. `df`. Pueden ser numéricos, caracteres, factores o lo que sea. Básicamente: los nombres de las filas de estos data.frames deben coincidir con los nombres de fila y columna del `val.df`.

[![](https://github.com/NateyJay/layermap/raw/main/images/DataStructures.png){alt="data_structures"}](https://github.com/NateyJay/layermap/blob/main/images/DataStructures.png)

Veamos algunos datos en este formato:

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

Aquí hay un par de funciones R muy potentes y comunes para ayudar con los marcos de datos.

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

Como esto traza en el entorno gráfico base, esto significa que necesitamos hacer el espacio de trazado usando la función `par()`. Podemos establecer el espacio de margen usando el comando `par(mar=c(1,2,3,4))` donde esos valores se refieren a los lados inferior (1), izquierdo (2), superior (3) y derecho (4) del espacio de trazado.

Los números de estos márgenes se refieren a «líneas» cuando se imprimen, y layermap también utiliza este lenguaje. En un sentido básico, un margen de `2,5` en la parte inferior significa que (generalmente) puedes trazar 2,5 capas allí.

[![](https://github.com/NateyJay/layermap/raw/main/images/Margins.png){alt="margins" width="527"}](https://github.com/NateyJay/layermap/blob/main/images/Margins.png)

Sobre todo, [*recuerde el poderoso signo de interrogación*]{.underline} *`?`*.

Si no sabes qué hace una función o no recuerdas cómo utilizarla, los archivos de ayuda te lo dirán:

``` r
?layermap
?lp_annotate
?lp_dend

?par
```

#### 3) Ejemplo sencillo

Probemos primero con un conjunto de datos común en R: AirPassengers

``` r
AirPassengers
class(AirPassengers)
```

Puede ver que se trata de datos temporales. Parece un `data.frame`, pero la clase nos muestra que es diferente.

Vamos a hacer un poco de *jiu jitsu* R para convertir esto en un objeto `data.frame`.

``` r
air.df <- as.data.frame(matrix(AirPassengers, ncol=12, byrow=T), row.names = 1949:1960)
names(air.df) <- month.abb
head(air.df)
```

Ahora, podemos utilizar layermap para hacer una representación realmente sencilla.

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

#### 4) Trazado más avanzado

El verdadero poder de layermap es hacer gráficos personalizados, con agrupaciones y visualizaciones personalizadas. Siguiendo el ejemplo 3 de layermap, podemos comparar datos demográficos del conjunto de datos de los estados.

Primero, leemos el conjunto de datos y ensamblamos nuestros data.frames constituyentes. El primero, es un `data.frame` lleno de diferentes datos estadísticos, dado por state.x77.

``` r
data(state)

val.df <- as.data.frame(state.x77)

## adding someo other values from the dataset to this data.frame
val.df$latitude = state.center$x
val.df$longitude = state.center$y
```

A continuación, para la fila y la columna atributo data.frames.

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

Por último, teniendo en cuenta que todos estos datos tienen diferentes rangos, significados y perfiles, escalaremos las columnas con un filtro de escala z (los valores representan desviaciones estándar respecto a la media).

``` r
val.df <- scale(val.df)
```

Trazar esto es más complicado. Aquí, vamos a proporcionar la `row.df` y `col.df` en la invocación de la trama. También le daremos una paleta de colores diferente. También le diremos algunas categorías predeterminadas por las cuales puede separar las filas/columnas (grupos).

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

#### 4) Consideraciones gráficas avanzadas

**Paletas de colores**

Es una buena idea personalizar tu esquema de colores para tus datos, aunque sólo sea para darle algo de singularidad. Layermap utiliza la biblioteca `hcl` (biblioteca estándar), que tiene muchas opciones para las paletas de colores. Revíselas cuando elija los colores para las capas:

<https://colorspace.r-forge.r-project.org/articles/hcl_palettes.html>

Son increíblemente fáciles de usar.

``` r
# getting 10 sequential colors in the 'blues' palette
colors = hcl.palette('blues', 10) 
colors
```

**Colores con nombre para las categorías**

Podemos utilizar vectores de colores con nombres que hagan referencia a sus atributos. Para un atributo data.frame que contenga los valores `c('tall', 'short', 'fat')`, podríamos proporcionar los siguientes colores en una capa `lp_annotate()`: `c(tall='red', short='green', fat='blue2')`.

**Límites de categoría y daltónicos**

Tenga cuidado. El ojo humano sólo puede diferenciar entre 8-10 colores. Esto es menos si tienes una percepción reducida del color. Intenta utilizar bibliotecas que favorezcan la accesibilidad de los daltónicos y recuerda que los colores son terribles para emparejar muchas categorías.

**Guardar**

Es esencial que guardes tu trabajo sobre la marcha. Una recomendación estándar es guardar las imágenes como un svg, escrito en el código. Esto le permite establecer reproduciblemente las dimensiones de trazado. *Cuidado* si usted tiene marcos de datos muy grandes, usted podría ser mejor guardar una imagen rasterizada como `.png`.

``` r
library(svglite)

par()$din # this secret function will give the dimensions of your current plotting window in r-studio. Perfect if you want to save *exactly* how a plot looks on your screen.

svglite("my_output_file.svg", height=8, width=5)
layermap(test_data)
dev.off()
```

#### 5) Práctica: ¿puedes reproducir el siguiente gráfico?

Esta imagen se ha creado con layermap.

[![](https://github.com/NateyJay/layermap/raw/main/images/example.png){alt="left"}](https://github.com/NateyJay/layermap/blob/main/images/example.png)

He incluido este conjunto de datos de prueba en la instalación de layermap, por lo que debería ser fácilmente capaz de acceder a él utilizando. Intenta usar layermap para producir los aspectos de esto en código R.

**Recuerda:**

1.  siempre puedes usar `?` para que te digan cómo usar una función

2.  si no puedes ver una capa trazada en el margen, probablemente necesites hacer tus márgenes más grandes. Tal vez pruebe `par(mar=c(5,5,5,5))`.

3.  recuerde, los lados son `1-inferior`, `2-izquierdo`, `3-arriba`, `4-derecho`. `lp` debe ser devuelto de cada función y llamado en la siguiente.

**Obteniendo los datos de antes:**

``` r
# using the ND dataset

value.df  = ND$values
column.df = ND$columns
row.df    = ND$rows
```

Aquí tienes una lista de funciones de trazado de capas en layermap... ¿cuál necesitas?

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
