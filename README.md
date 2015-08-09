## R package `summarytrees`

R package for computation and visualization of summary trees

#### Introduction

`summarytrees` is an R package to help you summarize and visualize a potentially large data set that can be represented by a rooted, node-weighted tree.

The input data looks something like the table below, containing the node ID, the ID of the node's parent, the node's (non-negative) weight, and its label. The example table pictured below contains a snapshot of the [DMOZ topic hierarchy](http://www.dmoz.org/) circa 2015, with over 635,000 nodes (unique topics) in the tree, where each node weight has been set to the number of URLs belonging to the given topic in the hierarchy. There are roughly 3.7 million total URLs (this is the sum of the weights):

|   node| parent| weight|label                                 | level|
|------:|------:|------:|:-------------------------------------|-----:|
|      1|      0|      0|Top                                   |     1|
|      2|      1|      0|Arts                                  |     2|
|      3|      1|      0|Business                              |     2|
|      4|      1|      0|Computers                             |     2|
|      5|      1|      0|Games                                 |     2|
|      6|      1|      0|Home                                  |     2|
|      7|      1|      0|Recreation                            |     2|
| 635853| 635689|      3|Instytut_Mikromechaniki_i_Fotoniki    |    15|
| 635854| 635062|      1|Maronite                              |    15|
| 635855| 635074|      3|Wirtschaftsprüfung_und_Steuerberatung |    15|

The `summarytrees` package implements a dynamic programming algorithm that aggregates the nodes of the input tree in an optimal way, maximizing the entropy of the distribution of the node weights of the aggregated tree among all possible aggregations of a given size, subject to certain constraints. The resulting set of *summary trees* are visualized using d3.js to allow for exploratory data analysis of the strucure and weight distribution of the input tree. Below is a snapshot of the 18-node maximum entropy summary tree of the DMOZ data:

![summarytrees preview](http://www2.research.att.com/~kshirley/figures/dmoz-readme.png)

#### Demonstrations

For a demonstration of the interactive visualization of a set of maximum entropy summary trees for a given data set, visit one of the links below:

Insert a table here with examples, summary stats, computation times, and links to the visualizations.

#### Installation

The `summarytrees` package is still under development, and is currently available here on Github, but not yet on CRAN. To install it, type
```{r}
library(devtools)
install_github("kshirley/summarytrees")
```

#### Vignettes

There are two vignettes included with the package, and the data associated with each vignette is also included with the package.

The first vignette contains an analysis of the Carl Gauss subtree of the Math Genealogy Project (MGP), where the hierarchy of the tree is defined by the advisor-student relationships among mathematicians (starting with Carl Gauss as an advisor) and the node weight for each mathematician is set to 1 by default. To load the vignette, type:
```{r}
vignette("gauss", package = "summarytrees")
```
[Note: This sample of data has been shared with the permission and cooperation of the MGP; please do not re-distribute it. See `help("gauss", package = "summarytrees")` for more.]

The second vignette contains an analysis of the file structure of the source code of R (version 3.2.1), where node weights are set to the sizes (in bytes) of the files in the source code of R, and the directory structure defines the hierarchy.
```{r}
vignette("Rsource", package = "summarytrees")
```

#### Documentation/Help

The original paper describing the algorithm and resulting visualizations is:

(2013). Howard Karloff and Kenneth E. Shirley. "Maximum Entropy Summary Trees", 
<i>Computer Graphics Forum (Proc. EuroVis)</i>, Volume 32, Issue 3, Part 1, pp. 71-80.

A copy of the paper is available [here](http://www.research.att.com/~kshirley/papers/KarloffShirleyWebsite.pdf).

A website describing the work is [here](http://www.research.att.com/~kshirley/summarytrees.html).

