## R package `summarytrees`

R package for computation and visualization of summary trees

#### Introduction

`summarytrees` is an R package to help you summarize and visualize a potentially large data set that can be represented by a rooted, node-weighted tree.

The input data looks something like the table below, containing the node ID, the ID of the node's parent, the node's (non-negative) weight, and its label. The example table pictured below contains a snapshot of the [DMOZ topic hierarchy](http://www.dmoz.org/) circa 2015, with over 635,000 nodes (unique topics) in the tree, where each node weight has been set to the number of URLs belonging to the given topic in the hierarchy. There are roughly 3.7 million total URLs (this is the sum of the weights). (The level of each node is also shown in the table, but is not required as input to compute summary trees.) The question that `summarytrees` helps to answer is "What is the distribution of weights in this large tree?"

|   node| parent| weight|label                                 | level|
|------:|------:|------:|:-------------------------------------|-----:|
|      1|      0|      0|Top                                   |     1|
|      2|      1|      0|Arts                                  |     2|
|      3|      1|      0|Business                              |     2|
|      4|      1|      0|Computers                             |     2|
|      5|      1|      0|Games                                 |     2|
|      6|      1|      0|Home                                  |     2|
|      7|      1|      0|Recreation                            |     2|
|    ...|    ...|   ...|...                                    |   ...|
| 635853| 635689|      3|Instytut_Mikromechaniki_i_Fotoniki    |    15|
| 635854| 635062|      1|Maronite                              |    15|
| 635855| 635074|      3|Wirtschaftsprüfung_und_Steuerberatung |    15|

The `summarytrees` package implements a dynamic programming algorithm that aggregates the nodes of the input tree in an optimal way, maximizing the entropy of the distribution of the node weights of the aggregated tree among all possible aggregations of a given size, subject to certain constraints. The resulting set of *summary trees* can be visualized using d3.js to allow for exploratory data analysis of the strucure and weight distribution of the input tree. Below is a snapshot of the 18-node maximum entropy summary tree of the DMOZ data. Click on the picture to see the interactive version of the visualization.

<a href='http://www.kennyshirley.com/summarytrees/dmoz'>
<img src='http://www.kennyshirley.com/figures/dmoz-readme.png'>
</a>

#### Demonstrations

To see some demos, visit one of the links in the first column of the table below. The running times reported for each data set (in minutes and seconds) are from a single run (not averaged over multiple runs) computing maximum entropy summary trees for k = 1, 2, .., K = 100 on a single machine with a 2.20 GHz CPU and over 100 GB of RAM. The rightmost three columns report running times for the 3 different versions of the algorithm that are available. See Section 7 of [our paper](http://www.kennyshirley.com/papers/KarloffShirleyWebsite.pdf) for more details.

|Data            | # Nodes| Total Weight| Max Depth|Optimal |Approx (epsilon = 0.1) |Approx (epsilon = 0.5) |Greedy |
|:---------------|-------:|------------:|---------:|:-------|:-------------------------|:-------------------------|:------|
|Web Traffic     |   19335|    260276921|        17|--    |6:33                      |1:10                      |0:03   |
|Hard Drive      |   15671|       146933|         7|20:25   |21:29                     |3:01                      |0:02   |
|Tree of Life    |   94080|        54121|       123|11:53   |20:44                     |2:34                      |0:09   |
|[Math Genealogy](http://www.kennyshirley.com/summarytrees/gauss)  |   43527|        43527|        17|11:45   |25:22                     |2:45                      |0:04   |
|Org Chart       |   43134|        43134|        10|3:53    |10:03                     |0:52                      |0:03   |
|[DMOZ](http://www.kennyshirley.com/summarytrees/dmoz)            |  635855|      3776432|        15|--    |57:03                     |7:56                      |0:54   |
|[R Source](http://www.kennyshirley.com/summarytrees/Rsource)        |    4704|     77420268|         8|--    |7:04                      |1:20                      |0:00   |
|[DMOZ Top/Sports](http://www.kennyshirley.com/summarytrees/dmozsports) |   15018|        76535|        11|41:57   |53:14                     |6:02                      |0:01   |


#### Installation

The `summarytrees` package is still under development, and is currently available here on Github, but not yet on CRAN. To install it, type
```{r}
library(devtools)
install_github("kshirley/summarytrees", build_vignettes = TRUE)
```

It has been developed on a Mac OS X 10.9.5, using R version 3.1.2, Chrome Version 44.0.2403.130 (64-bit), and Firefox 39.0. More testing is planned for the near future to make sure it all works in multiple environments.

#### Vignettes

There are two vignettes included with the package.

1\. The "Gauss" vignette contains an analysis of the Carl Gauss subtree of the Math Genealogy Project (MGP), where the hierarchy of the tree is defined by the advisor-student relationships among mathematicians (starting with Carl Gauss as an advisor) and the node weight for each mathematician is set to 1 by default. To load the vignette, type:
```{r}
vignette("Gauss", package = "summarytrees")
```
Note: This sample of data has been shared with the permission and cooperation of the Math Genealogy Project; please do not re-distribute it. See `help("Gauss", package = "summarytrees")` for more.

2\. The "DMOZ" vignette contains an analysis of a subset of URLs from the DMOZ (aka Open Directory Project) directory of websites. This data contains all the URLs which are classified into the "Top/Sports" branch of the tree, which consists of about 76,000 URLs (about 2% of the total data set) spread out over about 14,000 unique topics. The node weights are set to the number of URLs belonging to each topic in the hierarchy.
```{r}
vignette("DMOZ", package = "summarytrees")
```

<!--
3. The "RSource" vignette contains an analysis of the file structure of the source code of R (version 3.2.1), where node weights are set to the sizes (in bytes) of the files, and the directory structure defines the hierarchy.
```{r}
vignette("Rsource", package = "summarytrees")
```
-->

#### Documentation/Help

The original paper describing the algorithm and resulting visualizations is:

(2013). Howard Karloff and Kenneth E. Shirley. "Maximum Entropy Summary Trees", 
<i>Computer Graphics Forum (Proc. EuroVis)</i>, Volume 32, Issue 3, Part 1, pp. 71-80.

A copy of the paper is available [here](http://www.kennyshirley.com/papers/KarloffShirleyWebsite.pdf).

A website describing the work is [here](http://www.kennyshirley.com/summarytrees).


