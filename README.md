## R package `summarytrees`

R package for computation and visualization of summary trees

#### Introduction

<style>
.node {
    font: 12px sans-serif;
}

.link {
    fill: none;
    stroke: black;
    stroke-width: 0.8px;
    stroke-opacity: 0.2;
}
</style>


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

<svg width="900" height="260">
<g transform="translate(0,-145)">
<path class="link" d="M0,256C112.5,256 112.5,152.20000000000002 225,152.20000000000002"></path><path class="link" d="M0,256C112.5,256 112.5,254.20000000000002 225,254.20000000000002"></path><path class="link" d="M0,256C112.5,256 112.5,359.79999999999995 225,359.79999999999995"></path><path class="link" d="M225,254.20000000000002C337.5,254.20000000000002 337.5,193.00000000000003 450,193.00000000000003"></path><path class="link" d="M225,254.20000000000002C337.5,254.20000000000002 337.5,213.40000000000003 450,213.40000000000003"></path><path class="link" d="M225,254.20000000000002C337.5,254.20000000000002 337.5,254.20000000000002 450,254.20000000000002"></path><path class="link" d="M0,256C112.5,256 112.5,193.00000000000003 225,193.00000000000003"></path><path class="link" d="M225,254.20000000000002C337.5,254.20000000000002 337.5,295 450,295"></path><path class="link" d="M0,256C112.5,256 112.5,233.8 225,233.8"></path><path class="link" d="M225,254.20000000000002C337.5,254.20000000000002 337.5,274.6 450,274.6"></path><path class="link" d="M0,256C112.5,256 112.5,172.60000000000002 225,172.60000000000002"></path><path class="link" d="M225,359.79999999999995C337.5,359.79999999999995 337.5,339.4 450,339.4"></path><path class="link" d="M225,359.79999999999995C337.5,359.79999999999995 337.5,380.20000000000005 450,380.20000000000005"></path><path class="link" d="M225,254.20000000000002C337.5,254.20000000000002 337.5,233.8 450,233.8"></path><path class="link" d="M225,254.20000000000002C337.5,254.20000000000002 337.5,315.40000000000003 450,315.40000000000003"></path><path class="link" d="M0,256C112.5,256 112.5,213.4 225,213.4"></path><path class="link" d="M225,359.79999999999995C337.5,359.79999999999995 337.5,359.8 450,359.8"></path><g class="node" transform="translate(0,256)"><rect height="12" width="38.6875" x="0" y="-6" style="stroke: rgb(255, 255, 255); fill: rgb(255, 255, 255);"></rect><text x="0" dy=".35em" transform="translate(0)" text-anchor="start" style="fill-opacity: 1;">Top (0)</text></g><g class="node" transform="translate(225,152.1999969482422)"><rect height="12" width="79.07311226528167" x="0" y="-6" style="stroke: rgb(51, 160, 44); fill: rgb(51, 160, 44);"></rect><text x="0" dy=".35em" transform="translate(0)" text-anchor="start" style="fill-opacity: 1;">9 others (455216)</text></g><g class="node" transform="translate(225,254.1999969482422)"><rect height="12" width="0" x="0" y="-6" style="stroke: rgb(31, 120, 180); fill: rgb(31, 120, 180);"></rect><text x="-3" dy=".35em" transform="translate(0)" text-anchor="end" style="fill-opacity: 1;">World (0)</text></g><g class="node" transform="translate(225,359.79998779296875)"><rect height="12" width="0.0015633413816463723" x="0" y="-6" style="stroke: rgb(178, 223, 138); fill: rgb(178, 223, 138);"></rect><text x="0" dy=".35em" transform="translate(0)" text-anchor="start" style="fill-opacity: 1;">Regional (9)</text></g><g class="node" transform="translate(450,193)"><rect height="12" width="83.00908475047335" x="0" y="-6" style="stroke: rgb(31, 120, 180); fill: rgb(31, 120, 180);"></rect><text x="0" dy=".35em" transform="translate(0)" text-anchor="start" style="fill-opacity: 1;">84 others (477875)</text></g><g class="node" transform="translate(450,213.39999389648438)"><rect height="12" width="84.0910906911706" x="0" y="-6" style="stroke: rgb(31, 120, 180); fill: rgb(31, 120, 180);"></rect><text x="0" dy=".35em" transform="translate(0)" text-anchor="start" style="fill-opacity: 1;">Deutsch (484104)</text></g><g class="node" transform="translate(450,254.1999969482422)"><rect height="12" width="37.76529034723549" x="0" y="-6" style="stroke: rgb(31, 120, 180); fill: rgb(31, 120, 180);"></rect><text x="0" dy=".35em" transform="translate(0)" text-anchor="start" style="fill-opacity: 1;">Français (217411)</text></g><g class="node" transform="translate(225,193)"><rect height="12" width="32.0698639892998" x="0" y="-6" style="stroke: rgb(227, 26, 28); fill: rgb(227, 26, 28);"></rect><text x="0" dy=".35em" transform="translate(0)" text-anchor="start" style="fill-opacity: 1;">Business (184623)</text></g><g class="node" transform="translate(450,295)"><rect height="12" width="29.783042957147078" x="0" y="-6" style="stroke: rgb(31, 120, 180); fill: rgb(31, 120, 180);"></rect><text x="0" dy=".35em" transform="translate(0)" text-anchor="start" style="fill-opacity: 1;">Japanese (171458)</text></g><g class="node" transform="translate(225,233.8000030517578)"><rect height="12" width="31.39554274001633" x="0" y="-6" style="stroke: rgb(166, 206, 227); fill: rgb(166, 206, 227);"></rect><text x="0" dy=".35em" transform="translate(0)" text-anchor="start" style="fill-opacity: 1;">Society (180741)</text></g><g class="node" transform="translate(450,274.6000061035156)"><rect height="12" width="28.021157220031615" x="0" y="-6" style="stroke: rgb(31, 120, 180); fill: rgb(31, 120, 180);"></rect><text x="0" dy=".35em" transform="translate(0)" text-anchor="start" style="fill-opacity: 1;">Italiano (161315)</text></g><g class="node" transform="translate(225,172.60000610351562)"><rect height="12" width="31.26352724556619" x="0" y="-6" style="stroke: rgb(251, 154, 153); fill: rgb(251, 154, 153);"></rect><text x="0" dy=".35em" transform="translate(0)" text-anchor="start" style="fill-opacity: 1;">Arts (179981)</text></g><g class="node" transform="translate(450,339.3999938964844)"><rect height="12" width="20.4703920512776" x="0" y="-6" style="stroke: rgb(178, 223, 138); fill: rgb(178, 223, 138);"></rect><text x="0" dy=".35em" transform="translate(0)" text-anchor="start" style="fill-opacity: 1;">8 others (117846)</text></g><g class="node" transform="translate(450,380.20001220703125)"><rect height="12" width="100.00069481839185" x="0" y="-6" style="stroke: rgb(178, 223, 138); fill: rgb(178, 223, 138);"></rect><text x="0" dy=".35em" transform="translate(0)" text-anchor="start" style="fill-opacity: 1;">North_America (575694)</text></g><g class="node" transform="translate(450,233.8000030517578)"><rect height="12" width="21.373308551477358" x="0" y="-6" style="stroke: rgb(31, 120, 180); fill: rgb(31, 120, 180);"></rect><text x="0" dy=".35em" transform="translate(0)" text-anchor="start" style="fill-opacity: 1;">Español (123044)</text></g><g class="node" transform="translate(450,315.3999938964844)"><rect height="12" width="18.19955184213726" x="0" y="-6" style="stroke: rgb(31, 120, 180); fill: rgb(31, 120, 180);"></rect><text x="0" dy=".35em" transform="translate(0)" text-anchor="start" style="fill-opacity: 1;">Russian (104773)</text></g><g class="node" transform="translate(225,213.39999389648438)"><rect height="12" width="15.198978616963991" x="0" y="-6" style="stroke: rgb(253, 191, 111); fill: rgb(253, 191, 111);"></rect><text x="0" dy=".35em" transform="translate(0)" text-anchor="start" style="fill-opacity: 1;">Science (87499)</text></g><g class="node" transform="translate(450,359.79998779296875)"><rect height="12" width="44.267400858100714" x="0" y="-6" style="stroke: rgb(178, 223, 138); fill: rgb(178, 223, 138);"></rect><text x="0" dy=".35em" transform="translate(0)" text-anchor="start" style="fill-opacity: 1;">Europe (254843)</text></g></g></svg>


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

