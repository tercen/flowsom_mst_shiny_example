# FlowSOM MST Shiny operator for Tercen

##### Description

The `FlowSOM_MST` operator for flow cytometry data creates the Minimum Spanning Tree based on FlowSOM clustering.

##### Usage

Input projection|.
---|---
`row`   | represents the variables (e.g. channels, markers)
`col`   | represents the clusters (e.g. cells) 
`y-axis`| is the value of measurement signal of the channel/marker

Input parameters|.
---|---
`nclust`   | Number of clusters to make (default = `NULL`)
`maxMeta`   | Maximal number of cluster (ignored if `nclust` is not `NULL`)
`seed`   | Random seed

Output relations|.
---|---
`Operator view`| MST plot.

##### Details

The operator is a wrapper for the `PlotStars` function of the `FlowSOM` R/Bioconductor package.

#### References

https://bioconductor.org/packages/FlowSOM/

##### See Also

[flowsom_operator](https://github.com/tercen/flowsom_operator)


