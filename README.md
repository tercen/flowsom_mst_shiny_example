# FlowSOM MST Shiny operator for Tercen

##### Description

The `FlowSOM_MST` operator for flow cytometry data creates the Minimum Spanning Tree based on FlowSOM clustering.

##### Usage

Input projection|.
---|---
`labels`   | Model name, corresponding to the output of the [flowsom_operator](https://github.com/tercen/flowsom_operator)


Output relations|.
---|---
`Operator view`| MST plot.

##### Details

The operator is a wrapper for the `PlotStars` function of the `FlowSOM` [R/Bioconductor package](https://bioconductor.org/packages/FlowSOM/).

##### See Also

[flowsom_operator](https://github.com/tercen/flowsom_operator)



