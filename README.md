This package provides an implementation of a class similar to ExpressionSet

The main difference lies in the fact that the "exprs" field is based on a data.frame, instead of a matrix.
Besides this there is a phenoData and a featureData slots, both data frames as well.  

The class is, by far, not so sofisticated as ExpressionSet or SummarizedExperiment, but it allows to have 
datasets containing distinct data types as the "exprs" field.
