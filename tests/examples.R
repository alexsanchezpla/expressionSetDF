library(expressionSetDF)
es_df <- expressionSetDF(exprs = data.frame(A = c(1, 2, 3), B = c(4, 5, 6)),
                          phenoData = data.frame(Sample = c("S1", "S2", "S3")),
                          featureData = data.frame(Gene = c("G1", "G2")))
exprs.eDF(es_df)
phenoData.eDF(es_df)
featureData.eDF(es_df)
es_subset <- es_df[1:2, ]
es_subset

es_reordered <- es_df[c(3,2,1), c(2,1)]
es_reordered
filterColumns.eDF(es_df, condition = "sum(.) <= 10")
filterRows.eDF(es_df, condition = "sum(.) <= 10")
