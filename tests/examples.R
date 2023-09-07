library(expressionSetDF)
datos <- data.frame(A = c(1, 2, 3, 7), B = c(4, 5, 6, 0), 
                    C=c("C", "C", "T", "T"))
sampleNames <- c("S1", "S2", "S3", "S4")
varNames <- c("G1", "G2", "Group")
rownames(datos) <- sampleNames
colnames(datos) <- varNames
phenoDat <-  data.frame(sampleGrp = c(rep("H", 2), rep("A", 2)))
rownames(phenoDat) <- rownames(datos)
featureDat <- data.frame(varGrp= c(rep("c", 2), "n"))
rownames(featureDat) <- colnames(datos)

identical(rownames(datos), rownames(phenoDat))
identical(colnames(datos), rownames(featureDat))

es_df0 <- expressionSetDF(exprs = datos,
                          phenoData = NULL,
                          featureData = NULL)
es_df <- expressionSetDF(exprs = datos,
                         phenoData = phenoDat,
                         featureData = featureDat)
exprs.eDF(es_df)
phenoData.eDF(es_df)
featureData.eDF(es_df)
es_subset <- es_df[1:2, ]
es_subset

es_reordered <- es_df[c(3,2,1), c(2,1)]
es_reordered
filterColumns.eDF(es_df, condition = "sum(.) <= 10")
filterRows.eDF(es_df, condition = "sum(.) <= 10")
