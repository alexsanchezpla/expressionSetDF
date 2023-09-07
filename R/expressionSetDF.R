#' Clase expressionSetDF
#'
#' Esta clase representa un conjunto de datos de expresión genética con campos exprs,
#' phenoData y featureData, donde exprs es un data.frame.
#'
#' @slot exprs Data frame con datos de expresión genética.
#' @slot phenoData Data frame con información fenotípica.
#' @slot featureData Data frame con información de características.
#'
#' @name expressionSetDF
#' @rdname expressionSetDF
#' @export
setClass("expressionSetDF",
         slots = list(
           exprs = "data.frame",
           phenoData = "data.frame",
           featureData = "data.frame"
         )
)

#' Constructor para expressionSetDF
#'
#' @param exprs Data frame con datos de expresión genética.
#' @param phenoData Data frame con información fenotípica (opcional).
#' @param featureData Data frame con información de características (opcional).
#' @return Un objeto de clase expressionSetDF.
#'
#' @examples
#' datos <- data.frame(A = c(1, 2, 3, 7), B = c(4, 5, 6, 0), 
#'             C=c("C", "C", "T", "T"))
#' sampleNames <- c("S1", "S2", "S3", "S4")
#' varNames <- c("G1", "G2", "Group")
#' rownames(datos) <- sampleNames
#' colnames(datos) <- varNames
#' phenoDat <-  data.frame(sampleGrp = c(rep("H", 2), rep("A", 2)))
#' rownames(phenoDat) <- rownames(datos)
#' featureDat <- data.frame(varGrp= c(rep("c", 2), "n"))
#' rownames(featureDat) <- colnames(datos)
#' es_df0 <- expressionSetDF(exprs = datos,
#'                        phenoData = NULL,
#'                        featureData = NULL)
#' es_df <- expressionSetDF(exprs = datos,
#'                       phenoData = phenoDat,
#'                       featureData = featureDat)
#' @rdname expressionSetDF
#' @export
expressionSetDF <- function(exprs = data.frame(),
                            phenoData = NULL,
                            featureData = NULL)   {
  # Validar que los nombres de las filas de "exprs" coincidan con "phenoData"
  if (!is.null(phenoData)) {
    if (!identical(rownames(exprs), rownames(phenoData))) {
      stop("The row names of 'exprs' must match the 'Sample' column in 'phenoData'.")
    }
  }
  
  # Validar que los nombres de las columnas de "exprs" coincidan con las filas de "featureData"
  if (!is.null(featureData)) {
    if (!identical(colnames(exprs), rownames(featureData))) {
      stop("The column names of 'exprs' must match the row names of 'featureData'.")
    }
  }
  
  # Si phenoData es NULL, crearlo con los nombres de las filas de "exprs"
  if (is.null(phenoData)) {
    phenoData <- data.frame(Sample = rownames(exprs))
    rownames(phenoData) <- rownames(exprs)
  }

  
  # Si featureData es NULL, crearlo con los nombres de las columnas de "exprs"
  if (is.null(featureData)) {
    featureData <- data.frame(varNames = colnames(exprs))
    rownames(featureData) <- colnames(exprs)
  }
  
  es <- methods::new("expressionSetDF",
                     exprs = exprs,
                     phenoData = phenoData,
                     featureData = featureData)
  return(es)
}


#' Método para acceder al campo "exprs"
#'
#' @param object Un objeto de clase expressionSetDF.
#' @return Un data frame con datos de expresión genética.
#'
#' @examples
#' exprs.eDF(es_df)
#'
#' @rdname expressionSetDF
#' @export
exprs.eDF <- function(object) {
  return(object@exprs)
}

#' Método para acceder al campo "phenoData"
#'
#' @param object Un objeto de clase expressionSetDF.
#' @return Un data frame con información fenotípica.
#'
#' @examples
#' phenoData.eDF(es_df)
#'
#' @rdname expressionSetDF
#' @export
phenoData.eDF <- function(object) {
  return(object@phenoData)
}

#' Método para acceder al campo "featureData"
#'
#' @param object Un objeto de clase expressionSetDF.
#' @return Un data frame con información de características.
#'
#' @examples
#' featureData.eDF(es_df)
#'
#' @rdname expressionSetDF
#' @export
featureData.eDF <- function(object) {
  return(object@featureData)
}

#' Método para realizar subsetting
#'
#' @param x Un objeto de clase expressionSetDF.
#' @param i Índices o condiciones de filas.
#' @param j Índices o condiciones de columnas.
#' @param drop Indica si se deben eliminar las dimensiones no utilizadas.
#' @return Un nuevo objeto de clase expressionSetDF.
#'
#' @examples
#' es_subset <- es_df[1:2, ]
#'
#' @rdname expressionSetDF
#' @export
`[.expressionSetDF` <- function(x, i, j, drop = FALSE, ...) {
  # Validar que las condiciones de filas y columnas no rompan las condiciones
  if (!is.null(x@phenoData)) {
    if (!all(rownames(x@exprs[i, ]) == x@phenoData$Sample[i])) {
      stop("Subsetting breaks the condition: row names of 'exprs' must match 'Sample' column in 'phenoData'.")
    }
  }
  
  if (!is.null(x@featureData)) {
    if (!all(colnames(x@exprs[, j, drop = drop]) == rownames(x@featureData))) {
      stop("Subsetting breaks the condition: column names of 'exprs' must match row names in 'featureData'.")
    }
  }
  
  exprs_subset <- x@exprs[i, j, drop = drop]
  phenoData_subset <- x@phenoData[i, , drop = FALSE]
  featureData_subset <- x@featureData
  
  es_subset <- expressionSetDF(exprs = exprs_subset,
                               phenoData = phenoData_subset,
                               featureData = featureData_subset)
  
  return(es_subset)
}

#' Método para filtrar columnas del campo "exprs"
#'
#' @param object Un objeto de clase expressionSetDF.
#' @param condition Condición para filtrar columnas.
#' @return Un nuevo objeto de clase expressionSetDF con las columnas filtradas.
#'
#' @examples
#' filterColumns.eDF(es_df, condition = "sum(.) <= 10")
#'
#' @rdname expressionSetDF
#' @export
filterColumns.eDF <- function(object, condition) {
  condition_expr <- rlang::parse_expr(condition)
  
  # Evaluar la condición en cada columna
  col_indices <- sapply(object@exprs, function(col) eval(condition_expr, envir = list(. = col)))
  
  # Validar que las columnas cumplen con la condición
  if (!identical(names(col_indices), colnames(object@exprs))) {
    stop("Filtering columns breaks the condition: column names of 'exprs' must match row names in 'featureData'.")
  }
  
  # Seleccionar las columnas que cumplen con la condición
  exprs_filtered <- object@exprs[, col_indices, drop = FALSE]
  featureData_filtered <- object@featureData
  
  es_filtered <- expressionSetDF(exprs = exprs_filtered,
                                 phenoData = object@phenoData,
                                 featureData = featureData_filtered)
  
  return(es_filtered)
}

#' Método para filtrar filas del campo "exprs"
#'
#' @param object Un objeto de clase expressionSetDF.
#' @param condition Condición para filtrar filas.
#' @return Un nuevo objeto de clase expressionSetDF con las filas filtradas.
#'
#' @examples
#' filterRows.eDF(es_df, condition = "sum(.) > 5")
#'
#' @rdname expressionSetDF
#' @export
filterRows.eDF <- function(object, condition) {
  condition_expr <- rlang::parse_expr(condition)
  
  # Evaluar la condición en cada fila
  row_indices <- sapply(1:nrow(object@exprs), function(row_idx) {
    eval(condition_expr, envir = list(. = object@exprs[row_idx, , drop = FALSE]))
  })
  
  # Validar que las filas cumplen con la condición
  if (!identical(row_indices, seq_len(nrow(object@exprs)))) {
    stop("Filtering rows breaks the condition: row names of 'exprs' must match 'Sample' column in 'phenoData'.")
  }
  
  # Seleccionar las filas que cumplen con la condición
  exprs_filtered <- object@exprs[row_indices, , drop = FALSE]
  phenoData_filtered <- object@phenoData
  featureData_filtered <- object@featureData
  
  es_filtered <- expressionSetDF(exprs = exprs_filtered,
                                 phenoData = phenoData_filtered,
                                 featureData = featureData_filtered)
  
  return(es_filtered)
}

#' MultMerge2
#'
#' Combina varios data frames en un único data frame, 
#' manteniendo todas las filas y columnas únicas de cada uno.
#'
#' @param lst Una lista de data frames que se fusionarán.
#' @param all.x Si TRUE, incluir todas las filas de los data frames en la lista original.
#' @param all.y Si TRUE, incluir todas las filas de los data frames en la lista nueva.
#' @param by Nombre de la columna por la que se unirán los data frames (opcional).
#'
#' @return Un nuevo data frame fusionado.
#'
#' @examples
#' Merge data frames keeping all rows from all datasets
#' df1 <- data.frame(matrix(rnorm(20), nrow=10))
#' df2 <- data.frame(min=letters[1:8], may=LETTERS[1:8])
#' dfList <- list(df1, df2)
#' MultMerge2(dfList)
#' 
#' ' Merge data frames keeping ONLY COMMON rows to all datasets
#' df1 <- data.frame(ID = 1:10, Value = runif(10))
#' df2 <- data.frame(ID = 6:15, Value = runif(10))
#' df3 <- data.frame(ID = 1:10, Value = runif(10))
#' dfList <- list(df1, df2, df3)
#' MultMerge2(dfList, all.x = FALSE, all.y = FALSE, by = "ID")
#'
#' @import DescTools
#'
#' @export
MultMerge2 <- function (lst, all.x = TRUE, all.y = TRUE, by = NULL) 
{
  # lst <- list(...) # The original version had "..." instead of "list" as argument
  if (length(lst) == 1) 
    return(lst[[1]])
  if (!is.null(by)) {
    for (i in seq_along(lst)) {
      rownames(lst[[i]]) <- lst[[i]][[by]]
      lst[[i]][by] <- NULL
    }
  }
  unames <- DescTools::SplitAt(make.unique(unlist(lapply(lst, colnames)), 
                                           sep = "."), 
                               cumsum(sapply(utils::head(lst, -1), ncol)) + 1)
  for (i in seq_along(unames)) colnames(lst[[i]]) <- unames[[i]]
  res <- Reduce(function(y, z) merge(y, z, all.x = all.x, all.y = all.x, sort=FALSE), 
                lapply(lst, function(x) data.frame(x, rn = row.names(x))))
  rownames(res) <- res$rn
  res$rn <- NULL
  seq_ord <- function(xlst) {
    jj <- character(0)
    for (i in seq_along(xlst)) {
      jj <- c(jj, setdiff(xlst[[i]], jj))
    }
    return(jj)
  }
  ord <- seq_ord(lapply(lst, rownames))
  res[ord, ]
  if (!is.null(by)) {
    res <- data.frame(row.names(res), res)
    colnames(res)[1] <- by
    rownames(res) <- c()
  }
  return(res)
}


### MODIFICACIONS
# PhenoData i FeatureData han de tenir com noms de files
#     els noms de les files i de les columnes de exprs respectivament

# Revisar els filtres de forma que si cap fila o columna 
#     compleix la condició retorni NULL pero no un error

