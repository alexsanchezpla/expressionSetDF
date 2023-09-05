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
#' @param phenoData Data frame con información fenotípica.
#' @param featureData Data frame con información de características.
#' @return Un objeto de clase expressionSetDF.
#'
#' @examples
#' es_df <- expressionSetDF(exprs = data.frame(A = c(1, 2, 3), B = c(4, 5, 6)),
#'                          phenoData = data.frame(Sample = c("S1", "S2", "S3")),
#'                          featureData = data.frame(Gene = c("G1", "G2")))
#'
#' @rdname expressionSetDF
#' @export
expressionSetDF <- function(exprs = data.frame(),
                            phenoData = data.frame(),
                            featureData = data.frame())   {
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
  exprs_subset <- x@exprs[i, j, drop = drop]
  phenoData_subset <- x@phenoData[i, , drop = FALSE]
  featureData_subset <- x@featureData[j, , drop = FALSE]

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

  # Seleccionar las columnas que cumplen con la condición
  exprs_filtered <- object@exprs[, col_indices, drop = FALSE]
  featureData_filtered <- object@featureData[, col_indices, drop = FALSE]

  # Crear un nuevo objeto ExpressionSetDF con las columnas filtradas
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

  # Seleccionar las filas que cumplen con la condición
  exprs_filtered <- object@exprs[row_indices, , drop = FALSE]
  phenoData_filtered <- object@phenoData[row_indices, , drop = FALSE]
  featureData_filtered <- object@featureData

  # Crear un nuevo objeto ExpressionSetDF con las filas filtradas
  es_filtered <- expressionSetDF(exprs = exprs_filtered,
                                 phenoData = phenoData_filtered,
                                 featureData = featureData_filtered)

  return(es_filtered)
}

