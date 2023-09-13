#' Clase expressionSetDF
#'
#' Esta clase representa un conjunto de datos de expresión genética con campos exprs,
#' phenoData y featureData, donde exprs es un data.frame.
#'
#' @slot exprs Data frame con datos de expresión genética.
#' @slot phenoData Data frame con información fenotípica.
#' @slot featureData Data frame con información de características.
#' @name expressionSetDF
#' @rdname expressionSetDF-class
#' @aliases expressionSetDF-class
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
#' @rdname expressionSetDF
#' @aliases expressionSetDF
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
#' @export
#' @importFrom methods new
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

  es <- new("expressionSetDF",
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
#' es_colset <- es_df[1:2,]
#' es_rowset <- es_df[,1:2]
#'
#' @rdname expressionSetDF
#' @export
`[.expressionSetDF` <- function(x, i, j, drop = FALSE, ...) {
  if (!is.null(x@phenoData)) {
    if (!all(rownames(x@exprs[i, , drop = FALSE]) == x@phenoData$Sample[i])) {
      stop("Subsetting breaks the condition: row names of 'exprs' must match 'Sample' column in 'phenoData'.")
    }
  }

  if (!is.null(x@featureData)) {
    if (!missing(j) && !all(colnames(x@exprs[, j, drop = FALSE]) %in% rownames(x@featureData))) {
      stop("Subsetting breaks the condition: column names of 'exprs' must match row names in 'featureData'.")
    }
  }

  if (missing(i)) {
    i <- seq_len(nrow(x@exprs))
  }

  if (missing(j)) {
    j <- seq_len(ncol(x@exprs))
  }

  exprs_subset <- x@exprs[i, j, drop = drop]
  phenoData_subset <- if (!is.null(i)) x@phenoData[i, , drop = drop] else NULL
  featureData_subset <- if (!is.null(j)) x@featureData[j, , drop = drop] else NULL

  es_subset <- expressionSetDF(exprs = exprs_subset,
                               phenoData = phenoData_subset,
                               featureData = featureData_subset)

  return(es_subset)
}


