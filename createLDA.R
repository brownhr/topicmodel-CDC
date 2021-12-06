cast_lda <- function(dtm, k, ...) {
    lda <- FitLdaModel(
        dtm = dtm,
        k = k,
        alpha = 0.1,
        beta = 0.05,
        calc_coherence = TRUE
    )
    return(lda)
}