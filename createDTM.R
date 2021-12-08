create_dtm <- function(doc_vec, doc_names, ...) {
    dtm <- CreateDtm(
        doc_vec = doc_vec,
        doc_names = doc_names,
        ngram_window = c(1, 3),
        stopword_vec = c(
            twitter_stopwords,
            stopwords::stopwords(language = "en"),
            stopwords::stopwords(source = "smart")
        ),
        stem_lemma_function = function(x) {
            SnowballC::wordStem(x, language = "porter")
        }
    )
    return(dtm)
}



create_dtm_topicm <- function(x) {
    topicmodels::CTM(
        x = 
    )

}