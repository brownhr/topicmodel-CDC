library(textmineR)


sample <- slice_sample(tweets.text, n = 20000)

test.DTM <- textmineR::CreateDtm(doc_vec = sample$text,
                     doc_names = sample$doc_id,
                     ngram_window = c(1,2),
                     verbose = T)

findk <- function(k)
{
  textmineR::FitLdaModel(
    test.DTM,
    k = k,
    iterations = 2000,
    burnin = 1500,
    optimize_alpha = T,
    calc_likelihood = T,
    calc_coherence = T,
    calc_r2 = T
  )
}

cl <- makeCluster(getOption("cl.cores", 12))


find_k_list <- TmParallelApply(
  X = seq(3, 24, 1),
  FUN = function(k) {
    textmineR::FitLdaModel(
      test.DTM,
      k = k,
      iterations = 2000,
      burnin = 1500,
      optimize_alpha = T,
      calc_likelihood = T,
      calc_coherence = T,
      calc_r2 = T
    )
  },
  export = c("test.DTM")
)
