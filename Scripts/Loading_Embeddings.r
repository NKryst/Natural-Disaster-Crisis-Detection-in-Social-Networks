load_embedding <- function(file_path){
     
     # load full file
     lines <- readLines(file_path)
     
     # create new environment
     embeddings_env <- new.env(hash = TRUE, parent = emptyenv())
     
     # this function is used to convert vectors to unit vectors
     # by dividing their components by vector length
     normalize_vector <- function(a){
         a/sqrt(sum(a**2))
     }
     
     # iterate through the whole file line by line
     for (i in 1:length(lines)) {
         line <- lines[[i]]
         values <- strsplit(line, " ")[[1]]
         label <- values[[1]]
         embeddings_env[[label]] <- normalize_vector(as.double(values[-1]))
     }
     
     embeddings_env
 }
> 
> cosine_similarity <- function(a,b){
     # assuming unit vectors
     # the cosine is just the dot-product
     a %*% b
 }
> 
> 
> most_similar <- function(embeddings, ref_item, n_top = 10){
     # calculate cos similarity to ref_item for all elements
     cos_sims <- eapply(embeddings, cosine_similarity, b = ref_item)
     
     # only look at cos values smaller than 1
     # this will ignore the same element
     cos_sims <- cos_sims[cos_sims < 1]
     
     # return top elements
     cos_sims[order(unlist(cos_sims),decreasing=TRUE)][1:n_top]
 }