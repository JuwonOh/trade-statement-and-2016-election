preproc <- function(input.data, start.date = '2015-01-01', lower.thresh = 3){
    ## input data
    if(!is.data.frame(input.data)){
        input.data <- as.data.frame(input.data)
    }
    start.date = "2016-01-01"
  
    input.data$party <- as.factor(input.data$party)
    input.data$state <- as.factor(input.data$state)
    input.data$region <- as.factor(input.data$region)
    ## fixing: difftime(Sys.Date() + 1,  Sys.Date(), units = "mins") 
    input.data$ndate <- as.Date(input.data$date) - as.Date(start.date)
    input.data$trump.binary <- ifelse(input.data$date > '2016-11-08', 1, 0)
    input.data$trump.dates <- input.data$date - as.Date('2016-11-08')
    input.data$trump.dates <- as.numeric(input.data$trump.dates)
    input.data$ndate <- input.data$date - as.Date('2015-01-01')
    input.data$ndate <- as.numeric(input.data$ndate)
    input.data$name <- as.factor(input.data$name)

    middata <- subset(input.data, select = c(name, party, ndate, state, title, noun, trump.binary, trump.dates, region
                                             , pre16, pre12))

    ## middata_1 <- middata %>% 
    ##     mutate(noun = map(noun, unlist), precessed = map_chr(noun, paste, collapse = " "))
    
    middata$noun <- gsub("[]$*?[^{|\\#%&~_/<=>!,:;`\")(}@-],", " ", middata$noun)
    middata$noun <- gsub("\"", " ", middata$noun)
    middata$noun <- gsub("\n", "", middata$noun)
    
    corpus <- corpus(middata$noun)
    docvars(corpus, field='party') <- middata$party
    docvars(corpus, field='state') <- middata$state
    docvars(corpus, field='ndate') <- as.integer(middata$ndate)
    docvars(corpus, field='electionyear') <- middata$electionyear
    docvars(corpus, field='trump.binary') <- middata$trump.binary
    docvars(corpus, field='trump.dates') <- middata$trump.dates
    docvars(corpus, field='percentage') <- middata$percentage
    docvars(corpus, field='pre16') <- middata$pre16
    docvars(corpus, field='pre12') <- middata$pre12
    docvars(corpus, field='region') <- middata$region
    docvars(corpus, field='name') <- middata$name
    ## convert dfm
    dfm <- dfm(dfm(corpus, tolower=FALSE, stem=FALSE))
    h_stmdfm <- convert(dfm, to = "stm", docvars = docvars(corpus))
    
    ## Savinf meta middata
    h_out <- prepDocuments(h_stmdfm$documents, h_stmdfm$vocab, h_stmdfm$meta, 
                           lower.thresh = 5)
    return(h_out)
}


which.median = function(x) {
  if (length(x) %% 2 != 0) {
    which(x == median(x))
  } else if (length(x) %% 2 == 0) {
    a = sort(x)[c(length(x)/2, length(x)/2+1)]
    c(which(x == a[1]), which(x == a[2]))
  }
}

## example
## h_out <- preproc(house)
## h_docs <- h_out$documents
## h_vocab <- h_out$vocab
## h_meta <- h_out$meta




## find the title of the text
## model = housePrevFit
## texts = shortdoc
## n = 10
## topics = 25
## thresh = NULL; where = NULL; meta = NULL

findText <- 
function (model, texts = NULL, topics = NULL, n = 3, thresh = NULL, 
    where = NULL, meta = NULL) 
{
    theta <- model$theta
    if (is.null(topics)) 
        topics <- 1:ncol(theta)
    if (!is.null(texts) && length(texts) != nrow(theta)) 
        stop("Number of provided texts and number of documents modeled do not match")
    if (!is.null(texts) && class(texts) == "list" && class(texts[[1]]) == 
        "matrix") 
        stop("It looks like you are trying to pass the numeric documents object. \n The texts argument wants a vector of characters that contain the actual text of the documents.")
    if (n > nrow(theta)) 
        n <- nrow(theta)
    if (n < 1) 
        stop("Must request at least one returned document.")
    out <- list()
    where <- substitute(where)
    if (!is.null(where) & !is.null(thresh)) 
        warning("Threshold value is ignored when where argument is non null \n Include threshold explicitly in where statement.")
    if (is.null(thresh)) 
        thresh <- 0
    if (!is.null(where)) {
        colnames(theta) <- sprintf("Topic%i", 1:ncol(theta))
        if (is.null(meta)) {
            dt <- data.table::data.table(docnum = 1:nrow(theta), 
                theta)
        }
        else {
            dt <- data.table::data.table(docnum = 1:nrow(theta), 
                theta, meta)
        }
        for (i in 1:length(topics)) {
            what <- parse(text = sprintf("docnum[order(Topic%i, decreasing=TRUE)][1:%i]", 
                topics[i], n))
            index <- dt[eval(where), eval(what)]
            out$index[[i]] <- index
            if (!is.null(texts)) 
                out$docs[[i]] <- texts[index]
        }
    }
    else {
        for (i in 1:length(topics)) {
            k <- topics[i]
            index <- order(theta[, k], decreasing = TRUE)[1:n]
            val <- sort(theta[, k], decreasing = TRUE)[1:n]
            index <- index[which(val >= thresh)]
            out$index[[i]] <- index
            if (!is.null(texts)) 
                out$docs[[i]] <- texts[index]
        }
    }
    names(out$index) <- paste("Topic", topics)
    if (!is.null(texts)) 
        names(out$docs) <- paste("Topic", topics)
    class(out) <- "findThoughts"
    if (is.null(texts)) 
        return(out)
    out$docs <- lapply(out$docs, unlist)
    if (is.factor(texts)) {
        warning("texts are of type 'factor.'  Converting to character vectors.  Use 'as.character' to avoid this warning in the future.")
        out$docs <- lapply(out$docs, as.character)
    }
    outobj <- list(out$docs, val)
    names(outobj) <- c("title", "theta")
    return(outobj)
}

