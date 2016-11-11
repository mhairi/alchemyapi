# Define our own paste operator
`%+%` <- function(x, y) paste0(x, y)

#' Get keywords from Alchemy API
#'
#' @param url URL to extract the keywords from
#' @param api_key
#'
#' @return Dataframe with a row for each word, the corrisponding score
#' and the origional url
#' @export
#'
#' @examples\dontrun{
#' get_keywords('google.com', '1234')
#' }
get_keywords <- function(url, api_key){
  get_url <-
    'http://gateway-a.watsonplatform.net/calls/url/URLGetRankedKeywords?' %+%
    '&url=' %+% url %+%
    '&apikey=' %+% api_key %+%
    '&outputMode=json'

  response <- httr::GET(get_url)

  httr::stop_for_status(response)

  output <- httr::content(response, as = 'text', encoding = 'UTF-8')
  output <- jsonlite::fromJSON(output)

  df <- output$keywords

  df$relevance <- as.numeric(df$relevance)
  df$url       <- url

  return(df)
}

#' Get concepts from Alchemy API
#'
#' @param url URL to extract the concepts from
#' @param api_key
#'
#' @return Dataframe with a concept for each word, the corrisponding score
#' and the origional url
#' @export
#'
#' @examples\dontrun{
#' get_concepts('google.com', '1234')
#' }
get_concepts <- function(url, api_key){
  get_url <-
    'http://gateway-a.watsonplatform.net/calls/url/URLGetRankedConcepts?' %+%
    '&url=' %+% url %+%
    '&apikey=' %+% api_key  %+%
    '&outputMode=json'

  response <- httr::GET(get_url)

  httr::stop_for_status(response)

  output <- httr::content(response, as = 'text', encoding = 'UTF-8')
  output <- jsonlite::fromJSON(output)

  df <- output$concepts

  df$relevance <- as.numeric(df$relevance)
  df$url  <- url

  return(df)
}

#' Get raw text of a URL
#'
#' @param url URL to extract the text from
#' @param api_key
#'
#' @return One element vector with page's text
#' @export
#'
#' @examples\dontrun{
#' get_text('google.com', '1234')
#' }
#'
get_text <- function(url, api_key){
  get_url <-
    'http://gateway-a.watsonplatform.net/calls/url/URLGetText?' %+%
    '&url=' %+% url %+%
    '&apikey=' %+% api_key %+%
    '&outputMode=json'

  response <- httr::GET(get_url)

  httr::stop_for_status(response)

  output <- httr::content(response, as = 'text', encoding = 'UTF-8')
  output <- jsonlite::fromJSON(output)

  return(output$text)
}

#' Get emotions from Alchemy API
#'
#' @param url URL to extract the emotions from
#' @param api_key
#'
#' @return Dataframe with a row for each emotion, the score for that emotion
#' And the origional URL.
#' @export
#'
#' @examples\dontrun{
#' get_emotions('google.com', '1234')
#' }
get_emotions <- function(url, api_key){
  get_url <-
    'http://gateway-a.watsonplatform.net/calls/url/URLGetEmotion?' %+%
    '&url=' %+% url %+%
    '&apikey=' %+% api_key %+%
    '&outputMode=json'

  response <- httr::GET(get_url)

  httr::stop_for_status(response)

  output <- httr::content(response, as = 'text', encoding = 'UTF-8')
  output <- jsonlite::fromJSON(output)

  return_df <- output$docEmotions
  return_df <- as.data.frame(lapply(return_df, as.numeric))

  df <- tidyr::gather(return_df, emotion, score)
  df$url <- url

  return(df)
}


#' Get sentiment from Alchemy API
#'
#' @param url URL to extract sentiment from
#' @param api_key
#'
#' @return Dataframe with a row for each emotion, the score for that emotion
#' And the origional URL.
#' @export
#'
#' @examples\dontrun{
#' get_sentiment('google.com', '1234')
#' }
get_sentiment <- function(url, api_key){
  get_url <-
    'http://gateway-a.watsonplatform.net/calls/url/URLGetTextSentiment?' %+%
    '&url=' %+% url %+%
    '&apikey=' %+% api_key %+%
    '&outputMode=json'

  response <- httr::GET(get_url)

  httr::stop_for_status(response)

  output <- httr::content(response, as = 'text', encoding = 'UTF-8')
  output <- jsonlite::fromJSON(output)

  return(as.numeric(output$docSentiment$score))
}

