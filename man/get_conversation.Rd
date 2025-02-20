% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_conversation.R
\name{get_conversation}
\alias{get_conversation}
\title{Get complete conversation}
\usage{
get_conversation(conversation_ids, ...)
}
\arguments{
\item{conversation_ids}{string conversation_id or vector of conversation ids}

\item{...}{arguments will be passed to \code{\link[=build_query]{build_query()}} function. See \code{?build_query()} for further information.}
}
\value{
a data frame
}
\description{
This function fetches all tweets that form part of a conversation
}
