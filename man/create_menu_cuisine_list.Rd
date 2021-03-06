% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uberEx.r
\name{create_menu_cuisine_list}
\alias{create_menu_cuisine_list}
\title{create_menu_cuisine_list}
\usage{
create_menu_cuisine_list(cuisine, ndishes)
}
\arguments{
\item{cuisine}{a character vector of cuisine letter, e.g., c("A", "B", "C")}

\item{ndishes}{an integer vector of number of dishes within each cuisine,
must have the same length with cuisine, e.g., c(5L, 3L, 4L)}
}
\value{
menu data.table
}
\description{
create a menu with cuisine and number of dishes within each cuisine
}
\note{
create_menu_cuisine_list(cuisine = c("A", "B", "C"), ndishes = c(5L, 3L, 4L))
}

