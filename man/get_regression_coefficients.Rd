% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getter_functions_for_pv_obj.R
\name{get_regression_coefficients}
\alias{get_regression_coefficients}
\title{Get the regression coefficients estimated in the latent regression on the
background data}
\usage{
get_regression_coefficients(pv_obj)
}
\arguments{
\item{pv_obj}{return object of function \code{NEPSscaling::plausible_values()}}
}
\value{
the matrix containing the regression weight for each background
variable, including an intercept, as numeric values
}
\description{
Get the regression coefficients estimated in the latent regression on the
background data
}
