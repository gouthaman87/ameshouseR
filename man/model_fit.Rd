% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_workflow.R
\name{model_fit}
\alias{model_fit}
\title{Fit the workflow set}
\usage{
model_fit(DF, type_of_resample = as.character(), model_set)
}
\arguments{
\item{DF}{The training Data Frame}

\item{type_of_resample}{The type of resample Cross-Validation/Validation/Bootstrap}

\item{model_set}{The workflow set which is output of \code{model_workflow}}
}
\value{
Workflow Set with fit column
}
\description{
Fit the workflow set
}
