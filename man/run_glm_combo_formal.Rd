% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{run_glm_combo_formal}
\alias{run_glm_combo_formal}
\title{Run binomial GLM for overall quality of formal consultations}
\usage{
run_glm_combo_formal(x)
}
\arguments{
\item{x}{The 'formal' data.frame of formal consultations}
}
\value{
A list of results including:
  \describe{
    \item{mods}{Nine GLM (binomial) model objects}
    \item{AICs}{AICc values for the nine models}
    \item{summaries}{Results from \code{summary(mod)} for the nine models}
    \item{aovs}{Analysis of Variance for the nine models}
  }
}
\description{
Run binomial GLM for overall quality of formal consultations
}
\examples{
\dontrun{
run_glm_combo(combo)
}
}
