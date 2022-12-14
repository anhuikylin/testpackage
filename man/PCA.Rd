% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PCA.R
\name{PCA}
\alias{PCA}
\title{pca analysis plot}
\usage{
PCA(data, group, pca_digits = 2)
}
\arguments{
\item{data}{View examples}

\item{group}{View examples}

\item{pca_digits}{decimal digits}
}
\value{
pca_data
}
\description{
pca plot
}
\details{
Title
}
\examples{
data <- data.frame(matrix(sample(1:100000, 150000, replace = TRUE),ncol=15))
colnames(data)=paste0(rep(LETTERS[1:5],each=3),rep(1:3,5))
rownames(data)=paste0('gene',1:10000)
data <- data.frame(data)
group <- data.frame('Group'=c(rep(LETTERS[1:5],each=3)),'samplenames'=colnames(data))
PCA(data,group,pca_digits = 2)
}
\author{
Fei Liang
}
