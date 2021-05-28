\name{visuArc}
\alias{visuArc}
\title{
Plots an arc diagram.
}
\description{
An arc diagram is graphical representation of a selected part of the VisuNet network.
}
\usage{
visuArch(net, df, icol, dec, mainTitle)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{
An output network from the VisuNet.
}
  \item{df}{
A data frame containing network connection in columns.
}
  \item{discrete}{
Integer. Defines which column from df shall be taken.
}
  \item{dec}{
A character indciating decision class to be presented.
}
  \item{mainTitle}{
A character indciating title for the arch diagram.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{heatmap}{An arc diagram presenting hub and all its connections dervied from rules.}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Mateusz Garbulowski
}
\note{
%%  ~~further notes~~
Function is supported with a package "arcdiagram".

Reference:
Sanchez, Gaston. "Introduction to the R package arcdiagram." (2016)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as 
\code{\link{https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.707.6275&rep=rep1&type=pdf}}
}
\examples{
library(VisuNet)
library(arcdiagram)

visuArc(vNet, df, 1, "cancer", "sample arc diagram")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
