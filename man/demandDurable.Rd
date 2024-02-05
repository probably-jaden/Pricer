\name{demandDurable}
\alias{demandDurable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
demandDurable
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~

Calculates the quantity demanded for durable goods based on the provided data containing 'wtp' (willingness to pay) information.

}
\usage{
demandDurable(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data frame (needs to be in a tibble format) with 'wtp' or `WTP` column representing the willingness to pay for durable goods.
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns the input data with an additional 'quantity' and 'revenue' columns. 'quantity' indicates the calculated quantity demanded for durable goods from the sampled customers at that willingness to pay level. 'revenue' indicates the revenue generated from the sampled customers at that willingness to pay level.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
