\name{demandNonDurable}
\alias{demandNonDurable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
demandNonDurable
}
\description{
Estimates the quantity demanded for non-durable goods based on the provided data containing 'price' and 'quantityPerPerson' information.
}
\usage{
demandNonDurable(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Data frame (must be tibble) with 'price' column representing the price of non-durable goods and 'quantityPerPerson' column indicating the quantity desired per person at that price.
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
