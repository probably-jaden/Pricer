\name{demandSummary}
\alias{demandSummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
demandSummary
}
\description{
Generates a statistical summary of the demand model fitted to the provided data with 'wtp' and 'quantity' information.
}
\usage{
demandSummary(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Data frame (must be a tibble) with 'wtp' column representing the willingness to pay and 'quantity' column indicating the corresponding quantity.
}
  \item{type}{
  Model type to fit, choose from "Linear," "Exponential," "Log," "Power," or "Sigmoid."

}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a summary of the fitted demand model (see the 'summary()' and 'lm()' functions), including relevant statistics and coefficients, based on the specified type.
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
