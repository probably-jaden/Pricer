\name{demandModel}
\alias{demandModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
demandModel
}
\description{
Fits a linear regression model to represent demand to the provided data with 'wtp' and 'quantity' information. The model type can be specified as "Linear," "Exponential," "Log," "Power," or "Sigmoid" transformations.
}
\usage{
demandModel(x)
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
Returns the lm() model, (see `?lm()` for more details). The model shows the coefficients of the fitted demand model based on the specified type.
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
