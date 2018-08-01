\name{Write_binary_file}
\alias{Write_binary_file}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Write a binary file from cell/variable matrix used as input by \link{Compute_similarities}, \link{RunBhtSNE} and \link{RunNetSNE}.
}
\description{
Write a binary file.\cr Input: cell/variable matrix. \cr Output: binary file (.dat) with nrows (integer), ncols (integer) and row by row data (double).
}
\usage{
Write_binary_file(matrix, path.to.bin.file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{matrix}{
Matrix; Where rows are individuals/cells and columns are genes/variables/features
}
  \item{path.to.bin.file}{
Character: The path to save the output binary file (see Details)
}
}
\details{
If the \emph{matrix} is sparce, it should work (at least with "dgCMatrix").\cr The path to the save the output file (\emph{path.to.bin.file}) must be of the form "Documents/MyProject/Matrix.dat". "Documents/MyProject" \bold{must exist}, while "Matrix.dat" is the name of the file to be created and thefore should \bold{not exist}.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
function (matrix, path.to.bin.file)
{
    connection = file(path.to.bin.file, "wb")
    rownames(matrix) = NULL
    colnames(matrix) = NULL
    writeBin(as.integer(dim(matrix)), connection)
    for (i in 1:nrow(matrix)) {
        writeBin(as.double(matrix[i, ]), connection)
    }
    close(connection)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
