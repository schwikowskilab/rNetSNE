\name{Compute_similarities}
\alias{Compute_similarities}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute similarities for Net-SNE or BhtSNE.
}
\description{
Compute similarities for Net-SNE or BhtSNE.\cr Input : cell/variable matrix saved as binary file (.dat) with \link{Write_binary_file}.\cr Output : binary file (.dat) used for \link{RunBhtSNE} or \link{RunNetSNE}.\cr \bold{Requirment:} C++ implementation of Net-SNE installed and functional.
}
\usage{
Compute_similarities(perp = 30L, verbose = TRUE,
                     path.netSNE.dir = path.netSNE.dir,
                     path.to.bin.file = path.to.bin.file,
                     path.output.file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{perp}{
Integer; Stands for 'Perplexity', an integer between 0 and 100 (\link[=Rtsne]{see t-SNE documentation}, default: 30)
}
  \item{verbose}{
Logical; Should the outputs be printed to the console? (default: TRUE)
}
  \item{path.netSNE.dir}{
Character; The path to the directory containing the executables created after Net-SNE installation (usually : path/to/netsne-master/bin). \bold{Must end with the name of directory with exectuables} (here: '/bin')
}
  \item{path.to.bin.file}{
Character; The path to the binary file of the train data matrix obtained with \link{Write_binary_file}
}
  \item{path.output.file}{
Character; The path to save the output file (see Details)
}
}
\details{
The path to the Net-SNE directory (\emph{path.netSNE.dir}) must end with the \strong{directory} containing the executables (ComputeP in this case). Therefore, it should end with "bin" (\strong{no slash after bin}).

The path to the save the output directory (\emph{path.output.dir}) must be of the form "Documents/MyProject/SimFile.dat". "Documents/MyProject" \bold{must exist}, while "SimFile.dat" is the name of the file to be created and thefore should \bold{not exist}.
}
\value{
The function does not return anything but creates a binary file to the path specified with \emph{path.output.dir}.
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

data(iris)
labels = iris[,5]
iris = iris[,1:4]

path.netSNE.dir = "~/netsne-master/bin"
path.output
On verra plus tard...
}
