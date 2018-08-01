\name{RunBhtSNE}
\alias{RunBhtSNE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Run a usual t-SNE.
}
\description{
Run a usual t-SNE.\cr Input: binary file (.dat) describing similarities from \link{Compute_similarities}.\cr Output : directory containing text files (.txt), including 'Y_final.txt' used for \link[graphics]{plot} or \link{RunNetSNE} ('learn' mode).\cr \bold{Requirment:} C++ implementation of Net-SNE installed and functional.

}
\usage{
RunBhtSNE(out.dims = 2L, max.iter = 1000L, theta = 0.5,
          mom.init = 0.5, mom.final = 0.8, mom.switch.iter = 250L,
          early.exag.iter = 250L, learn.rate = 200,
          batch.frac = NULL, seed = -1, verbose = TRUE, random.init = TRUE,
          save.iters.cache = FALSE, path.netSNE.dir = path.netSNE.dir,
          path.to.bin.file = path.to.bin.file,
          path.to.simil.file, path.output.dir)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{out.dims}{
Integer; Specifies the output dimensionality (default: 2)
}
  \item{max.iter}{
Integer; Specifies the number of iterations (default: 1e3)
}
  \item{theta}{
Double; Bounded by 0 and 1, controls the accuracy-efficiency tradeoff in SPTree for gradient computation; 0 means exact (default: 0.5)
}
  \item{mom.init}{
Double; Bounded by 0 and 1, defines the momentum used before n=\emph{mom.switch.iter} iterations (default: 0.5)
}
  \item{mom.final}{
Double; Bounded by 0 and 1, defines the momentum used after n=\emph{mom.switch.iter} iterations (default: 0.8)
}
  \item{mom.switch.iter}{
Integer; Number of iterations before switching the value of momentum (default: 250)
}
  \item{early.exag.iter}{
Integer; Number of iterations of early exaggeration (default: 250)
}
  \item{learn.rate}{
Double; Learning rate used for gradient steps (default: 200.0)
}
  \item{batch.frac}{
Integer: Number of points to update at each iteration (default: NULL, i.e. all of them)
}
  \item{seed}{
Integer; Equivalent to \link[=Random]{set.seed} (default: -1, i.e. use current time as seed)
}
  \item{verbose}{
Logical; Should the outputs be printed to the console? (default: TRUE)
}
  \item{random.init}{
Logical; Set to TRUE to allow random initialization (default: TRUE)
}
  \item{save.iters.cache}{
Integer; Number of iterations after which an intermediary embedding is repeatidly recorded. The final embedding (Y_final.txt) is always saved (default: NULL, i.e. no intermediary embedding is recorded, only the definitive one after \emph{max.iter})
}
  \item{path.netSNE.dir}{
Character; The path to the directory containing the executables created after Net-SNE installation (usually : path/to/netsne-master/bin). \bold{Must end with the name of directory with exectuables} (here: '/bin')
}
  \item{path.to.bin.file}{
Character; The path to the binary file of the train data matrix obtained with \link{Write_binary_file}
}
  \item{path.to.simil.file}{
Character; The path to the binary file containing the similarities of the train data matrix obtained with \link{Compute_similarities}
}
  \item{path.output.dir}{
Character; The path to save to output directory containing the .txt files
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
function (out.dims = 2, max.iter = 1000L, theta = 0.5, mom.init = 0.5,
    mom.final = 0.8, mom.switch.iter = 250L, early.exag.iter = 250,
    learn.rate = 200, batch.frac = NULL, seed = -1, verbose = TRUE,
    random.init = TRUE, save.iters.cache = FALSE, path.netSNE.dir = path.netSNE.dir,
    path.to.bin.file = path.to.bin.file, path.to.simil.file,
    path.output.dir)
{
    command = paste(path.netSNE.dir, "RunBhtsne", sep = "/")
    command = paste(command, path.netSNE.dir, "--input-P", path.to.simil.file,
        "--out-dir", path.output.dir, sep = " ")
    command = paste(command, "--out-dim", out.dims, "--max-iter",
        max.iter, "--rand-seed", seed, "--theta", theta, "--learn-rate",
        learn.rate, "--mom-init", mom.init, "--mom-final", mom.final,
        "--mom-switch-iter", mom.switch.iter, "--early-exag-iter",
        early.exag.iter, sep = " ")
    if (!random.init) {
        command = paste(command, "--skip-random-init", sep = " ")
    }
    if (!is.null(batch.frac)) {
        command = paste(command, "--batch-frac", batch.frac,
            sep = " ")
    }
    if (!is.null(save.iters.cache)) {
        command = paste(command, "--cache-iter", save.iters.cache,
            sep = " ")
    }
    if (!verbose) {
        command = paste(command, "> /dev/null", sep = " ")
    }
    if ("crayon" \%in\% installed.packages()[, 1]) {
        cat(crayon::bold(crayon::red("\n\nRunning BhtSNE !\n\n")))
    }
    else {
        cat("\n\nRunning BhtSNE !\n\n")
    }
    system(command)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
