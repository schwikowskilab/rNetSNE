\name{RunNetSNE}
\alias{RunNetSNE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Run Net-SNE.
}
\description{
Run Net-SNE in three possible modes: "t-SNE for a t-SNE-like embedding, "Learn" for a training Net-SNE on an embedding and "Project" to project a new dataset on a learnt embedding (with "Learn").\cr Input: Depends on the mode.\cr Output: Different text (.txt) files depending on the mode saved in a single folder.\cr \bold{Requirment:} C++ implementation of Net-SNE installed and functional.
}
\usage{
RunNetSNE(utilis = c("t-SNE", "Learn", "Project"), out.dims = 2L,
          max.iter = 1000L, theta = 0.5, step.method = "adam",
          NN.layers = 2L, NN.units = 50L, NN.function = "relu", l2.reg.param = 0,
          mom.init = 0.5, mom.final = 0.8, mom.switch.iter = 250L,
          early.exag.iter = 250L, learn.rate = 0.02,
          local.sample = 20L, batch.frac = 0.1, min.sample.Z = 0.1,
          sgd = TRUE, seed = -1, verbose = TRUE,
          permute.after.iters = NULL, save.iters.cache = NULL,
          path.netSNE.dir = path.netSNE.dir,
          path.to.bin.train.file = path.to.bin.file, path.to.bin.test.file = NULL,
          path.to.simil.file = NULL, path.output.dir, path.ref.embedding = NULL,
          path.to.model.dir = NULL, model.prefix = "model_final")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{utilis}{
Character; Vector to choose the use of Net-SNE (default: c("t-SNE", "Learn", "Project") for running the three)
}
  \item{out.dims}{
Integer; Specifies the output dimensionality (default: 2)
}
  \item{max.iter}{
Integer; Specifies the number of iterations (default: 1e3)
}
  \item{theta}{
Double; Bounded by 0 and 1, controls the accuracy-efficiency tradeoff in SPTree for gradient computation; 0 means exact (default: 0.5)
}
  \item{step.method}{
Character; Specifies the gradient step schedule. Possible values: 'adam', 'mom' (momentum), 'mom_gain' (momentum with gains) or 'fixed' (default: adam)
}
  \item{NN.layers}{
Integer: Number of layers in the Neural Network (default: 2)
}
  \item{NN.units}{
Integer: Number of units for each layer in the Neural Network (default: 50)
}
  \item{NN.function}{
Character: Specifies the activation function of the Neural Network. Possible values: 'sigmoid' or 'relu' (default: relu)
}
  \item{l2.reg.param}{
Numerical; L2 regularization parameter for introducing sparcity in the Neural Network and avoid train data overfitting  (default: 0, i.e. no sparcity)
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
Double; Learning rate used for gradient steps (default: 0.02)
}
  \item{local.sample}{
Integer: Number of local samples for each data point in the mini-batch (default: 20)
}
  \item{batch.frac}{
Integer: Fraction of data to sample for mini-batch (default: 0.1, i.e. 10\%)
}
  \item{min.sample.Z}{
Double; Minimum fraction of data to use for approximating the normalization factor Z in the gradient (default: 0.1, i.e. 10\%)
}
  \item{sgd}{
Logical; Set to TRUE to use SGD acceleration. If set to FALSE (effective for small datasets), equivalent to t-SNE with an additional backpropagation step to train a neural network (default: TRUE)
}
  \item{seed}{
Integer; Equivalent to \link[=Random]{set.seed} (default: -1, i.e. use current time as seed)
}
  \item{verbose}{
Logical; Should the outputs be printed to the console? (default: TRUE)
}
  \item{permute.after.iters}{
Integer; Number of iterations after which the ordering of data points is repeatidly permuted for fast mini-batching (default: NULL, i.e. permute.after.iters = \emph{max.iter})
}
  \item{save.iters.cache}{
Integer; Number of iterations after which an intermediary embedding is repeatidly recorded. The final embedding (Y_final.txt) is always saved (default: NULL, i.e. no intermediary embedding is recorded, only the definitive one after \emph{max.iter})
}
  \item{path.netSNE.dir}{
Character; The path to the directory containing the executables created after Net-SNE installation (usually : path/to/netsne-master/bin). \bold{Must end with the name of directory with exectuables} (here: '/bin')
}
  \item{path.to.bin.train.file}{
Character; The path to the binary file of the train data matrix obtained with \link{Write_binary_file} (same as parameter \emph{path.to.bin.file} in \link{RunBhtSNE})
}
  \item{path.to.bin.test.file}{
Character; The path to the binary file of the test data matrix obtained with \link{Write_binary_file}
}
  \item{path.to.simil.file}{
Character; The path to the binary file obtained with \link{Compute_similarities}
}
  \item{path.output.dir}{
Character; The path to save to output directory containing the .txt files
}
  \item{path.ref.embedding}{
Character; The path to the Y_final.txt file produced by \link{RunBhtSNE} (or RunNetSNE in 't-SNE' mode) used as a reference embedding to learn
}
  \item{path.to.model.dir}{
Character; The path to the directory obtained with RunNetSNE in 'Learn' mode, containing 'model_final*.txt' files used to project new data.
}
  \item{model.prefix}{
Charatcer; Prefix of the text files used as model for RunNetSNE 'Project' mode (default: 'model_final')
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
function (utilis = c("t-SNE", "Learn", "Project"), out.dims = 2L,
    max.iter = 1000L, theta = 0.5, step.method = "adam", NN.layers = 2L,
    NN.units = 50L, NN.function = "relu", mom.init = 0.5, mom.final = 0.8,
    mom.switch.iter = 250L, early.exag.iter = 250L, learn.rate = 0.02,
    local.sample = 20L, min.sample.Z = 0.1, l2.reg.param = 0,
    sgd = TRUE, seed = -1, verbose = TRUE, batch.frac = 0.1,
    random.init = TRUE, permute.after.iters = NULL, save.iters.cache = NULL,
    path.netSNE.dir = path.netSNE.dir, path.to.bin.train.file = path.to.bin.file,
    path.to.bin.test.file = NULL, path.to.simil.file = NULL,
    path.output.dir, path.ref.embedding = NULL, path.to.model.dir = NULL,
    model.prefix = "model_final")
{
    utilis = tolower(utilis)
    for (i in 1:length(utilis)) {
        if (!utilis[i] \%in\% c("t-sne", "tsne", "learn", "train",
            "project", "proj", "projection")) {
            stop(paste(utilis[i], ": Unknown use of Net-SNE (param: utilis).\nPossible values:\n\t- 't-SNE': t-SNE like embedding\n\t- 'Learn': train Net-SNE on a training dataset to latter project new data on the embedding\n\t- 'Project': project a new dataset on a prior embedding (obtain with utilis = Train)"),
                call. = FALSE)
        }
    }
    if (step.method != "adam" & step.method != "mom" & step.method !=
        "mom_gain" & step.method != "fixed") {
        stop(paste(step.method, ": Unknown gradient step schedule (param: step.method).\nPossible values:'adam', 'mom' (momentum), 'mom_gain' (momentum with gains) or 'fixed'"),
            call. = FALSE)
    }
    if (NN.function != "relu" & NN.function != "sigmoid") {
        stop(paste(NN.function, ": Unknown activation function of the neural network (param: NN.function).\nPossible values:'relu'or 'sigmoid'"),
            call. = FALSE)
    }
    path.netSNE.dir = paste(path.netSNE.dir, "RunNetsne", sep = "/")
    if ("t-sne" \%in\% utilis | "tsne" \%in\% utilis) {
        command = paste(path.netSNE.dir, "--input-P", path.to.simil.file,
            "--input-X", path.to.bin.train.file, "--out-dir",
            path.output.dir, sep = " ")
        command = paste(command, "--out-dim", out.dims, "--max-iter",
            max.iter, "--rand-seed", seed, "--theta", theta,
            "--learn-rate", learn.rate, "--mom-init", mom.init,
            "--mom-final", mom.final, "--mom-switch-iter", mom.switch.iter,
            "--early-exag-iter", early.exag.iter, "--num-local-sample",
            local.sample, "--batch-frac", batch.frac, "--min-sample-Z",
            min.sample.Z, "--l2-reg", l2.reg.param, "--step-method",
            step.method, "--num-layers", NN.layers, "--num-units",
            NN.units, "--act-fn", NN.function, sep = " ")
        if (!random.init) {
            command = paste(command, "--skip-random-init", sep = " ")
        }
        if (!is.null(permute.after.iters)) {
            command = paste(command, "--perm-iter", permute.after.iters,
                sep = " ")
        }
        if (!is.null(save.iters.cache)) {
            command = paste(command, "--cache-iter", save.iters.cache,
                sep = " ")
        }
        if (!sgd) {
            command = paste(command, "--no-sgd", sep = " ")
        }
        if (!verbose) {
            command = paste(command, "> /dev/null", sep = " ")
        }
        if ("crayon" \%in\% installed.packages()[, 1]) {
            cat(crayon::bold(crayon::red("\n\nRunning NetSNE !  (t-SNE like)\n\n")))
        }
        else {
            cat("\n\nRunning NetSNE !  (t-SNE like)\n\n")
        }
        system(command)
    }
    if ("learn" \%in\% utilis | "train" \%in\% utilis) {
        command = paste(path.netSNE.dir, "--input-Y", path.ref.embedding,
            "--input-X", path.to.bin.train.file, "--out-dir",
            path.output.dir, sep = " ")
        command = paste(command, "--out-dim", out.dims, "--max-iter",
            max.iter, "--rand-seed", seed, "--theta", theta,
            "--learn-rate", learn.rate, "--mom-init", mom.init,
            "--mom-final", mom.final, "--mom-switch-iter", mom.switch.iter,
            "--early-exag-iter", early.exag.iter, "--num-local-sample",
            local.sample, "--batch-frac", batch.frac, "--min-sample-Z",
            min.sample.Z, "--l2-reg", l2.reg.param, "--step-method",
            step.method, "--num-layers", NN.layers, "--num-units",
            NN.units, "--act-fn", NN.function, sep = " ")
        if (!random.init) {
            command = paste(command, "--skip-random-init", sep = " ")
        }
        if (!is.null(permute.after.iters)) {
            command = paste(command, "--perm-iter", permute.after.iters,
                sep = " ")
        }
        if (!is.null(save.iters.cache)) {
            command = paste(command, "--cache-iter", save.iters.cache,
                sep = " ")
        }
        if (!sgd) {
            command = paste(command, "--no-sgd", sep = " ")
        }
        if (!verbose) {
            command = paste(command, "> /dev/null", sep = " ")
        }
        if ("crayon" \%in\% installed.packages()[, 1]) {
            cat(crayon::bold(crayon::red("\n\nRunning NetSNE !  (learnining embedding)\n\n")))
        }
        else {
            cat("\n\nRunning NetSNE !  (learnining embedding)\n\n")
        }
        system(command)
    }
    if ("projection" \%in\% utilis | "project" \%in\% utilis | "proj" \%in\%
        utilis) {
        command = paste(path.netSNE.dir, "--input-X", path.to.bin.test.file,
            "--init-model-prefix", paste(path.to.model.dir, model.prefix,
                sep = "/"), "--test-model", "--no-target", "--out-dir",
            path.output.dir, sep = " ")
        if (!verbose) {
            command = paste(command, "> /dev/null", sep = " ")
        }
        if ("crayon" \%in\% installed.packages()[, 1]) {
            cat(crayon::bold(crayon::red("\n\nRunning NetSNE !  (projection)\n\n")))
        }
        else {
            cat("\n\nRunning NetSNE !  (projection)\n\n")
        }
        system(command)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
