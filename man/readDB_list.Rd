\name{read_fstDB}
\alias{read_fstDB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read in an FST List
}
\description{
Use a combination of target fst files as a mini on-disk database to later operate on.
}
\usage{
read_fstDB(dirlist = ".", filelist = NULL, pattern = "^.*sub.*\\\.fst$", recursive = TRUE,
  combine = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dirlist}{
Character vector; vector of full paths of directories to treat as containing fst files fragments.
}
  \item{filelist}{
Character vector; vector of full paths of fst files to treat as containing fst files fragments.
}
  \item{pattern}{
Scalar character; regular expression to filter \option{filelist} by. The default limits to files ending ".fst", which is usually what you want. Most people find it easier to work via \code{\link{glob2rx}} since then you can specify file wild cards in the usual shell way, e.g. glob2rx("*data*fst") becomes "^.*data.*fst"
}
  \item{recursive}{
Logical; if using \option{dirlist} should all sub-directories be checked recursively?
}
  \item{combine}{
Logical; should the targetted fst files be immediately combined on read in, creating a \code{data.table} output rather than \code{fstDB} object.
}
}
\details{
This function is usually used in combination with \code{fstDB} methods to efficiently operate on large volumes of data held in an on-disk mini database. The real strength of this method is the \code{fst} backend, which is a threaded and highly optimised method to work on random access on disk files.
}
\value{
In the default mode of \option{combine} = FALSE the output is a \code{fstDB} object which has an associated '[' subset method to allow data.table like operations.

\option{combine} = TRUE then the output will be a fully joined \code{data.table}.
}
\author{
Aaron Robotham
}
\seealso{
\code{\link{write_fstDB}}, \code{\link{fstDB_methods}}
}
\examples{
\dontrun{
temp = list(data.table(num=1:13, let=letters[1:13]),
            data.table(num=14:26, let=letters[14:26]))

write_fstDB(temp, name='~/Documents/test_fstDB')

temp2 = read_fstDB('~/Documents/test_fstDB')

print(temp2)

#you can do data.table like logic on i (but must be character of an expression):

print(temp2['num < 14',])

#We can write out a data.table using a target column to organise the files:

tempDT = temp2[,] #this loads all the fstDB into a single data.table

write_fstDB(tempDT, name='~/Documents/test_fstDB2', breakby='num', breaks=4)

temp3 = read_fstDB('~/Documents/test_fstDB2')

print(temp3)

print(temp3['num < 14',]) #this should be the same, but operating on 4 files (not 2):
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
