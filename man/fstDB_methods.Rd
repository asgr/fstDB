\name{fstDB_methods}
\alias{fstDB_methods}
\alias{[.fstDB}
\alias{dim.fstDB}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Operators for fstDB Objects
}
\description{
Operators for fstDB Objects
}
\usage{
\method{[}{fstDB}(x, i, j)
\method{dim}{fstDB}(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class 'fstDB'.
}
  \item{i}{
Either integer elements to subset each fst file with; or (more usefully) a character scalar of expression containing the column subset operation to carry out in order to select rows we wish to keep. This should use \code{data.table} style syntax, where column names can be treated as variables. If using logic on \option{i} the columns listed in \option{j} must include all inputs required.
}
  \item{j}{
Character/Integer vector, the column names or locations to extract. If using logic on \option{i} the columns listed in \option{j} must include all inputs required.
}
}
\details{
These methods offer efficient and intuitive ways to interact with a target \code{fstDB} mini on-disk database.
}
\value{
\item{[.fstDB}{The combined \code{data.table}.}
\item{dim.fstDB}{Integer dimension vector (N row, N col).}
}
\author{
Aaron Robotham
}

\seealso{
\code{\link{read_fstDB}}, \code{\link{write_fstDB}}
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
