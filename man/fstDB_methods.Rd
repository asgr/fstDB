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
\method{[}{fstDB}(x, i, j, j_logic = 'auto', limit_sub = NULL, cores = length(x),
  log_sub = TRUE)
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
Character/Integer vector; the column names or locations to extract. If using logic on \option{i} the columns listed in \option{j} must include all inputs required.
}
  \item{j_logic}{
Character/Integer vector; which columns to select for the row logic \option{i}. The default of 'auto' matches the available colnames against the logical expression requested and tries to find all columns that might be needed (this usually works well). Otherwise you can specify the exact column names to select out (probably only worth doing if there is a problem with the 'auto' selection). Setting to NULL means it will only use the columns selected in \option{j}. If these certainly contain the logic columns needed then this should be find (and in fact faster), but it requires a bit more care from the user (hence not the default).
}
  \item{limit_sub}{
Integer vector; users can specify which elements of \option{x} to search over. If you happen to know that the data was broken up by a certain column and therefore know which fstDM elements containing the search you are after, setting this can substantially speed things up. Checking the output from a previous \option{.log_sub} attribute can help determine this. See Examples.
}
  \item{cores}{
Integer scalar; the number of cores to run on. The default is the number elements in the \option{x} fstDB. Internally the number of cores is capped to the number available on the machine. Also, it will be capped to the number elements in the \option{x} fstDB (if you explicitly request more).
}
  \item{log_sub}{
Logical; should an attribute be added to the output specifying the origin of the row (i.e. which fstDB element it came from)? This might be useful for setting \option{limit_sub} in future searches. See Examples.
}
}
\details{
These methods offer efficient and intuitive ways to interact with a target \code{fstDB} mini on-disk database.
}
\value{
\code{[.fstDB} the combined \code{data.table}.

\code{dim.fstDB} integer dimension vector (N row, N col).
}
\author{
Aaron Robotham
}

\seealso{
\code{\link{read_fstDB}}, \code{\link{write_fstDB}}
}
\examples{
library(data.table)

temp = list(data.table(num=1:13, let=letters[1:13]),
            data.table(num=14:26, let=letters[14:26]))

fstDB = tempfile()
dir.create(fstDB)
on.exit(unlink(fstDB))

write_fstDB(temp, name=fstDB)

temp2 = read_fstDB(fstDB)

print(temp2)

#you can do data.table like logic on i (but must be character of an expression):

print(temp2['num < 14',])

#We can write out a data.table using a target column to organise the files:

temp_DT = temp2[,] #this loads all the fstDB into a single data.table

fstDB2 = tempfile()
dir.create(fstDB2)
on.exit(unlink(fstDB2))

write_fstDB(temp_DT, name=fstDB2, breakby='num', breaks=4)

temp3 = read_fstDB(fstDB2)

print(temp3)

#Note we have to set cores=2 because that is the CRAN max allowed

print(temp3['num < 14',cores=2]) #should be the same, but operating on 4 files (not 2)

#We can check which files of the temp3 fstDB contained the selected rows:

attributes(temp3['num < 14',cores=2])$.log_sub #just 1 and 2

#knowing this we can search just those files:

print(temp3['num < 14',limit_sub=c(1,2),cores=2])

#if we specified files 1 and 3 we would miss some rows:

print(temp3['num < 14',limit_sub=c(1,3),cores=2])

#so users need to be careful if using this feature, but it can lead to large speed ups
}
