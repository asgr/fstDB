\name{limits_fstDB}
\alias{limits_fstDB}
\alias{check_limits_fstDB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find fstDB Limits
}
\description{
Functions to find fstDB limits, and to check them against a given selection.
}
\usage{
limits_fstDB(fstDB, columns = NULL, cores = length(fstDB), na.rm = TRUE)

check_limits_fstDB(limits_list, limits_fstDB)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fstDB}{
Object of class 'fstDB', i.e. as read in with \code{\link{read_fstDB}}.
}
  \item{columns}{
Character vector; which columns to analyse. If NULL then all columns are scanned (which might take a long time for big databases).
}
  \item{cores}{
Integer scalar; the number of cores to run on.
}
  \item{na.rm}{
Logical; should rm be ignored?
}
  \item{limits_list}{
A list of named limits to consider. Each list item should be a vector of length 2 (min and max limits to consider). Note limits are inclusive so if an upper limit reaches the lower limit of a subset then it will be included in the output that can be passed into \option{limit_sub} of \code{\link{[.fstDB}}.
}
  \item{limits_fstDB}{
Object of class 'limits_fstDB', i.e. the output of \code{limits_fstDB}.
}
}
\details{
If potentially making a lot of similar selections of the same columns on a fstDB, creating a 'limits_fstDB' object with  \code{limits_fstDB} and operating on it with \code{check_limits_fstDB} can hugely speed up repeated operations. If this process is a one off, then it is better to just operate on the fstDB directly.
}
\value{
\code{limits_fstDB} a named list of matrices that contain the lower (min) and upper limit (max) of each fstDB subset (ordered into rows).

\code{check_limits_fstDB} integer vector of fstDB subsets that overlap the specified selection ranges. This output can be passed into the \option{limit_sub} of \code{\link{[.fstDB}}, which can hugely speed up fstDB searches.
}
\author{
Aaron Robotham
}

\seealso{
\code{\link{read_fstDB}}, \code{\link{write_fstDB}}
}
\examples{
library(data.table)

temp_DT = data.table(num=1:26, let=letters[1:26])

fstDB = tempfile()
dir.create(fstDB)
on.exit(unlink(fstDB))

write_fstDB(temp_DT, name=fstDB, breakby='num', breaks=4)

temp_fstDB = read_fstDB(fstDB)

temp_limits = limits_fstDB(temp_fstDB, cores=2) #CRAN allows 2 cores max

print(temp_limits)

limit_sub = check_limits_fstDB(list(num=c(3,18), let=c('j','w')), temp_limits)

print(limit_sub)

#these should all then be the same

temp_DT[num >= 3 & num <= 18 & let >= 'j' & let <= 'w',]
temp_fstDB["num >= 3 & num <= 18 & let >= \'j\' & let <= \'w\'", cores=2]
temp_fstDB["num >= 3 & num <= 18 & let >= \'j\' & let <= \'w\'", limit_sub=limit_sub,
  cores=2]
}

