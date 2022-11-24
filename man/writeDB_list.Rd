\name{write_fstDB}
\alias{write_fstDB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Write out an FST List
}
\description{
Write out an \code{fstDB} or \code{data.table} / \code{data.frame} as a mini on-disk database.
}
\usage{
write_fstDB(x, name = "fstDB", sub = "sub", sub_col = ".list_position",
  compress = 50, uniform_encoding = TRUE, cores = 1, append = FALSE, breakby = NULL,
  breaks = 10, trim = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{fstDB}, a single \code{data.table} / \code{data.frame}, or a list with \code{data.table} / \code{data.frame} elements.
}
  \item{name}{
Character scalar; the name of the fstDB database. This will be the name of the directory that contains the multiple fst files making up the mini on-disk database.
}
  \item{sub}{
Character scalar; file name stub for the subset files contained inside the top level fstDB database directory.
}
  \item{sub_col}{
Character scalar; the column name that refers to the subsets to be created. Only relevant if \option{x} is \code{data.table} / \code{data.frame}. This column should contain integer values. If missing then the \code{link{attributes}} of \option{x} will be searched instead. The latter behaviour will work with files read in with \code{\link{read_fstDB}} since that function automatically creates an attribtues called ".list_position".
}
  \item{compress}{
Numeric scalar; value in the range 0 to 100, indicating the amount of compression to use. Lower values mean larger file sizes. The default compression is set to 50. See \code{\link{write_fst}}.
}
  \item{uniform_encoding}{
Logical; if 'TRUE', all character vectors will be assumed to have elements with equal encoding. The encoding (latin1, UTF8 or native) of the first non-NA element will used as encoding for the whole column. This will be a correct assumption for most use cases. If 'uniform.encoding' is set to 'FALSE', no such assumption will be made and all elements will be converted to the same encoding. The latter is a relatively expensive operation and will reduce write performance for character columns. See \code{\link{write_fst}}.
}
  \item{cores}{
Integer scalar; the number of cores to run on.
}
  \item{append}{
Logical; should the output be appended to an already started fstDB mini on-disk database. If TRUE then the largest current \option{sub}_X value is obtained, and new files are written with values above this.
}
  \item{breakby}{
Character/Integer scalar; if \option{x} is a \code{data.table} / \code{data.frame} then you can specify a target column to break the files up by.
}
  \item{breaks}{
Integer scalar/vector
}
  \item{trim}{
Logical; should data outside of \option{breaks} range be trimmed?
}
}
\details{
If \option{x} is already a \code{fstDB} object then this function is really a way to copy a data base, but of course additional elements can be appended to \option{x} to create a modified version of the data base.
}
\value{
The output is a directory containing the subset fst files that collectively form a fstDB mini on-disk database that can be read back in with \code{\link{read_fstDB}}.
}
\author{
Aaron Robotham
}
\seealso{
\code{\link{read_fstDB}}, \code{\link{fstDB_methods}}
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
