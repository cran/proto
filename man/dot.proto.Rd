\name{dot.proto}
\alias{dot.proto}
\alias{name.proto}
\title{Proto graphviz interface}
\description{
\code{dot.proto} creates a GraphViz \code{.dot} file showing the relationships
among proto objects.
oriented system.
}
\usage{
dot.proto(e = if (exists(".that")) .that else parent.frame(), 
     file = "", control)
}
\arguments{
  \item{e}{an environment or proto object to look in for the proto objects.  
    Defaults to current environment.}
  \item{file}{file where \code{dot} ouput is placed.  Defaults to standard 
    output.}
  \item{control}{a list of parameters .that affect the grahical display.  
    It may be omitted.  If specified, only specify those components for
    which the default is not desired. The components are \code{include}
    and \code{arrow.from.child}.  \code{include} is a string of
    GraphViz dot commands .that are included in the output.
    \code{arrow.from.child} causes arrows to point from children to 
    parents if \code{TRUE} and otherwise from parents to children.}
}


\details{
\code{proto} creates a GraphViz \code{.dot} file which can be
transformed to postscript or other displayable graphics format
using the \code{GraphViz} \code{dot} command to graphically
display the relationship among proto objects.  Proto objects are
identified by the contents of their \code{..Name} component, if any,
or else by their name, if available, or else by the hash value of 
their environment.  If there are mulitple variables holding the
same proto or environment the results are undefined.
}

\value{
No value is returned.
}

\note{
To create PDF files on Windows from a \code{.dot} file created
by \code{dot.proto} issue the following command at the Windows console
\code{dot -Tps2 myfile.dot -o myfile.ps} and then use a postscript to
PDF translator.  For example, with Ghostscript the command 
\code{ps2pdf myfile.ps myfile.pdf} can be used where the ps2pdf
command can be found at \code{/gs/.../lib/ps2pdf.bat} and 
\code{...} will depend on the version of ghostscript.

This function may be revised to use \code{GraphViz} via 
the \code{RGraphViz} package if and when
that package is ported to all common operating systems.
}

\references{
  \url{http://www.graphviz.org}
  \url{http://www.ghostscript.com}
}


\seealso{\code{\link{proto}}}

\examples{
a <- proto( ..Name = "a" )
b <- a$proto( ..Name = "b" )
dot.proto()
dot.proto(control = list(include = "") )  # different appearance
# or 
#   dot.proto(file = "example.dot")
# At the operating system command line:
#   dot -Tps example.dot -o example.ps
# or
#   dot -Tjpg example.dot -o example.jpg
}
\keyword{programming}

