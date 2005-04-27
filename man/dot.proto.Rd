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
  \item{control}{a list of parameters that affects the grahical display.  
    It may be omitted.  The \code{include} component
    is a string of \code{dot} commands that are included prior to
    the generated \code{dot} output.
    The default value is  \code{"graph [rankdir=BT];"}.  The 
    \code{arrow.from.child} component causes arrows to point from children to 
    parents if \code{TRUE}, the default, and otherwise from parents to children.}
}


\details{
\code{proto} creates a \code{GraphViz} \code{.dot} file which can be
transformed to postscript or other displayable graphics format
outside of \code{R}
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

oo <- proto(..Name = "root")
oo2 <- oo$proto()
oo3 <- oo$proto()
oo4 <- oo$proto() 
dot.proto()

# different appearance
dot.proto(control = list(include = "") )

# just oo, oo2, oo3.  Display dot commands on console.
dot.proto(proto(oo = oo, oo2 = oo2, oo3 = oo3))   

# same since oo is parent of oo2 and oo3
dot.proto(proto(oo2 = oo2, oo3 = oo3))   


# In R, output dot commands to a file:
#   dot.proto(file = "example.dot")
# Assuming GraphViz is installed, at
# the operating system command line level:
#   dot -Tps example.dot -o example.ps
# or
#   dot -Tjpg example.dot -o example.jpg
}
\keyword{programming}

