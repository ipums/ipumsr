% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lbl_helpers.R
\name{lbl}
\alias{lbl}
\title{Make a label placeholder object}
\usage{
lbl(...)
}
\arguments{
\item{...}{Either one or two arguments specifying the label (\code{.lbl}) and
value (\code{.val}) to use in the new label pair.

If arguments are named, they must be named \code{.val} and/or \code{.lbl}.

If a single unnamed value is passed, it is used as the \code{.lbl} for the new
label. If two unnamed values are passed, they are used as the \code{.val} and
\code{.lbl}, respectively.}
}
\value{
A \code{label_placeholder} object
}
\description{
Define a new label/value pair. For use in functions like \code{\link[=lbl_relabel]{lbl_relabel()}}
and \code{\link[=lbl_add]{lbl_add()}}.
}
\details{
Several \verb{lbl_*()} functions include arguments that can be passed a function
of \code{.val} and/or \code{.lbl}. These refer to the existing values and
labels in the input vector, respectively.

Use \code{.val} to refer to the \emph{values} in the vector's value labels.
Use \code{.lbl} to refer to the \emph{label names} in the vector's value labels.

Note that not all \verb{lbl_*()} functions support both of these arguments.
}
\examples{
# Label placeholder with no associated value
lbl("New label")

# Label placeholder with a value/label pair
lbl(10, "New label")

# Use placeholders as inputs to other label handlers
x <- haven::labelled(
  c(100, 200, 105, 990, 999, 230),
  c(`Unknown` = 990, NIU = 999)
)

x <- lbl_add(
  x,
  lbl(100, "$100"),
  lbl(105, "$105"),
  lbl(200, "$200"),
  lbl(230, "$230")
)

lbl_relabel(x, lbl(9999, "Missing") ~ .val > 900)
}
\seealso{
Other lbl_helpers: 
\code{\link{lbl_add}()},
\code{\link{lbl_clean}()},
\code{\link{lbl_define}()},
\code{\link{lbl_na_if}()},
\code{\link{lbl_relabel}()},
\code{\link{zap_ipums_attributes}()}
}
\concept{lbl_helpers}
