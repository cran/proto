proto <- function (. = parent.env(envir), expr = {}, envir = 
		new.env(parent = parent.frame()), ... ) {
    parent.env(envir) <- .
    as.proto.environment(envir)  # must do this before eval(...)
    eval(substitute(eval(quote({ expr }))), envir)
    dots <- list(...); names <- names(dots)
    for (i in seq(length = length(dots))) { 
        assign(names[i], dots[[i]], envir)
        if (is.function(dots[[i]])) environment(envir[[names[i]]]) <- envir
    }
    if (length(dots)) as.proto.environment(envir) else envir
}

as.proto <- function(x, ...) UseMethod("as.proto")
as.proto.environment <- function(x, ...) {
	assign(".that", x, x)
	assign(".super", parent.env(x), x)
	structure(x, class = c("proto", "environment"))
}
as.proto.proto <- function(x, ...) x
as.proto.list <- function(x, envir, parent, FUN = function(x) TRUE, 
    all.names = FALSE, ...) {
       if (missing(envir)) {
		if (missing(parent)) parent <- parent.frame()
		envir <- if (is.proto(parent)) 
			parent$proto(...) 
		else 
			proto(parent, ...)
       }
       for(s in names(x))
          if (FUN(x[[s]])) {
             assign(s, x[[s]], envir)
             if (is.function(x[[s]])) environment(envir[[s]]) <- envir
          }
       if (!missing(parent)) parent.env(envir) <- parent
       as.proto.environment(envir)  # force refresh of .that and .super
}

"$.proto" <- function(this, x) {
   inh <- substr(x,1,2) != ".."
   p <- parent.frame()
   is.function <- is.function(get(x, this, inherits = inh))
   is.that <- match(deparse(substitute(this)), c(".that",".super"), nomatch=0)
   s <- if (is.function && !is.that)
         substitute(function(...) get(x, this, inherits = inh)(this, ...))
   else
         substitute(get(x, this, inherits = inh))
   res <- eval(s, list(inh = inh), p)
   if (is.function) environment(res) <- p
   res
}

"$<-.proto" <- function(this,s,value) { 
        if (s == ".super") parent.env(this) <- value
	if (is.function(value))  environment(value) <- this
	this[[as.character(substitute(s))]] <- value
	this
}

is.proto <- function(x) inherits(x, "proto")
isnot.function <- function(x) ! is.function(x)
