
name.proto <- function(., envir = parent.frame()) {
   stopifnot(is.environment(.) || 
      (is.character(.) && is.environment(get(., envir))))
   if (is.environment(.)) {
      if (exists("..Name", ., inherits = FALSE)) .$..Name
      else {
         L <- unlist(eapply(envir, identical, .))
         if (any(L)) names(L[L])[1]
         else gsub("^.* |>$", "", capture.output(print(.))[[1]])
      }
   } else {
      e <- get(., envir)
      if (exists("..Name", e, inherits = FALSE)) e$..Name
      else .
   }
}
      
dot.proto <- function(e = if (exists(".that")) .that else parent.frame(), 
     file = "", control) {
   control.default <- list(include = "graph [rankdir=BT];", 
	arrow.from.child = TRUE)
   if (missing(control)) control <- NULL
   control <- replace(control.default, names(control), control)
   with(control, {
	   nn <- unlist(eapply(e, is.proto))
	   nn <- names(nn[nn])
	   cat("digraph G {\n", include, "\n", file = file)
	   sapply(nn, function(x) cat(dQuote(name.proto(x,e)), 
	      if (arrow.from.child) "->" else "<-", 
	      dQuote(name.proto(get(x,e)$parent.env(), e)), ";\n", file = file, 
	      append = TRUE))
	   cat("}\n", file = file, append = TRUE)
	})
   }

# test
# a <- proto( { ..Name = "a" } )
# b <- proto( { ..Name = "b" }, parent = a)
# dot.proto()

