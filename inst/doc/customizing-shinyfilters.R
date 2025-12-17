## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
	collapse = TRUE,
	comment = "#>"
)

## ----message=FALSE------------------------------------------------------------
library(S7)

StringNonEmpty <- new_property(
	class = class_character,
	validator = function(value) {
		if (length(value) != 1 || is.na(value) || value == "") {
			return("must be a non-empty string")
		}
	}
)

Person <- new_class(
	name = "Person",
	properties = list(
		first_name = StringNonEmpty,
		last_name = StringNonEmpty
	)
)

## -----------------------------------------------------------------------------
People <- new_class(
	name = "People",
	parent = class_list,
	constructor = function(...) new_object(list(...)),
	validator = function(self) {
		if (!all(vapply(self, S7_inherits, logical(1), class = Person))) {
			return("must be a list of `Person`'s")
		}
	}
)

people <- People(
	Person("Ross", "Ihaka"),
	Person("Robert", "Gentleman")
)
people

## ----message=FALSE, error=TRUE------------------------------------------------
try({
library(shinyfilters)
library(shiny)

filterInput(people, inputId = "people", label = "Pick a person:")
})

## -----------------------------------------------------------------------------
method(filterInput, People) <- function(x, ...) {
	call_filter_input(x, shiny::selectizeInput, ...)
}

## ----error=TRUE---------------------------------------------------------------
try({
filterInput(people, inputId = "people", label = "Pick a person:")
})

## -----------------------------------------------------------------------------
full_names <- new_generic("full_names", "x")
method(full_names, People) <- function(x) vapply(x, full_names, character(1))
method(full_names, Person) <- function(x) paste(x@first_name, x@last_name)

method(args_filter_input, People) <- function(x, ...) {
	list(choices = full_names(x))
}

## ----results='asis'-----------------------------------------------------------
filterInput(people, inputId = "people", label = "Pick a person:")

## ----echo=FALSE---------------------------------------------------------------
if (!requireNamespace("shinyWidgets", quietly = TRUE)) {
	message("shinyWidgets not installed; code chunks will not be evaluated.")
	knitr::opts_chunk$set(eval = FALSE)
}

## -----------------------------------------------------------------------------
library(shinyWidgets)

method(filterInput, class_numeric) <- function(x, ...) {
	call_filter_input(x, numericRangeInput, ...)
}

## -----------------------------------------------------------------------------
filterInput(0:10, inputId = "number", label = "Pick a number:")

## -----------------------------------------------------------------------------
method(args_filter_input, class_numeric) <- function(x, ...) {
	list(
		# Value should be a length-two vector, per ?numericRangeInput
		value = c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))
	)
}

## -----------------------------------------------------------------------------
filterInput(0:10, inputId = "number", label = "Pick a number:")

## ----echo=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(eval = TRUE)

## -----------------------------------------------------------------------------
method(filterInput, People) <- function(x, ...) {
	shiny::selectizeInput(
		choices = full_names(x),
		...
	)
}

filterInput(people, inputId = "people", label = "Pick a person:")

## ----error=TRUE---------------------------------------------------------------
try({
filterInput(
	people,
	inputId = "people",
	label = "Pick a person:",
	choices = full_names(people)
)
})

## ----error=TRUE---------------------------------------------------------------
try({
method(filterInput, People) <- function(x, ...) {
	call_filter_input(x, shiny::selectizeInput, ...)
}

filterInput(
	people,
	inputId = "people",
	label = "Pick a person:",
	choices = full_names(people)
)
})

## ----echo=FALSE---------------------------------------------------------------
unloadNamespace("shinyfilters")
rm(filterInput)
rm(args_filter_input)

