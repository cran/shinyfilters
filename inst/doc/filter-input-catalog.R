## ----setup, echo=FALSE--------------------------------------------------------
library(shinyfilters)
library(shiny)

knitr::opts_chunk$set(
	results = "asis"
)

## ----ex-dateInput-date, eval=FALSE--------------------------------------------
# filterInput(
# 	x = Sys.Date() + 0:9,
# 	inputId = "id",
# 	label = "Pick a date:"
# )

## ----ex-dateInput-posixt, eval=FALSE------------------------------------------
# filterInput(
# 	x = Sys.time() + as.difftime(0:9, units = "days"),
# 	inputId = "id",
# 	label = "Pick a date:"
# )

## ----ex-dateRangeInput-date, eval=FALSE---------------------------------------
# filterInput(
# 	x = Sys.Date() + 0:9,
# 	inputId = "id",
# 	label = "Pick a date range:",
# 	range = TRUE
# )

## ----ex-dateRangeInput-posixt, eval=FALSE-------------------------------------
# filterInput(
# 	x = Sys.time() + as.difftime(0:9, units = "days"),
# 	inputId = "id",
# 	label = "Pick a date range:",
# 	range = TRUE
# )

## ----ex-numericInput-numeric--------------------------------------------------
filterInput(
	x = 0:9,
	inputId = "id",
	label = "Pick a number:"
)

## ----ex-radioButtons-character------------------------------------------------
filterInput(
	x = letters[1:10],
	inputId = "id",
	label = "Pick a letter:",
	inline = TRUE,
	radio = TRUE
)

## ----ex-radioButtons-factor---------------------------------------------------
filterInput(
	x = as.factor(letters[1:10]),
	inputId = "id",
	label = "Pick a letter:",
	inline = TRUE,
	radio = TRUE
)

## ----ex-radioButtons-list-----------------------------------------------------
filterInput(
	x = as.list(letters[1:10]),
	inputId = "id",
	label = "Pick a letter:",
	inline = TRUE,
	radio = TRUE
)

## ----ex-selectInput-character-------------------------------------------------
filterInput(
	x = letters[1:10],
	inputId = "id",
	label = "Pick a letter:"
)

## ----ex-selectInput-factor----------------------------------------------------
filterInput(
	x = as.factor(letters[1:10]),
	inputId = "id",
	label = "Pick a letter:"
)

## ----ex-selectInput-list------------------------------------------------------
filterInput(
	x = as.list(letters[1:10]),
	inputId = "id",
	label = "Pick a letter:"
)

## ----ex-selectizeInput-character----------------------------------------------
filterInput(
	x = letters[1:10],
	inputId = "id",
	label = "Pick a letter:",
	selectize = TRUE
)

## ----ex-selectizeInput-factor-------------------------------------------------
filterInput(
	x = as.factor(letters[1:10]),
	inputId = "id",
	label = "Pick a letter:",
	selectize = TRUE
)

## ----ex-selectizeInput-list---------------------------------------------------
filterInput(
	x = as.list(letters[1:10]),
	inputId = "id",
	label = "Pick a letter:",
	selectize = TRUE
)

## ----ex-sliderInput-numeric, eval=FALSE---------------------------------------
# filterInput(
# 	x = 0:9,
# 	inputId = "id",
# 	label = "Pick a number:",
# 	slider = TRUE
# )

## ----ex-textAreaInput-character-----------------------------------------------
filterInput(
	x = letters[1:10],
	inputId = "id",
	label = "Type many letters[1:10]:",
	textbox = TRUE,
	area = TRUE
)

## ----ex-textInput-character---------------------------------------------------
filterInput(
	x = letters[1:10],
	inputId = "id",
	label = "Type a letter:",
	textbox = TRUE
)

## ----ex-character-default-----------------------------------------------------
filterInput(
	x = letters[1:10],
	inputId = "id",
	label = "Pick a letter:"
)

## ----ex-character-selectize---------------------------------------------------
filterInput(
	x = letters[1:10],
	inputId = "id",
	label = "Pick a letter:",
	selectize = TRUE
)

## ----ex-character-radio-------------------------------------------------------
filterInput(
	x = letters[1:10],
	inputId = "id",
	label = "Pick a letter:",
	inline = TRUE,
	radio = TRUE
)

## ----ex-character-text--------------------------------------------------------
filterInput(
	x = letters[1:10],
	inputId = "id",
	label = "Type a letter:",
	textbox = TRUE
)

## ----ex-character-textarea----------------------------------------------------
filterInput(
	x = letters[1:10],
	inputId = "id",
	label = "Type many letters[1:10]:",
	textbox = TRUE,
	area = TRUE
)

## ----ex-date-default, eval=FALSE----------------------------------------------
# filterInput(
# 	x = Sys.Date() + 0:9,
# 	inputId = "id",
# 	label = "Pick a date:"
# )

## ----ex-date-range, eval=FALSE------------------------------------------------
# filterInput(
# 	x = Sys.Date() + 0:9,
# 	inputId = "id",
# 	label = "Pick a date range:",
# 	range = TRUE
# )

## ----ex-factor-default--------------------------------------------------------
filterInput(
	x = as.factor(letters[1:10]),
	inputId = "id",
	label = "Pick a letter:"
)

## ----ex-factor-selectize------------------------------------------------------
filterInput(
	x = as.factor(letters[1:10]),
	inputId = "id",
	label = "Pick a letter:",
	selectize = TRUE
)

## ----ex-factor-radio----------------------------------------------------------
filterInput(
	x = as.factor(letters[1:10]),
	inputId = "id",
	label = "Pick a letter:",
	inline = TRUE,
	radio = TRUE
)

## ----ex-list-default----------------------------------------------------------
filterInput(
	x = as.list(letters[1:10]),
	inputId = "id",
	label = "Pick a letter:"
)

## ----ex-list-selectize--------------------------------------------------------
filterInput(
	x = as.list(letters[1:10]),
	inputId = "id",
	label = "Pick a letter:",
	selectize = TRUE
)

## ----ex-list-radio------------------------------------------------------------
filterInput(
	x = as.list(letters[1:10]),
	inputId = "id",
	label = "Pick a letter:",
	inline = TRUE,
	radio = TRUE
)

## ----ex-numeric-default-------------------------------------------------------
filterInput(
	x = 0:9,
	inputId = "id",
	label = "Pick a number:"
)

## ----ex-numeric-slider, eval=FALSE--------------------------------------------
# filterInput(
# 	x = 0:9,
# 	inputId = "id",
# 	label = "Pick a number:",
# 	slider = TRUE
# )

## ----ex-posixt-default, eval=FALSE--------------------------------------------
# filterInput(
# 	x = Sys.time() + as.difftime(0:9, units = "days"),
# 	inputId = "Id",
# 	label = "Pick a date:"
# )

## ----ex-posixt-range, eval=FALSE----------------------------------------------
# filterInput(
# 	x = Sys.time() + as.difftime(0:9, units = "days"),
# 	inputId = "Id",
# 	label = "Pick a date range:",
# 	range = TRUE
# )

