# Update Functions
# Server-side functions to update UI inputs

# ============================================================================
# Text Input Updates
# ============================================================================

#' Update a text input
#'
#' @param session Shiny session object
#' @param inputId Input ID to update
#' @param label New label (NULL to leave unchanged)
#' @param value New value (NULL to leave unchanged)
#' @param placeholder New placeholder (NULL to leave unchanged)
#' @return NULL (called for side effect)
#' @export
updateTextInput <- function(session = getDefaultReactiveDomain(), inputId, 
                            label = NULL, value = NULL, placeholder = NULL) {
  message <- dropNulls(list(
    inputId = inputId,
    label = label,
    value = value,
    placeholder = placeholder
  ))
  
  session$sendInputMessage(inputId, message)
  invisible(NULL)
}

#' Update a textarea input
#'
#' @param session Shiny session object
#' @param inputId Input ID to update
#' @param label New label
#' @param value New value
#' @param placeholder New placeholder
#' @return NULL (called for side effect)
#' @export
updateTextAreaInput <- function(session = getDefaultReactiveDomain(), inputId,
                                label = NULL, value = NULL, placeholder = NULL) {
  message <- dropNulls(list(
    inputId = inputId,
    label = label,
    value = value,
    placeholder = placeholder
  ))
  
  session$sendInputMessage(inputId, message)
  invisible(NULL)
}

#' Update a numeric input
#'
#' @param session Shiny session object
#' @param inputId Input ID to update
#' @param label New label
#' @param value New value
#' @param min New minimum
#' @param max New maximum
#' @param step New step
#' @return NULL (called for side effect)
#' @export
updateNumericInput <- function(session = getDefaultReactiveDomain(), inputId,
                               label = NULL, value = NULL, min = NULL, max = NULL, step = NULL) {
  message <- dropNulls(list(
    inputId = inputId,
    label = label,
    value = value,
    min = min,
    max = max,
    step = step
  ))
  
  session$sendInputMessage(inputId, message)
  invisible(NULL)
}

# ============================================================================
# Selection Input Updates
# ============================================================================

#' Update a select input
#'
#' @param session Shiny session object
#' @param inputId Input ID to update
#' @param label New label
#' @param choices New choices
#' @param selected New selected value(s)
#' @return NULL (called for side effect)
#' @export
updateSelectInput <- function(session = getDefaultReactiveDomain(), inputId,
                              label = NULL, choices = NULL, selected = NULL) {
  if (!is.null(choices)) {
    choices <- normalizeChoices(choices)
    # Convert to list for JSON
    choices <- lapply(names(choices), function(name) {
      list(label = name, value = choices[[name]])
    })
  }
  
  message <- dropNulls(list(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected
  ))
  
  session$sendInputMessage(inputId, message)
  invisible(NULL)
}

#' Update a selectize input
#'
#' @param session Shiny session object
#' @param inputId Input ID to update
#' @param label New label
#' @param choices New choices
#' @param selected New selected value(s)
#' @param options Selectize options
#' @param server Use server-side selectize?
#' @return NULL (called for side effect)
#' @export
updateSelectizeInput <- function(session = getDefaultReactiveDomain(), inputId,
                                 label = NULL, choices = NULL, selected = NULL,
                                 options = NULL, server = FALSE) {
  updateSelectInput(session, inputId, label, choices, selected)
}

#' Update a checkbox input
#'
#' @param session Shiny session object
#' @param inputId Input ID to update
#' @param label New label
#' @param value New value (TRUE/FALSE)
#' @return NULL (called for side effect)
#' @export
updateCheckboxInput <- function(session = getDefaultReactiveDomain(), inputId,
                                label = NULL, value = NULL) {
  message <- dropNulls(list(
    inputId = inputId,
    label = label,
    value = value
  ))
  
  session$sendInputMessage(inputId, message)
  invisible(NULL)
}

#' Update a checkbox group input
#'
#' @param session Shiny session object
#' @param inputId Input ID to update
#' @param label New label
#' @param choices New choices
#' @param selected New selected value(s)
#' @param inline Display inline?
#' @param choiceNames Choice display names
#' @param choiceValues Choice values
#' @return NULL (called for side effect)
#' @export
updateCheckboxGroupInput <- function(session = getDefaultReactiveDomain(), inputId,
                                     label = NULL, choices = NULL, selected = NULL,
                                     inline = FALSE, choiceNames = NULL, choiceValues = NULL) {
  if (!is.null(choiceNames) && !is.null(choiceValues)) {
    choices <- setNames(as.list(choiceValues), choiceNames)
  } else if (!is.null(choices)) {
    choices <- normalizeChoices(choices)
  }
  
  message <- dropNulls(list(
    inputId = inputId,
    label = label,
    choices = if (!is.null(choices)) {
      lapply(names(choices), function(name) {
        list(label = name, value = choices[[name]])
      })
    },
    selected = selected,
    inline = inline
  ))
  
  session$sendInputMessage(inputId, message)
  invisible(NULL)
}

#' Update radio buttons
#'
#' @param session Shiny session object
#' @param inputId Input ID to update
#' @param label New label
#' @param choices New choices
#' @param selected New selected value
#' @param inline Display inline?
#' @param choiceNames Choice display names
#' @param choiceValues Choice values
#' @return NULL (called for side effect)
#' @export
updateRadioButtons <- function(session = getDefaultReactiveDomain(), inputId,
                               label = NULL, choices = NULL, selected = NULL,
                               inline = FALSE, choiceNames = NULL, choiceValues = NULL) {
  if (!is.null(choiceNames) && !is.null(choiceValues)) {
    choices <- setNames(as.list(choiceValues), choiceNames)
  } else if (!is.null(choices)) {
    choices <- normalizeChoices(choices)
  }
  
  message <- dropNulls(list(
    inputId = inputId,
    label = label,
    choices = if (!is.null(choices)) {
      lapply(names(choices), function(name) {
        list(label = name, value = choices[[name]])
      })
    },
    selected = selected,
    inline = inline
  ))
  
  session$sendInputMessage(inputId, message)
  invisible(NULL)
}

# ============================================================================
# Date Input Updates
# ============================================================================

#' Update a date input
#'
#' @param session Shiny session object
#' @param inputId Input ID to update
#' @param label New label
#' @param value New value
#' @param min New minimum date
#' @param max New maximum date
#' @return NULL (called for side effect)
#' @export
updateDateInput <- function(session = getDefaultReactiveDomain(), inputId,
                            label = NULL, value = NULL, min = NULL, max = NULL) {
  # Format dates
  format_date <- function(d) {
    if (is.null(d)) return(NULL)
    if (inherits(d, "Date")) return(format(d, "%Y-%m-%d"))
    as.character(d)
  }
  
  message <- dropNulls(list(
    inputId = inputId,
    label = label,
    value = format_date(value),
    min = format_date(min),
    max = format_date(max)
  ))
  
  session$sendInputMessage(inputId, message)
  invisible(NULL)
}

#' Update a date range input
#'
#' @param session Shiny session object
#' @param inputId Input ID to update
#' @param label New label
#' @param start New start date
#' @param end New end date
#' @param min New minimum date
#' @param max New maximum date
#' @return NULL (called for side effect)
#' @export
updateDateRangeInput <- function(session = getDefaultReactiveDomain(), inputId,
                                 label = NULL, start = NULL, end = NULL,
                                 min = NULL, max = NULL) {
  format_date <- function(d) {
    if (is.null(d)) return(NULL)
    if (inherits(d, "Date")) return(format(d, "%Y-%m-%d"))
    as.character(d)
  }
  
  message <- dropNulls(list(
    inputId = inputId,
    label = label,
    start = format_date(start),
    end = format_date(end),
    min = format_date(min),
    max = format_date(max)
  ))
  
  session$sendInputMessage(inputId, message)
  invisible(NULL)
}

# ============================================================================
# Slider Input Updates
# ============================================================================

#' Update a slider input
#'
#' @param session Shiny session object
#' @param inputId Input ID to update
#' @param label New label
#' @param value New value (single or two values for range)
#' @param min New minimum
#' @param max New maximum
#' @param step New step
#' @param timeFormat Time format for date sliders
#' @param timezone Time zone
#' @return NULL (called for side effect)
#' @export
updateSliderInput <- function(session = getDefaultReactiveDomain(), inputId,
                              label = NULL, value = NULL, min = NULL, max = NULL,
                              step = NULL, timeFormat = NULL, timezone = NULL) {
  message <- dropNulls(list(
    inputId = inputId,
    label = label,
    value = value,
    min = min,
    max = max,
    step = step,
    timeFormat = timeFormat,
    timezone = timezone
  ))
  
  session$sendInputMessage(inputId, message)
  invisible(NULL)
}

# ============================================================================
# Action Input Updates
# ============================================================================

#' Update an action button
#'
#' @param session Shiny session object
#' @param inputId Input ID to update
#' @param label New label
#' @param icon New icon
#' @param disabled Disabled state
#' @return NULL (called for side effect)
#' @export
updateActionButton <- function(session = getDefaultReactiveDomain(), inputId,
                               label = NULL, icon = NULL, disabled = NULL) {
  icon_html <- NULL
  if (!is.null(icon)) {
    icon_html <- tag_to_html(icon)
  }
  
  message <- dropNulls(list(
    inputId = inputId,
    label = label,
    icon = icon_html,
    disabled = disabled
  ))
  
  session$sendInputMessage(inputId, message)
  invisible(NULL)
}

#' Update an action link
#'
#' @param session Shiny session object
#' @param inputId Input ID to update
#' @param label New label
#' @param icon New icon
#' @return NULL (called for side effect)
#' @export
updateActionLink <- function(session = getDefaultReactiveDomain(), inputId,
                             label = NULL, icon = NULL) {
  updateActionButton(session, inputId, label, icon)
}

# ============================================================================
# Navigation Updates
# ============================================================================

#' Update a tabset panel
#'
#' @param session Shiny session object
#' @param inputId Tabset ID
#' @param selected Tab to select
#' @return NULL (called for side effect)
#' @export
updateTabsetPanel <- function(session = getDefaultReactiveDomain(), inputId, selected = NULL) {
  message <- dropNulls(list(
    inputId = inputId,
    selected = selected
  ))
  
  session$sendInputMessage(inputId, message)
  invisible(NULL)
}

#' Update a navbar page
#'
#' @param session Shiny session object
#' @param inputId Navbar ID
#' @param selected Tab to select
#' @return NULL (called for side effect)
#' @export
updateNavbarPage <- function(session = getDefaultReactiveDomain(), inputId, selected = NULL) {
  updateTabsetPanel(session, inputId, selected)
}

#' Update a navlist panel
#'
#' @param session Shiny session object
#' @param inputId Navlist ID
#' @param selected Tab to select
#' @return NULL (called for side effect)
#' @export
updateNavlistPanel <- function(session = getDefaultReactiveDomain(), inputId, selected = NULL) {
  updateTabsetPanel(session, inputId, selected)
}

# ============================================================================
# URL Updates
# ============================================================================

#' Update the browser's URL
#'
#' @param queryString New query string
#' @param mode Update mode: "push" or "replace"
#' @param session Shiny session object
#' @return NULL (called for side effect)
#' @export
updateQueryString <- function(queryString, mode = c("push", "replace"),
                              session = getDefaultReactiveDomain()) {
  mode <- match.arg(mode)
  
  if (!is.null(session)) {
    session$sendCustomMessage("shiny-update-query-string", list(
      queryString = queryString,
      mode = mode
    ))
  }
  
  invisible(NULL)
}

#' Get the query string from the URL
#'
#' @param session Shiny session object
#' @return Named list of query parameters
#' @export
getQueryString <- function(session = getDefaultReactiveDomain()) {
  if (is.null(session)) {
    return(list())
  }
  
  # This would need to be implemented with client communication
  # For now, return empty list
  session$clientData$url_search %||% list()
}

#' Get the URL hash
#'
#' @param session Shiny session object
#' @return Hash string (without #)
#' @export
getUrlHash <- function(session = getDefaultReactiveDomain()) {
  if (is.null(session)) {
    return("")
  }
  
  session$clientData$url_hash %||% ""
}

# ============================================================================
# Variable Select Updates
# ============================================================================

#' Update a variable select input
#'
#' @param session Shiny session object
#' @param inputId Input ID to update
#' @param label New label
#' @param data New data frame for variables
#' @param selected New selected variable(s)
#' @return NULL (called for side effect)
#' @export
updateVarSelectInput <- function(session = getDefaultReactiveDomain(), inputId,
                                 label = NULL, data = NULL, selected = NULL) {
  choices <- if (!is.null(data)) names(data) else NULL
  updateSelectInput(session, inputId, label, choices, selected)
}

#' Update a variable selectize input
#'
#' @param session Shiny session object
#' @param inputId Input ID to update
#' @param label New label
#' @param data New data frame for variables
#' @param selected New selected variable(s)
#' @return NULL (called for side effect)
#' @export
updateVarSelectizeInput <- function(session = getDefaultReactiveDomain(), inputId,
                                    label = NULL, data = NULL, selected = NULL) {
  updateVarSelectInput(session, inputId, label, data, selected)
}

# ============================================================================
# Helper Functions
# ============================================================================

#' Remove NULL values from a list
#'
#' @param x List to filter
#' @return List with NULL values removed
dropNulls <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

#' Null-coalescing operator
#' @param x Value to check
#' @param y Default value
#' @return x if not NULL, otherwise y
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
