# UI Input Functions
# Implements all Shiny-compatible input functions

# ----------------------------------------------------------------------------
# Extra-attribute helper
# ----------------------------------------------------------------------------
# Lets input functions accept `...` (most usefully `class = "..."`) and forward
# those attributes onto a chosen element -- normally the control itself (the
# <input>/<select>/<textarea>) so users can attach utility classes (e.g.
# Tailwind) directly: textInput("name", NULL, class = "border rounded-xl").
#
# `class` is appended to the element's existing class rather than replacing it;
# any other named attribute is set (overwriting if already present). Unnamed
# `...` entries are ignored.
.apply_extra_attribs <- function(tag_obj, extra) {
  if (length(extra) == 0) return(tag_obj)
  nms <- names(extra)
  if (is.null(nms)) return(tag_obj)
  for (i in seq_along(extra)) {
    nm <- nms[i]
    val <- extra[[i]]
    if (is.null(nm) || nm == "" || is.null(val)) next
    if (identical(nm, "class")) {
      existing <- tag_obj$attribs$class
      add <- paste(as.character(val), collapse = " ")
      tag_obj$attribs$class <- if (is.null(existing) || !nzchar(existing)) add else paste(existing, add)
    } else {
      tag_obj$attribs[[nm]] <- val
    }
  }
  tag_obj
}

# ============================================================================
# Text Inputs
# ============================================================================

#' Create a text input control
#'
#' @param inputId The input slot that will be used to access the value
#' @param label Display label for the control
#' @param value Initial value
#' @param width Width of the input (e.g., "100%", "400px")
#' @param placeholder Placeholder text
#' @param ... Additional attributes applied to the `<input>` element. Most
#'   usefully `class` (appended to the base class, e.g. for Tailwind utilities).
#' @return A text input tag
#' @export
textInput <- function(inputId, label, value = "", width = NULL, placeholder = NULL, ...) {
  input_tag <- tag("input", attribs = list(
    type = "text",
    id = inputId,
    name = inputId,
    class = "form-control",
    value = value,
    `data-input-id` = inputId
  ))

  if (!is.null(placeholder)) {
    input_tag$attribs$placeholder <- placeholder
  }
  input_tag <- .apply_extra_attribs(input_tag, list(...))

  container_style <- if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";") else NULL
  
  tag("div", 
    attribs = list(
      class = "form-group shiny-input-container",
      style = container_style
    ),
    children = list(
      if (!is.null(label) && label != "") tag("label", attribs = list(`for` = inputId, class = "form-label"), children = list(label)),
      input_tag
    )
  )
}

#' Create a textarea input control
#'
#' @param inputId The input slot that will be used to access the value
#' @param label Display label for the control
#' @param value Initial value
#' @param width Width of the input
#' @param height Height of the input
#' @param cols Number of columns (overrides width)
#' @param rows Number of rows (overrides height)
#' @param placeholder Placeholder text
#' @param resize Resize behavior: "both", "none", "vertical", "horizontal"
#' @return A textarea input tag
#' @export
textAreaInput <- function(inputId, label, value = "", width = NULL, height = NULL,
                          cols = NULL, rows = NULL, placeholder = NULL, resize = NULL, ...) {

  textarea_attribs <- list(
    id = inputId,
    name = inputId,
    class = "form-control",
    `data-input-id` = inputId
  )

  if (!is.null(cols)) textarea_attribs$cols <- cols
  if (!is.null(rows)) textarea_attribs$rows <- rows
  if (!is.null(placeholder)) textarea_attribs$placeholder <- placeholder

  # Build style string
  styles <- character(0)
  if (!is.null(width)) styles <- c(styles, paste0("width: ", validateCssUnit(width)))
  if (!is.null(height)) styles <- c(styles, paste0("height: ", validateCssUnit(height)))
  if (!is.null(resize)) styles <- c(styles, paste0("resize: ", resize))
  if (length(styles) > 0) {
    textarea_attribs$style <- paste(styles, collapse = "; ")
  }

  textarea_tag <- tag("textarea", attribs = textarea_attribs, children = list(value))
  textarea_tag <- .apply_extra_attribs(textarea_tag, list(...))
  
  container_style <- if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";") else NULL
  
  tag("div",
    attribs = list(
      class = "form-group shiny-input-container",
      style = container_style
    ),
    children = list(
      if (!is.null(label) && label != "") tag("label", attribs = list(`for` = inputId, class = "form-label"), children = list(label)),
      textarea_tag
    )
  )
}

#' Create a password input control
#'
#' @param inputId The input slot that will be used to access the value
#' @param label Display label for the control
#' @param value Initial value
#' @param width Width of the input
#' @param placeholder Placeholder text
#' @return A password input tag
#' @export
passwordInput <- function(inputId, label, value = "", width = NULL, placeholder = NULL, ...) {
  input_tag <- tag("input", attribs = list(
    type = "password",
    id = inputId,
    name = inputId,
    class = "form-control",
    value = value,
    `data-input-id` = inputId
  ))

  if (!is.null(placeholder)) {
    input_tag$attribs$placeholder <- placeholder
  }
  input_tag <- .apply_extra_attribs(input_tag, list(...))

  container_style <- if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";") else NULL
  
  tag("div",
    attribs = list(
      class = "form-group shiny-input-container",
      style = container_style
    ),
    children = list(
      if (!is.null(label) && label != "") tag("label", attribs = list(`for` = inputId, class = "form-label"), children = list(label)),
      input_tag
    )
  )
}

#' Create a numeric input control
#'
#' @param inputId The input slot that will be used to access the value
#' @param label Display label for the control
#' @param value Initial value
#' @param min Minimum allowed value
#' @param max Maximum allowed value
#' @param step Interval to use when stepping between min and max
#' @param width Width of the input
#' @return A numeric input tag
#' @export
numericInput <- function(inputId, label, value, min = NA, max = NA, step = NA, width = NULL, ...) {
  input_attribs <- list(
    type = "number",
    id = inputId,
    name = inputId,
    class = "form-control",
    value = value,
    `data-input-id` = inputId
  )

  if (!is.na(min)) input_attribs$min <- min
  if (!is.na(max)) input_attribs$max <- max
  if (!is.na(step)) input_attribs$step <- step

  input_tag <- tag("input", attribs = input_attribs)
  input_tag <- .apply_extra_attribs(input_tag, list(...))

  container_style <- if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";") else NULL
  
  tag("div",
    attribs = list(
      class = "form-group shiny-input-container",
      style = container_style
    ),
    children = list(
      if (!is.null(label) && label != "") tag("label", attribs = list(`for` = inputId, class = "form-label"), children = list(label)),
      input_tag
    )
  )
}

# ============================================================================
# Selection Inputs
# ============================================================================

#' Create a select list input control
#'
#' @param inputId The input slot that will be used to access the value
#' @param label Display label for the control
#' @param choices List of values to select from
#' @param selected Initially selected value(s)
#' @param multiple Allow multiple selections?
#' @param selectize Use selectize.js?
#' @param width Width of the input
#' @param size Number of visible options (for non-selectize)
#' @return A select input tag
#' @export
selectInput <- function(inputId, label, choices, selected = NULL, multiple = FALSE,
                        selectize = TRUE, width = NULL, size = NULL, ...) {

  # Normalize choices to named list
  choices <- normalizeChoices(choices)
  
  # Build option elements
  options <- lapply(names(choices), function(name) {
    value <- choices[[name]]
    is_selected <- if (is.null(selected)) FALSE else value %in% selected
    
    option_attribs <- list(value = value)
    if (is_selected) option_attribs$selected <- "selected"
    
    tag("option", attribs = option_attribs, children = list(name))
  })
  
  select_attribs <- list(
    id = inputId,
    name = inputId,
    class = if (selectize) "form-select shiny-input-select selectized" else "form-select shiny-input-select",
    `data-input-id` = inputId
  )
  
  if (multiple) select_attribs$multiple <- "multiple"
  if (!is.null(size) && !selectize) select_attribs$size <- size

  select_tag <- tag("select", attribs = select_attribs, children = options)
  select_tag <- .apply_extra_attribs(select_tag, list(...))

  container_style <- if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";") else NULL
  
  tag("div",
    attribs = list(
      class = "form-group shiny-input-container",
      style = container_style
    ),
    children = list(
      if (!is.null(label) && label != "") tag("label", attribs = list(`for` = inputId, class = "form-label"), children = list(label)),
      select_tag
    )
  )
}

#' Create a selectize input (enhanced select)
#'
#' @param inputId The input slot that will be used to access the value
#' @param label Display label for the control
#' @param choices List of values to select from
#' @param selected Initially selected value(s)
#' @param multiple Allow multiple selections?
#' @param options Options for selectize.js
#' @param width Width of the input
#' @return A selectize input tag
#' @export
selectizeInput <- function(inputId, label, choices, selected = NULL, multiple = FALSE,
                          options = NULL, width = NULL) {
  selectInput(inputId, label, choices, selected, multiple, selectize = TRUE, width = width)
}

#' Create variable select input
#'
#' @param inputId The input slot that will be used to access the value
#' @param label Display label for the control
#' @param data Data frame to select variables from
#' @param selected Initially selected variable(s)
#' @param multiple Allow multiple selections?
#' @param selectize Use selectize.js?
#' @param width Width of the input
#' @param size Number of visible options
#' @return A variable select input tag
#' @export
varSelectInput <- function(inputId, label, data, selected = NULL, multiple = FALSE,
                           selectize = TRUE, width = NULL, size = NULL) {
  choices <- names(data)
  selectInput(inputId, label, choices, selected, multiple, selectize, width, size)
}

#' Create variable selectize input
#'
#' @param inputId The input slot that will be used to access the value
#' @param label Display label for the control
#' @param data Data frame to select variables from
#' @param selected Initially selected variable(s)
#' @param multiple Allow multiple selections?
#' @param width Width of the input
#' @return A variable selectize input tag
#' @export
varSelectizeInput <- function(inputId, label, data, selected = NULL, multiple = FALSE, width = NULL) {
  varSelectInput(inputId, label, data, selected, multiple, selectize = TRUE, width = width)
}

#' Create radio buttons
#'
#' @param inputId The input slot that will be used to access the value
#' @param label Display label for the control
#' @param choices List of values to select from
#' @param selected Initially selected value
#' @param inline Display choices inline?
#' @param width Width of the container
#' @param choiceNames Names to display (alternative to choices)
#' @param choiceValues Values to return (alternative to choices)
#' @return Radio buttons tag
#' @export
radioButtons <- function(inputId, label, choices = NULL, selected = NULL, inline = FALSE,
                         width = NULL, choiceNames = NULL, choiceValues = NULL, ...) {

  # Handle choiceNames/choiceValues
  if (!is.null(choiceNames) && !is.null(choiceValues)) {
    choices <- setNames(as.list(choiceValues), choiceNames)
  } else {
    choices <- normalizeChoices(choices)
  }
  
  # Default selection
  if (is.null(selected) && length(choices) > 0) {
    selected <- choices[[1]]
  }
  
  # Build radio elements
  radios <- lapply(seq_along(choices), function(i) {
    name <- names(choices)[i]
    value <- choices[[i]]
    is_selected <- !is.null(selected) && value == selected
    radio_id <- paste0(inputId, "_", i)
    
    input_tag <- tag("input", attribs = list(
      type = "radio",
      id = radio_id,
      name = inputId,
      value = value,
      class = "form-check-input",
      `data-input-id` = inputId,
      checked = if (is_selected) "checked" else NULL
    ))
    
    label_tag <- tag("label", 
      attribs = list(class = "form-check-label", `for` = radio_id),
      children = list(name)
    )
    
    tag("div",
      attribs = list(class = if (inline) "form-check form-check-inline" else "form-check"),
      children = list(input_tag, label_tag)
    )
  })
  
  container_style <- if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";") else NULL

  container <- tag("div",
    attribs = list(
      id = inputId,
      class = "form-group shiny-input-radiogroup shiny-input-container",
      style = container_style,
      `data-input-id` = inputId
    ),
    children = c(
      list(if (!is.null(label) && label != "") tag("label", attribs = list(class = "form-label"), children = list(label))),
      radios
    )
  )
  # Grouped input: extra attributes / class go on the group container.
  .apply_extra_attribs(container, list(...))
}

#' Create a single checkbox input
#'
#' @param inputId The input slot that will be used to access the value
#' @param label Display label for the control
#' @param value Initial value (TRUE or FALSE)
#' @param width Width of the container
#' @return A checkbox input tag
#' @export
checkboxInput <- function(inputId, label, value = FALSE, width = NULL, ...) {
  input_tag <- tag("input", attribs = list(
    type = "checkbox",
    id = inputId,
    name = inputId,
    class = "form-check-input",
    `data-input-id` = inputId,
    checked = if (value) "checked" else NULL
  ))
  input_tag <- .apply_extra_attribs(input_tag, list(...))

  label_tag <- tag("label",
    attribs = list(class = "form-check-label", `for` = inputId),
    children = list(label)
  )

  container_style <- if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";") else NULL
  
  tag("div",
    attribs = list(
      class = "form-check shiny-input-container",
      style = container_style
    ),
    children = list(input_tag, label_tag)
  )
}

#' Create a checkbox group input
#'
#' @param inputId The input slot that will be used to access the value
#' @param label Display label for the control
#' @param choices List of values to select from
#' @param selected Initially selected value(s)
#' @param inline Display choices inline?
#' @param width Width of the container
#' @param choiceNames Names to display (alternative to choices)
#' @param choiceValues Values to return (alternative to choices)
#' @return A checkbox group tag
#' @export
checkboxGroupInput <- function(inputId, label, choices = NULL, selected = NULL, inline = FALSE,
                                width = NULL, choiceNames = NULL, choiceValues = NULL, ...) {

  # Handle choiceNames/choiceValues
  if (!is.null(choiceNames) && !is.null(choiceValues)) {
    choices <- setNames(as.list(choiceValues), choiceNames)
  } else {
    choices <- normalizeChoices(choices)
  }
  
  # Build checkbox elements
  checkboxes <- lapply(seq_along(choices), function(i) {
    name <- names(choices)[i]
    value <- choices[[i]]
    is_selected <- !is.null(selected) && value %in% selected
    checkbox_id <- paste0(inputId, "_", i)
    
    input_tag <- tag("input", attribs = list(
      type = "checkbox",
      id = checkbox_id,
      name = inputId,
      value = value,
      class = "form-check-input",
      `data-input-id` = inputId,
      checked = if (is_selected) "checked" else NULL
    ))
    
    label_tag <- tag("label",
      attribs = list(class = "form-check-label", `for` = checkbox_id),
      children = list(name)
    )
    
    tag("div",
      attribs = list(class = if (inline) "form-check form-check-inline" else "form-check"),
      children = list(input_tag, label_tag)
    )
  })
  
  container_style <- if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";") else NULL

  container <- tag("div",
    attribs = list(
      id = inputId,
      class = "form-group shiny-input-checkboxgroup shiny-input-container",
      style = container_style,
      `data-input-id` = inputId
    ),
    children = c(
      list(if (!is.null(label) && label != "") tag("label", attribs = list(class = "form-label"), children = list(label))),
      checkboxes
    )
  )
  # Grouped input: extra attributes / class go on the group container.
  .apply_extra_attribs(container, list(...))
}

# ============================================================================
# Date Inputs
# ============================================================================

#' Create a date input
#'
#' @param inputId The input slot that will be used to access the value
#' @param label Display label for the control
#' @param value Initial value (Date or character)
#' @param min Minimum selectable date
#' @param max Maximum selectable date
#' @param format Date format string
#' @param startview Starting view: "month", "year", "decade"
#' @param weekstart Day of week to start (0 = Sunday)
#' @param language Language code
#' @param width Width of the input
#' @param autoclose Close picker on selection?
#' @param datesdisabled Vector of disabled dates
#' @param daysofweekdisabled Vector of disabled days (0-6)
#' @return A date input tag
#' @export
dateInput <- function(inputId, label, value = NULL, min = NULL, max = NULL,
                      format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                      language = "en", width = NULL, autoclose = TRUE,
                      datesdisabled = NULL, daysofweekdisabled = NULL, ...) {
  
  # Format value
  if (is.null(value)) {
    value <- ""
  } else if (inherits(value, "Date")) {
    value <- format(value, "%Y-%m-%d")
  }
  
  input_attribs <- list(
    type = "date",
    id = inputId,
    name = inputId,
    class = "form-control shiny-date-input",
    value = value,
    `data-input-id` = inputId,
    `data-date-format` = format,
    `data-date-language` = language,
    `data-date-start-view` = startview,
    `data-date-week-start` = weekstart,
    `data-date-autoclose` = if (autoclose) "true" else "false"
  )
  
  if (!is.null(min)) {
    if (inherits(min, "Date")) min <- format(min, "%Y-%m-%d")
    input_attribs$min <- min
  }
  if (!is.null(max)) {
    if (inherits(max, "Date")) max <- format(max, "%Y-%m-%d")
    input_attribs$max <- max
  }
  
  input_tag <- tag("input", attribs = input_attribs)
  input_tag <- .apply_extra_attribs(input_tag, list(...))

  container_style <- if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";") else NULL

  tag("div",
    attribs = list(
      class = "form-group shiny-input-container",
      style = container_style
    ),
    children = list(
      if (!is.null(label) && label != "") tag("label", attribs = list(`for` = inputId, class = "form-label"), children = list(label)),
      input_tag
    )
  )
}

#' Create a date range input
#'
#' @param inputId The input slot that will be used to access the value
#' @param label Display label for the control
#' @param start Initial start date
#' @param end Initial end date
#' @param min Minimum selectable date
#' @param max Maximum selectable date
#' @param format Date format string
#' @param startview Starting view: "month", "year", "decade"
#' @param weekstart Day of week to start (0 = Sunday)
#' @param language Language code
#' @param separator Separator between dates
#' @param width Width of the input
#' @param autoclose Close picker on selection?
#' @return A date range input tag
#' @export
dateRangeInput <- function(inputId, label, start = NULL, end = NULL, min = NULL, max = NULL,
                           format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                           language = "en", separator = " to ", width = NULL, autoclose = TRUE) {
  
  # Format values
  format_date <- function(d) {
    if (is.null(d)) return("")
    if (inherits(d, "Date")) return(format(d, "%Y-%m-%d"))
    as.character(d)
  }
  
  start <- format_date(start)
  end <- format_date(end)
  min_str <- format_date(min)
  max_str <- format_date(max)
  
  # Start date input
  start_input <- tag("input", attribs = list(
    type = "date",
    id = paste0(inputId, "_start"),
    name = paste0(inputId, "_start"),
    class = "form-control",
    value = start,
    min = if (min_str != "") min_str else NULL,
    max = if (max_str != "") max_str else NULL,
    `data-input-id` = inputId
  ))
  
  # End date input
  end_input <- tag("input", attribs = list(
    type = "date",
    id = paste0(inputId, "_end"),
    name = paste0(inputId, "_end"),
    class = "form-control",
    value = end,
    min = if (min_str != "") min_str else NULL,
    max = if (max_str != "") max_str else NULL,
    `data-input-id` = inputId
  ))
  
  container_style <- if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";") else NULL
  
  tag("div",
    attribs = list(
      id = inputId,
      class = "form-group shiny-input-container shiny-date-range-input",
      style = container_style,
      `data-input-id` = inputId
    ),
    children = list(
      if (!is.null(label) && label != "") tag("label", attribs = list(class = "form-label"), children = list(label)),
      tag("div",
        attribs = list(class = "input-group"),
        children = list(
          start_input,
          tag("span", attribs = list(class = "input-group-text"), children = list(separator)),
          end_input
        )
      )
    )
  )
}

# ============================================================================
# Slider Input
# ============================================================================

#' Create a slider input
#'
#' @param inputId The input slot that will be used to access the value
#' @param label Display label for the control
#' @param min Minimum value
#' @param max Maximum value
#' @param value Initial value (single or two values for range)
#' @param step Step size
#' @param round Round to nearest step?
#' @param ticks Show tick marks?
#' @param animate Add animation? Can be TRUE or animationOptions()
#' @param width Width of the slider
#' @param sep Thousands separator
#' @param pre Prefix for displayed values
#' @param post Suffix for displayed values
#' @param timeFormat Time format if values are dates
#' @param timezone Time zone
#' @param dragRange Allow dragging the range?
#' @return A slider input tag
#' @export
sliderInput <- function(inputId, label, min, max, value, step = NULL, round = FALSE,
                        ticks = TRUE, animate = FALSE, width = NULL, sep = ",",
                        pre = NULL, post = NULL, timeFormat = NULL, timezone = NULL,
                        dragRange = TRUE, ...) {
  
  # Check if range slider (two values)
  is_range <- length(value) == 2
  
  input_attribs <- list(
    type = "range",
    id = inputId,
    name = inputId,
    class = "form-range shiny-input-slider",
    min = min,
    max = max,
    value = if (is_range) value[1] else value,
    `data-input-id` = inputId,
    `data-min` = min,
    `data-max` = max,
    `data-from` = if (is_range) value[1] else value,
    `data-to` = if (is_range) value[2] else NULL,
    `data-step` = step,
    `data-round` = if (round) "true" else "false",
    `data-ticks` = if (ticks) "true" else "false",
    `data-sep` = sep,
    `data-pre` = pre,
    `data-post` = post,
    `data-drag-range` = if (dragRange) "true" else "false"
  )
  
  if (!is.null(step)) input_attribs$step <- step
  if (!is.null(timeFormat)) input_attribs$`data-time-format` <- timeFormat
  if (!is.null(timezone)) input_attribs$`data-timezone` <- timezone
  
  # Handle animate option
  if (!isFALSE(animate)) {
    if (isTRUE(animate)) {
      input_attribs$`data-animate` <- "true"
    } else if (is.list(animate)) {
      input_attribs$`data-animate` <- "true"
      if (!is.null(animate$interval)) input_attribs$`data-interval` <- animate$interval
      if (!is.null(animate$loop)) input_attribs$`data-loop` <- if (animate$loop) "true" else "false"
    }
  }
  
  input_tag <- tag("input", attribs = input_attribs)
  input_tag <- .apply_extra_attribs(input_tag, list(...))

  # Value display
  value_display <- tag("output",
    attribs = list(
      id = paste0(inputId, "_value"),
      `for` = inputId,
      class = "slider-value"
    ),
    children = list(
      if (is_range) paste0(value[1], " - ", value[2]) else as.character(value)
    )
  )
  
  container_style <- if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";") else NULL
  
  tag("div",
    attribs = list(
      class = "form-group shiny-input-container",
      style = container_style
    ),
    children = list(
      if (!is.null(label) && label != "") {
        tag("div", attribs = list(class = "d-flex justify-content-between"), children = list(
          tag("label", attribs = list(`for` = inputId, class = "form-label"), children = list(label)),
          value_display
        ))
      },
      input_tag
    )
  )
}

#' Animation options for slider input
#'
#' @param interval Time between animation frames (ms)
#' @param loop Loop the animation?
#' @param playButton Custom play button
#' @param pauseButton Custom pause button
#' @return Animation options list
#' @export
animationOptions <- function(interval = 1000, loop = FALSE, playButton = NULL, pauseButton = NULL) {
  list(
    interval = interval,
    loop = loop,
    playButton = playButton,
    pauseButton = pauseButton
  )
}

# ============================================================================
# Action Inputs
# ============================================================================

#' Create an action button
#'
#' @param inputId The input slot that will be used to access the value
#' @param label Button label
#' @param icon Optional icon (from icon())
#' @param width Width of the button
#' @param ... Additional attributes
#' @return An action button tag
#' @export
actionButton <- function(inputId, label, icon = NULL, width = NULL, ...) {
  extra_args <- list(...)
  
  btn_class <- "btn btn-primary action-button"
  if (!is.null(extra_args$class)) {
    btn_class <- paste(btn_class, extra_args$class)
    extra_args$class <- NULL
  }
  
  btn_attribs <- c(list(
    type = "button",
    id = inputId,
    class = btn_class,
    `data-input-id` = inputId,
    `data-val` = 0
  ), extra_args)
  
  if (!is.null(width)) {
    btn_attribs$style <- paste0("width: ", validateCssUnit(width), ";")
  }
  
  children <- list()
  if (!is.null(icon)) {
    children <- c(children, list(icon, " "))
  }
  children <- c(children, list(label))
  
  tag("button", attribs = btn_attribs, children = children)
}

#' Create an action link
#'
#' @param inputId The input slot that will be used to access the value
#' @param label Link label
#' @param icon Optional icon
#' @param ... Additional attributes
#' @return An action link tag
#' @export
actionLink <- function(inputId, label, icon = NULL, ...) {
  extra_args <- list(...)
  
  link_class <- "action-button"
  if (!is.null(extra_args$class)) {
    link_class <- paste(link_class, extra_args$class)
    extra_args$class <- NULL
  }
  
  link_attribs <- c(list(
    href = "#",
    id = inputId,
    class = link_class,
    `data-input-id` = inputId,
    `data-val` = 0
  ), extra_args)
  
  children <- list()
  if (!is.null(icon)) {
    children <- c(children, list(icon, " "))
  }
  children <- c(children, list(label))
  
  tag("a", attribs = link_attribs, children = children)
}

#' Create a submit button
#'
#' @param text Button text
#' @param icon Optional icon
#' @param width Width of the button
#' @return A submit button tag
#' @export
submitButton <- function(text = "Submit", icon = NULL, width = NULL) {
  btn_attribs <- list(
    type = "submit",
    class = "btn btn-primary"
  )
  
  if (!is.null(width)) {
    btn_attribs$style <- paste0("width: ", validateCssUnit(width), ";")
  }
  
  children <- list()
  if (!is.null(icon)) {
    children <- c(children, list(icon, " "))
  }
  children <- c(children, list(text))
  
  tag("button", attribs = btn_attribs, children = children)
}

#' Create a file input
#'
#' @param inputId The input slot that will be used to access the value
#' @param label Display label for the control
#' @param multiple Allow multiple file selection?
#' @param accept MIME types or file extensions to accept
#' @param width Width of the input
#' @param buttonLabel Label for the browse button
#' @param placeholder Placeholder text for file name
#' @param capture For mobile: "user" for front camera, "environment" for rear
#' @return A file input tag
#' @export
fileInput <- function(inputId, label, multiple = FALSE, accept = NULL, width = NULL,
                      buttonLabel = "Browse...", placeholder = "No file selected",
                      capture = NULL, ...) {

  input_attribs <- list(
    type = "file",
    id = inputId,
    name = inputId,
    class = "form-control",
    `data-input-id` = inputId
  )

  if (multiple) input_attribs$multiple <- "multiple"
  if (!is.null(accept)) input_attribs$accept <- paste(accept, collapse = ",")
  if (!is.null(capture)) input_attribs$capture <- capture

  input_tag <- tag("input", attribs = input_attribs)
  input_tag <- .apply_extra_attribs(input_tag, list(...))
  
  container_style <- if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";") else NULL
  
  tag("div",
    attribs = list(
      class = "form-group shiny-input-container",
      style = container_style
    ),
    children = list(
      if (!is.null(label) && label != "") tag("label", attribs = list(`for` = inputId, class = "form-label"), children = list(label)),
      input_tag
    )
  )
}

# ============================================================================
# Helper Functions
# ============================================================================

#' Normalize choices to named list
#'
#' @param choices Vector, list, or named vector of choices
#' @return Named list
normalizeChoices <- function(choices) {
  if (is.null(choices)) {
    return(list())
  }
  
  # If it's already a named list, return as-is
  if (is.list(choices)) {
    if (is.null(names(choices))) {
      names(choices) <- as.character(choices)
    }
    return(choices)
  }
  
  # Convert vector to named list
  if (is.vector(choices)) {
    if (is.null(names(choices))) {
      return(setNames(as.list(choices), as.character(choices)))
    } else {
      # Has names - use them
      return(setNames(as.list(choices), names(choices)))
    }
  }
  
  # Fallback
  as.list(choices)
}

#' Create an icon
#'
#' @param name Icon name (e.g., "search", "check")
#' @param class Additional CSS classes
#' @param lib Icon library: "font-awesome", "bootstrap-icons", "glyphicon"
#' @param ... Additional attributes
#' @return An icon tag
#' @export
icon <- function(name, class = NULL, lib = "font-awesome", ...) {
  extra_args <- list(...)
  
  # Build icon class based on library
  icon_class <- switch(lib,
    "font-awesome" = paste0("fa fa-", name),
    "fas" = paste0("fas fa-", name),
    "far" = paste0("far fa-", name),
    "fab" = paste0("fab fa-", name),
    "bootstrap-icons" = paste0("bi bi-", name),
    "glyphicon" = paste0("glyphicon glyphicon-", name),
    paste0("fa fa-", name)  # default to font-awesome
  )
  
  if (!is.null(class)) {
    icon_class <- paste(icon_class, class)
  }
  
  icon_attribs <- c(list(
    class = icon_class,
    `aria-hidden` = "true",
    role = "presentation"
  ), extra_args)
  
  tag("i", attribs = icon_attribs, children = list())
}
