# UI Output Functions
# Implements all Shiny-compatible output functions

# ============================================================================
# Text Outputs
# ============================================================================

#' Create a text output element
#'
#' @param outputId Output variable to read the value from
#' @param container Container tag function
#' @param inline Use inline element (span) instead of block (div)?
#' @return A text output tag
#' @export
textOutput <- function(outputId, container = if (inline) span else div, inline = FALSE) {
  tag_name <- if (inline) "span" else "div"
  
  tag(tag_name, attribs = list(
    id = outputId,
    class = "shiny-text-output",
    `data-output-id` = outputId
  ), children = list())
}

#' Create a verbatim text output element
#'
#' Displays text output in a fixed-width font, preserving whitespace.
#'
#' @param outputId Output variable to read the value from
#' @param placeholder Display placeholder when empty?
#' @return A verbatim text output tag
#' @export
verbatimTextOutput <- function(outputId, placeholder = FALSE) {
  pre_class <- "shiny-text-output"
  if (placeholder) {
    pre_class <- paste(pre_class, "noplaceholder")
  }
  
  tag("pre", attribs = list(
    id = outputId,
    class = pre_class,
    `data-output-id` = outputId
  ), children = list())
}

#' Create an HTML output element
#'
#' Renders arbitrary HTML from the server.
#'
#' @param outputId Output variable to read the value from
#' @param inline Use inline element?
#' @param container Container function
#' @param fill Whether to allow filling
#' @param ... Additional attributes
#' @return An HTML output tag
#' @export
htmlOutput <- function(outputId, inline = FALSE, container = if (inline) span else div,
                       fill = FALSE, ...) {
  tag_name <- if (inline) "span" else "div"
  
  attribs <- list(
    id = outputId,
    class = "shiny-html-output",
    `data-output-id` = outputId
  )
  
  extra_args <- list(...)
  attribs <- c(attribs, extra_args)
  
  tag(tag_name, attribs = attribs, children = list())
}

#' Create a UI output element
#'
#' Alias for htmlOutput for rendering UI elements.
#'
#' @param outputId Output variable to read the value from
#' @param inline Use inline element?
#' @param container Container function
#' @param fill Whether to allow filling
#' @param ... Additional attributes
#' @return A UI output tag
#' @export
uiOutput <- function(outputId, inline = FALSE, container = if (inline) span else div,
                     fill = FALSE, ...) {
  htmlOutput(outputId, inline = inline, container = container, fill = fill, ...)
}

# ============================================================================
# Plot and Image Outputs
# ============================================================================

#' Create a plot output element
#'
#' @param outputId Output variable for the plot
#' @param width Width of the plot
#' @param height Height of the plot
#' @param click Click event options (NULL or clickOpts)
#' @param dblclick Double-click event options
#' @param hover Hover event options
#' @param brush Brush event options
#' @param inline Display inline?
#' @return A plot output tag
#' @export
plotOutput <- function(outputId, width = "100%", height = "400px",
                       click = NULL, dblclick = NULL, hover = NULL, brush = NULL,
                       inline = FALSE) {
  
  style_parts <- c(
    paste0("width: ", validateCssUnit(width)),
    paste0("height: ", validateCssUnit(height))
  )
  if (inline) {
    style_parts <- c(style_parts, "display: inline-block")
  }
  
  attribs <- list(
    id = outputId,
    class = "shiny-plot-output",
    style = paste(style_parts, collapse = "; "),
    `data-output-id` = outputId
  )
  
  # Add interaction data attributes
  if (!is.null(click)) {
    if (is.character(click)) {
      attribs$`data-click-id` <- click
    } else if (is.list(click)) {
      attribs$`data-click-id` <- click$id
    }
  }
  if (!is.null(dblclick)) {
    if (is.character(dblclick)) {
      attribs$`data-dblclick-id` <- dblclick
    } else if (is.list(dblclick)) {
      attribs$`data-dblclick-id` <- dblclick$id
    }
  }
  if (!is.null(hover)) {
    if (is.character(hover)) {
      attribs$`data-hover-id` <- hover
    } else if (is.list(hover)) {
      attribs$`data-hover-id` <- hover$id
    }
  }
  if (!is.null(brush)) {
    if (is.character(brush)) {
      attribs$`data-brush-id` <- brush
    } else if (is.list(brush)) {
      attribs$`data-brush-id` <- brush$id
    }
  }
  
  tag("div", attribs = attribs, children = list())
}

#' Create an image output element
#'
#' @param outputId Output variable for the image
#' @param width Width of the image
#' @param height Height of the image
#' @param click Click event options
#' @param dblclick Double-click event options
#' @param hover Hover event options
#' @param brush Brush event options
#' @param inline Display inline?
#' @return An image output tag
#' @export
imageOutput <- function(outputId, width = "100%", height = "400px",
                        click = NULL, dblclick = NULL, hover = NULL, brush = NULL,
                        inline = FALSE) {
  
  style_parts <- c(
    paste0("width: ", validateCssUnit(width)),
    paste0("height: ", validateCssUnit(height))
  )
  if (inline) {
    style_parts <- c(style_parts, "display: inline-block")
  }
  
  attribs <- list(
    id = outputId,
    class = "shiny-image-output",
    style = paste(style_parts, collapse = "; "),
    `data-output-id` = outputId
  )
  
  # Add interaction data attributes (same as plotOutput)
  if (!is.null(click)) {
    attribs$`data-click-id` <- if (is.character(click)) click else click$id
  }
  if (!is.null(dblclick)) {
    attribs$`data-dblclick-id` <- if (is.character(dblclick)) dblclick else dblclick$id
  }
  if (!is.null(hover)) {
    attribs$`data-hover-id` <- if (is.character(hover)) hover else hover$id
  }
  if (!is.null(brush)) {
    attribs$`data-brush-id` <- if (is.character(brush)) brush else brush$id
  }
  
  tag("div", attribs = attribs, children = list())
}

# ============================================================================
# Table Outputs
# ============================================================================

#' Create a table output element
#'
#' @param outputId Output variable for the table
#' @return A table output tag
#' @export
tableOutput <- function(outputId) {
  tag("div", attribs = list(
    id = outputId,
    class = "shiny-table-output table-responsive",
    `data-output-id` = outputId
  ), children = list())
}

#' Create a DataTables output element
#'
#' @param outputId Output variable for the DataTable
#' @return A DataTable output tag
#' @export
dataTableOutput <- function(outputId) {
  tag("div", attribs = list(
    id = outputId,
    class = "shiny-datatable-output",
    `data-output-id` = outputId
  ), children = list())
}

# ============================================================================
# Download Outputs
# ============================================================================
#' Create a download button
#'
#' @param outputId Output variable bound to downloadHandler
#' @param label Button label
#' @param class CSS class for button
#' @param icon Optional icon
#' @param ... Additional attributes
#' @return A download button tag
#' @export
downloadButton <- function(outputId, label = "Download", class = NULL, icon = NULL, ...) {
  btn_class <- paste("btn btn-primary shiny-download-link", class)
  
  extra_args <- list(...)
  
  btn_attribs <- c(list(
    id = outputId,
    class = btn_class,
    href = "",
    target = "_blank",
    download = NA,
    `data-output-id` = outputId
  ), extra_args)
  
  children <- list()
  if (!is.null(icon)) {
    children <- c(children, list(icon, " "))
  } else {
    # Default download icon
    children <- c(children, list(
      tag("i", attribs = list(class = "bi bi-download", `aria-hidden` = "true"), children = list()),
      " "
    ))
  }
  children <- c(children, list(label))
  
  tag("a", attribs = btn_attribs, children = children)
}

#' Create a download link
#'
#' @param outputId Output variable bound to downloadHandler
#' @param label Link label
#' @param class CSS class for link
#' @param icon Optional icon
#' @param ... Additional attributes
#' @return A download link tag
#' @export
downloadLink <- function(outputId, label = "Download", class = NULL, icon = NULL, ...) {
  link_class <- paste("shiny-download-link", class)
  
  extra_args <- list(...)
  
  link_attribs <- c(list(
    id = outputId,
    class = link_class,
    href = "",
    target = "_blank",
    download = NA,
    `data-output-id` = outputId
  ), extra_args)
  
  children <- list()
  if (!is.null(icon)) {
    children <- c(children, list(icon, " "))
  }
  children <- c(children, list(label))
  
  tag("a", attribs = link_attribs, children = children)
}

# ============================================================================
# Output Options
# ============================================================================

#' Set options for an output object
#'
#' @param x Render function output
#' @param name Name of the option
#' @param ... Option values
#' @return Modified output with options
#' @export
outputOptions <- function(x, name, ...) {
  # Store options as attributes
  if (is.null(attr(x, "outputOptions"))) {
    attr(x, "outputOptions") <- list()
  }
  
  options <- attr(x, "outputOptions")
  args <- list(...)
  
  if (length(args) > 0) {
    options[[name]] <- args[[1]]
  }
  
  attr(x, "outputOptions") <- options
  x
}
