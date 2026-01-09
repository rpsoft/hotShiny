# UI Layout Functions
# Implements all Shiny-compatible layout functions

# ============================================================================
# Page Functions
# ============================================================================

#' Create a page with fluid layout
#'
#' Creates a Bootstrap container-fluid page that fills the width of the browser.
#'
#' @param ... UI elements to include in the page
#' @param title Page title (shown in browser tab)
#' @param theme Optional theme CSS file
#' @param lang Language attribute for the HTML tag
#' @return A fluid page tag
#' @export
fluidPage <- function(..., title = NULL, theme = NULL, lang = NULL) {
  tag("div", attribs = list(class = "container-fluid"), children = list(...))
}

#' Create a row in a fluid layout
#'
#' @param ... UI elements (typically column() calls)
#' @return A fluid row tag
#' @export
fluidRow <- function(...) {
  tag("div", attribs = list(class = "row"), children = list(...))
}

#' Create a page with fixed layout
#'
#' Creates a Bootstrap container page with fixed width.
#'
#' @param ... UI elements to include in the page
#' @param title Page title
#' @param theme Optional theme CSS file
#' @param lang Language attribute
#' @return A fixed page tag
#' @export
fixedPage <- function(..., title = NULL, theme = NULL, lang = NULL) {
  tag("div", attribs = list(class = "container"), children = list(...))
}

#' Create a row in a fixed layout
#'
#' @param ... UI elements (typically column() calls)
#' @return A fixed row tag
#' @export
fixedRow <- function(...) {
  tag("div", attribs = list(class = "row"), children = list(...))
}

#' Create a page that fills the window
#'
#' @param ... UI elements
#' @param padding Padding around the page
#' @param title Page title
#' @param theme Optional theme
#' @return A fill page tag
#' @export
fillPage <- function(..., padding = 0, title = NULL, theme = NULL) {
  style <- paste0(
    "position: absolute; ",
    "top: 0; left: 0; right: 0; bottom: 0; ",
    "overflow: hidden; ",
    "padding: ", validateCssUnit(padding), ";"
  )
  
  tag("div", attribs = list(class = "fill-page", style = style), children = list(...))
}

#' Create a Bootstrap page
#'
#' Minimal Bootstrap page without extra structure.
#'
#' @param ... UI elements
#' @param title Page title
#' @param theme Optional theme
#' @return A bootstrap page tag
#' @export
bootstrapPage <- function(..., title = NULL, theme = NULL) {
  tagList(...)
}

#' Basic page (alias for bootstrapPage)
#'
#' @param ... UI elements
#' @param title Page title
#' @param theme Optional theme
#' @return A basic page tag
#' @export
basicPage <- function(..., title = NULL, theme = NULL) {
  bootstrapPage(..., title = title, theme = theme)
}

# ============================================================================
# Grid Functions
# ============================================================================

#' Create a column within a fluid row
#'
#' @param width Column width (1-12 in Bootstrap grid)
#' @param ... UI elements to include in the column
#' @param offset Number of columns to offset
#' @return A column tag
#' @export
column <- function(width, ..., offset = 0) {
  col_class <- paste0("col-md-", width)
  
  if (offset > 0) {
    col_class <- paste(col_class, paste0("offset-md-", offset))
  }
  
  tag("div", attribs = list(class = col_class), children = list(...))
}

#' Create a flexbox-based row
#'
#' @param ... UI elements
#' @param flex Flex values for children
#' @param height Row height
#' @return A fill row tag
#' @export
fillRow <- function(..., flex = 1, height = "100%") {
  children <- list(...)
  
  # Wrap each child in flex container
  if (length(flex) == 1) {
    flex <- rep(flex, length(children))
  }
  
  wrapped <- lapply(seq_along(children), function(i) {
    tag("div", 
      attribs = list(style = paste0("flex: ", flex[i], "; overflow: auto;")),
      children = list(children[[i]])
    )
  })
  
  style <- paste0(
    "display: flex; ",
    "flex-direction: row; ",
    "height: ", validateCssUnit(height), ";"
  )
  
  tag("div", attribs = list(class = "fill-row", style = style), children = wrapped)
}

#' Create a flexbox-based column
#'
#' @param ... UI elements
#' @param flex Flex values for children
#' @param width Column width
#' @return A fill column tag
#' @export
fillCol <- function(..., flex = 1, width = "100%") {
  children <- list(...)
  
  if (length(flex) == 1) {
    flex <- rep(flex, length(children))
  }
  
  wrapped <- lapply(seq_along(children), function(i) {
    tag("div",
      attribs = list(style = paste0("flex: ", flex[i], "; overflow: auto;")),
      children = list(children[[i]])
    )
  })
  
  style <- paste0(
    "display: flex; ",
    "flex-direction: column; ",
    "width: ", validateCssUnit(width), ";"
  )
  
  tag("div", attribs = list(class = "fill-col", style = style), children = wrapped)
}

#' Create a flow layout
#'
#' Elements flow left-to-right, wrapping to new lines.
#'
#' @param ... UI elements
#' @param cellArgs Additional arguments for each cell
#' @return A flow layout tag
#' @export
flowLayout <- function(..., cellArgs = list()) {
  children <- list(...)
  
  wrapped <- lapply(children, function(child) {
    cell_attribs <- c(list(
      style = "display: inline-block; vertical-align: top; margin: 5px;"
    ), cellArgs)
    tag("div", attribs = cell_attribs, children = list(child))
  })
  
  tag("div", attribs = list(class = "flow-layout"), children = wrapped)
}

#' Create a split layout
#'
#' Splits space evenly between elements.
#'
#' @param ... UI elements to split between
#' @param cellWidths Widths for each cell (vector or single value)
#' @param cellArgs Additional arguments for cells
#' @return A split layout tag
#' @export
splitLayout <- function(..., cellWidths = NULL, cellArgs = list()) {
  children <- list(...)
  n <- length(children)
  
  if (is.null(cellWidths)) {
    cellWidths <- rep(paste0(100/n, "%"), n)
  } else if (length(cellWidths) == 1) {
    cellWidths <- rep(cellWidths, n)
  }
  
  wrapped <- lapply(seq_along(children), function(i) {
    cell_attribs <- c(list(
      style = paste0("display: inline-block; vertical-align: top; width: ", 
                     validateCssUnit(cellWidths[i]), ";")
    ), cellArgs)
    tag("div", attribs = cell_attribs, children = list(children[[i]]))
  })
  
  tag("div", 
    attribs = list(class = "split-layout", style = "width: 100%; white-space: nowrap;"),
    children = wrapped
  )
}

#' Create a vertical layout
#'
#' Stacks elements vertically.
#'
#' @param ... UI elements
#' @param fluid Use fluid width?
#' @return A vertical layout tag
#' @export
verticalLayout <- function(..., fluid = TRUE) {
  children <- list(...)
  
  wrapped <- lapply(children, function(child) {
    tag("div", attribs = list(style = "margin-bottom: 1rem;"), children = list(child))
  })
  
  container_class <- if (fluid) "container-fluid" else "container"
  tag("div", attribs = list(class = container_class), children = wrapped)
}

# ============================================================================
# Panel Functions
# ============================================================================

#' Create a sidebar layout
#'
#' @param sidebarPanel Sidebar panel content
#' @param mainPanel Main panel content
#' @param position Position of sidebar: "left" or "right"
#' @param fluid Use fluid widths?
#' @return A sidebar layout tag
#' @export
sidebarLayout <- function(sidebarPanel, mainPanel, position = c("left", "right"), fluid = TRUE) {
  position <- match.arg(position)
  
  row_children <- if (position == "left") {
    list(sidebarPanel, mainPanel)
  } else {
    list(mainPanel, sidebarPanel)
  }
  
  tag("div", attribs = list(class = "row"), children = row_children)
}

#' Create a sidebar panel
#'
#' @param ... UI elements
#' @param width Width of sidebar (1-12)
#' @return A sidebar panel tag
#' @export
sidebarPanel <- function(..., width = 4) {
  tag("div",
    attribs = list(class = paste0("col-md-", width, " well")),
    children = list(
      tag("div", attribs = list(class = "sidebar-panel"), children = list(...))
    )
  )
}

#' Create a main panel
#'
#' @param ... UI elements
#' @param width Width of main panel (1-12)
#' @return A main panel tag
#' @export
mainPanel <- function(..., width = 8) {
  tag("div",
    attribs = list(class = paste0("col-md-", width)),
    children = list(
      tag("div", attribs = list(class = "main-panel"), children = list(...))
    )
  )
}

#' Create a well panel
#'
#' A panel with a gray background and rounded corners.
#'
#' @param ... UI elements
#' @return A well panel tag
#' @export
wellPanel <- function(...) {
  tag("div",
    attribs = list(
      class = "well",
      style = "background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 0.375rem; padding: 1rem; margin-bottom: 1rem;"
    ),
    children = list(...)
  )
}

#' Create an absolutely positioned panel
#'
#' @param ... UI elements
#' @param top Distance from top
#' @param left Distance from left
#' @param right Distance from right
#' @param bottom Distance from bottom
#' @param width Panel width
#' @param height Panel height
#' @param draggable Allow dragging?
#' @param fixed Use fixed positioning?
#' @param cursor Cursor style
#' @return An absolute panel tag
#' @export
absolutePanel <- function(..., top = NULL, left = NULL, right = NULL, bottom = NULL,
                          width = NULL, height = NULL, draggable = FALSE, fixed = FALSE,
                          cursor = "auto") {
  
  style_parts <- character(0)
  style_parts <- c(style_parts, paste0("position: ", if (fixed) "fixed" else "absolute"))
  style_parts <- c(style_parts, paste0("cursor: ", cursor))
  
  if (!is.null(top)) style_parts <- c(style_parts, paste0("top: ", validateCssUnit(top)))
  if (!is.null(left)) style_parts <- c(style_parts, paste0("left: ", validateCssUnit(left)))
  if (!is.null(right)) style_parts <- c(style_parts, paste0("right: ", validateCssUnit(right)))
  if (!is.null(bottom)) style_parts <- c(style_parts, paste0("bottom: ", validateCssUnit(bottom)))
  if (!is.null(width)) style_parts <- c(style_parts, paste0("width: ", validateCssUnit(width)))
  if (!is.null(height)) style_parts <- c(style_parts, paste0("height: ", validateCssUnit(height)))
  
  attribs <- list(
    class = "panel panel-default",
    style = paste(style_parts, collapse = "; ")
  )
  
  if (draggable) {
    attribs$`data-draggable` <- "true"
  }
  
  tag("div", attribs = attribs, children = list(...))
}

#' Create a fixed position panel
#'
#' @param ... UI elements
#' @param top Distance from top
#' @param left Distance from left
#' @param right Distance from right
#' @param bottom Distance from bottom
#' @param width Panel width
#' @param height Panel height
#' @param draggable Allow dragging?
#' @param cursor Cursor style
#' @return A fixed panel tag
#' @export
fixedPanel <- function(..., top = NULL, left = NULL, right = NULL, bottom = NULL,
                       width = NULL, height = NULL, draggable = FALSE, cursor = "auto") {
  absolutePanel(..., top = top, left = left, right = right, bottom = bottom,
                width = width, height = height, draggable = draggable, fixed = TRUE,
                cursor = cursor)
}

#' Create a conditional panel
#'
#' A panel that shows/hides based on a JavaScript condition.
#'
#' @param condition JavaScript expression that evaluates to TRUE/FALSE
#' @param ... UI elements
#' @param ns Namespace function for module
#' @return A conditional panel tag
#' @export
conditionalPanel <- function(condition, ..., ns = NULL) {
  if (!is.null(ns)) {
    # Replace input. and output. references with namespaced versions
    condition <- gsub("input\\.", paste0("input['", ns(""), ""), condition)
    condition <- gsub("output\\.", paste0("output['", ns(""), ""), condition)
  }
  
  tag("div",
    attribs = list(
      `data-display-if` = condition,
      `data-ns-prefix` = if (!is.null(ns)) ns("") else ""
    ),
    children = list(...)
  )
}

#' Create an input panel
#'
#' A panel for grouping inputs.
#'
#' @param ... UI elements (inputs)
#' @return An input panel tag
#' @export
inputPanel <- function(...) {
  tag("div",
    attribs = list(
      class = "shiny-input-panel panel panel-default",
      style = "padding: 10px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 0.375rem; margin-bottom: 1rem;"
    ),
    children = list(...)
  )
}

#' Create a title panel
#'
#' @param title Title text
#' @param windowTitle Browser window title
#' @return A title panel tag
#' @export
titlePanel <- function(title, windowTitle = title) {
  tag("div",
    attribs = list(class = "col-sm-12"),
    children = list(
      tag("h2", attribs = list(class = "title-panel"), children = list(title))
    )
  )
}

#' Create help text
#'
#' @param ... Text content
#' @return A help text tag
#' @export
helpText <- function(...) {
  tag("span",
    attribs = list(class = "help-block form-text text-muted"),
    children = list(...)
  )
}

# ============================================================================
# MathJax Support
# ============================================================================

#' Load MathJax and typeset math expressions
#'
#' @param ... UI elements containing math
#' @return UI with MathJax support
#' @export
withMathJax <- function(...) {
  mathjax_script <- tag("script",
    attribs = list(
      src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js",
      async = TRUE
    ),
    children = list()
  )
  
  tagList(mathjax_script, ...)
}
