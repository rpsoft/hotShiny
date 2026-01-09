# Dynamic UI Functions
# Insert, remove, and manipulate UI elements dynamically

# ============================================================================
# Insert and Remove UI
# ============================================================================

#' Insert UI elements into the page
#'
#' @param selector CSS selector identifying where to insert
#' @param where Position: "beforeBegin", "afterBegin", "beforeEnd", "afterEnd"
#' @param ui UI element to insert
#' @param multiple Insert after all matches (TRUE) or just first (FALSE)
#' @param immediate Insert immediately (TRUE) or wait for flush (FALSE)
#' @param session Shiny session object
#' @return NULL (called for side effect)
#' @export
insertUI <- function(selector, where = c("beforeBegin", "afterBegin", "beforeEnd", "afterEnd"),
                     ui, multiple = FALSE, immediate = FALSE,
                     session = getDefaultReactiveDomain()) {
  where <- match.arg(where)
  
  # Convert UI to HTML
  html <- if (inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list")) {
    tag_to_html(ui)
  } else {
    as.character(ui)
  }
  
  if (!is.null(session)) {
    session$sendCustomMessage("shiny-insert-ui", list(
      selector = selector,
      where = where,
      html = html,
      multiple = multiple,
      immediate = immediate
    ))
  }
  
  invisible(NULL)
}

#' Remove UI elements from the page
#'
#' @param selector CSS selector identifying elements to remove
#' @param multiple Remove all matches (TRUE) or just first (FALSE)
#' @param immediate Remove immediately (TRUE) or wait for flush (FALSE)
#' @param session Shiny session object
#' @return NULL (called for side effect)
#' @export
removeUI <- function(selector, multiple = FALSE, immediate = FALSE,
                     session = getDefaultReactiveDomain()) {
  if (!is.null(session)) {
    session$sendCustomMessage("shiny-remove-ui", list(
      selector = selector,
      multiple = multiple,
      immediate = immediate
    ))
  }
  
  invisible(NULL)
}

# ============================================================================
# Tab Manipulation
# ============================================================================

#' Insert a tab into a tabset panel
#'
#' @param inputId Tabset panel ID
#' @param tab Tab panel to insert (from tabPanel())
#' @param target ID of tab to insert relative to
#' @param position Position relative to target: "before" or "after"
#' @param select Select the new tab?
#' @param session Shiny session object
#' @return NULL (called for side effect)
#' @export
insertTab <- function(inputId, tab, target = NULL, position = c("after", "before"),
                      select = FALSE, session = getDefaultReactiveDomain()) {
  position <- match.arg(position)
  
  # Extract tab info
  tab_title <- attr(tab, "title")
  tab_value <- attr(tab, "value") %||% tab_title
  tab_icon <- attr(tab, "icon")
  tab_html <- tag_to_html(tab)
  
  if (!is.null(session)) {
    session$sendCustomMessage("shiny-insert-tab", list(
      inputId = inputId,
      tab = list(
        title = tab_title,
        value = tab_value,
        icon = if (!is.null(tab_icon)) tag_to_html(tab_icon) else NULL,
        content = tab_html
      ),
      target = target,
      position = position,
      select = select
    ))
  }
  
  invisible(NULL)
}

#' Prepend a tab to a tabset panel
#'
#' @param inputId Tabset panel ID
#' @param tab Tab panel to prepend
#' @param select Select the new tab?
#' @param session Shiny session object
#' @return NULL (called for side effect)
#' @export
prependTab <- function(inputId, tab, select = FALSE, session = getDefaultReactiveDomain()) {
  insertTab(inputId, tab, target = NULL, position = "before", select = select, session = session)
}

#' Append a tab to a tabset panel
#'
#' @param inputId Tabset panel ID
#' @param tab Tab panel to append
#' @param select Select the new tab?
#' @param session Shiny session object
#' @return NULL (called for side effect)
#' @export
appendTab <- function(inputId, tab, select = FALSE, session = getDefaultReactiveDomain()) {
  insertTab(inputId, tab, target = NULL, position = "after", select = select, session = session)
}

#' Remove a tab from a tabset panel
#'
#' @param inputId Tabset panel ID
#' @param target ID or value of tab to remove
#' @param session Shiny session object
#' @return NULL (called for side effect)
#' @export
removeTab <- function(inputId, target, session = getDefaultReactiveDomain()) {
  if (!is.null(session)) {
    session$sendCustomMessage("shiny-remove-tab", list(
      inputId = inputId,
      target = target
    ))
  }
  
  invisible(NULL)
}

#' Show a hidden tab
#'
#' @param inputId Tabset panel ID
#' @param target ID or value of tab to show
#' @param select Select the tab after showing?
#' @param session Shiny session object
#' @return NULL (called for side effect)
#' @export
showTab <- function(inputId, target, select = FALSE, session = getDefaultReactiveDomain()) {
  if (!is.null(session)) {
    session$sendCustomMessage("shiny-show-tab", list(
      inputId = inputId,
      target = target,
      select = select
    ))
  }
  
  invisible(NULL)
}

#' Hide a tab
#'
#' @param inputId Tabset panel ID
#' @param target ID or value of tab to hide
#' @param session Shiny session object
#' @return NULL (called for side effect)
#' @export
hideTab <- function(inputId, target, session = getDefaultReactiveDomain()) {
  if (!is.null(session)) {
    session$sendCustomMessage("shiny-hide-tab", list(
      inputId = inputId,
      target = target
    ))
  }
  
  invisible(NULL)
}

# ============================================================================
# Helper
# ============================================================================

#' Null-coalescing operator
#' @param x Value to check
#' @param y Default value
#' @return x if not NULL, otherwise y
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
