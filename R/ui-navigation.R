# UI Navigation Functions
# Implements tabs, navbars, and navlists

# ============================================================================
# Tabset Panel
# ============================================================================

#' Create a tabset panel
#'
#' @param ... Tab panels created with tabPanel()
#' @param id Optional id for the tabset (allows programmatic control)
#' @param selected Value of initially selected tab
#' @param type Visual type: "tabs", "pills", or "hidden"
#' @param vertical Stack tabs vertically?
#' @param header Header content above tabs
#' @param footer Footer content below tabs
#' @return A tabset panel tag
#' @export
tabsetPanel <- function(..., id = NULL, selected = NULL, type = c("tabs", "pills", "hidden"),
                        vertical = FALSE, header = NULL, footer = NULL) {
  type <- match.arg(type)
  tabs <- list(...)
  
  # Generate random ID if not provided
  if (is.null(id)) {
    id <- paste0("tabset_", sample.int(100000, 1))
  }
  
  # Extract tab info
  tab_info <- lapply(tabs, function(tab) {
    list(
      title = attr(tab, "title"),
      value = attr(tab, "value") %||% attr(tab, "title"),
      icon = attr(tab, "icon"),
      content = tab
    )
  })
  
  # Determine selected tab
  if (is.null(selected) && length(tab_info) > 0) {
    selected <- tab_info[[1]]$value
  }
  
  # Build nav items
  nav_items <- lapply(tab_info, function(info) {
    is_active <- info$value == selected
    
    btn_attribs <- list(
      class = paste("nav-link", if (is_active) "active" else ""),
      id = paste0(id, "-", info$value, "-tab"),
      `data-bs-toggle` = if (type != "hidden") "tab" else NULL,
      `data-bs-target` = paste0("#", id, "-", info$value),
      type = "button",
      role = "tab",
      `aria-controls` = paste0(id, "-", info$value),
      `aria-selected` = if (is_active) "true" else "false"
    )
    
    btn_children <- list()
    if (!is.null(info$icon)) {
      btn_children <- c(btn_children, list(info$icon, " "))
    }
    btn_children <- c(btn_children, list(info$title))
    
    tag("li", attribs = list(class = "nav-item", role = "presentation"),
      children = list(
        tag("button", attribs = btn_attribs, children = btn_children)
      )
    )
  })
  
  # Build tab panes
  tab_panes <- lapply(tab_info, function(info) {
    is_active <- info$value == selected
    
    tag("div",
      attribs = list(
        class = paste("tab-pane fade", if (is_active) "show active" else ""),
        id = paste0(id, "-", info$value),
        role = "tabpanel",
        `aria-labelledby` = paste0(id, "-", info$value, "-tab"),
        tabindex = "0"
      ),
      children = list(info$content)
    )
  })
  
  # Build nav class
  nav_class <- switch(type,
    "tabs" = "nav nav-tabs",
    "pills" = "nav nav-pills",
    "hidden" = "nav d-none"
  )
  if (vertical) {
    nav_class <- paste(nav_class, "flex-column")
  }
  
  # Assemble
  children <- list()
  if (!is.null(header)) {
    children <- c(children, list(header))
  }
  
  children <- c(children, list(
    tag("ul",
      attribs = list(class = nav_class, id = id, role = "tablist", `data-input-id` = id),
      children = nav_items
    ),
    tag("div", attribs = list(class = "tab-content"), children = tab_panes)
  ))
  
  if (!is.null(footer)) {
    children <- c(children, list(footer))
  }
  
  tag("div", attribs = list(class = "shiny-tabset"), children = children)
}

#' Create a tab panel
#'
#' @param title Tab title
#' @param ... Tab content
#' @param value Value used to identify tab
#' @param icon Optional icon
#' @return A tab panel tag with metadata
#' @export
tabPanel <- function(title, ..., value = title, icon = NULL) {
  content <- tag("div", attribs = list(), children = list(...))
  attr(content, "title") <- title
  attr(content, "value") <- value
  attr(content, "icon") <- icon
  class(content) <- c("shiny.tab", class(content))
  content
}

#' Create a tab panel body (content only)
#'
#' @param value Value identifying this tab
#' @param ... Tab content
#' @return A tab panel body tag
#' @export
tabPanelBody <- function(value, ...) {
  tag("div",
    attribs = list(
      class = "tab-pane fade",
      id = value,
      `data-value` = value
    ),
    children = list(...)
  )
}

# ============================================================================
# Navbar Page
# ============================================================================

#' Create a page with a top-level navigation bar
#'
#' @param title Application title
#' @param ... Tab panels or navbarMenu items
#' @param id Optional id for the navbar
#' @param selected Initially selected tab
#' @param position Navbar position: "static-top", "fixed-top", "fixed-bottom"
#' @param header Header content
#' @param footer Footer content
#' @param inverse Use dark theme?
#' @param collapsible Collapse on small screens?
#' @param fluid Use fluid container?
#' @param responsive Enable responsive behavior?
#' @param theme Custom theme
#' @param windowTitle Browser window title
#' @param lang Language code
#' @return A navbar page tag
#' @export
navbarPage <- function(title, ..., id = NULL, selected = NULL,
                       position = c("static-top", "fixed-top", "fixed-bottom"),
                       header = NULL, footer = NULL, inverse = FALSE,
                       collapsible = TRUE, fluid = TRUE, responsive = TRUE,
                       theme = NULL, windowTitle = title, lang = NULL) {
  
  position <- match.arg(position)
  tabs <- list(...)
  
  # Generate ID
  if (is.null(id)) {
    id <- paste0("navbar_", sample.int(100000, 1))
  }
  
  # Navbar classes
  navbar_class <- "navbar navbar-expand-lg"
  navbar_class <- paste(navbar_class, if (inverse) "navbar-dark bg-dark" else "navbar-light bg-light")
  
  if (position == "fixed-top") {
    navbar_class <- paste(navbar_class, "fixed-top")
  } else if (position == "fixed-bottom") {
    navbar_class <- paste(navbar_class, "fixed-bottom")
  }
  
  # Process tabs
  nav_items <- list()
  tab_contents <- list()
  
  for (i in seq_along(tabs)) {
    item <- tabs[[i]]
    
    if (inherits(item, "shiny.navbarmenu")) {
      # Dropdown menu
      nav_items <- c(nav_items, list(item))
    } else if (inherits(item, "shiny.tab")) {
      # Regular tab
      tab_title <- attr(item, "title")
      tab_value <- attr(item, "value") %||% tab_title
      tab_icon <- attr(item, "icon")
      
      is_active <- if (is.null(selected) && i == 1) TRUE else tab_value == selected
      
      link_children <- list()
      if (!is.null(tab_icon)) {
        link_children <- c(link_children, list(tab_icon, " "))
      }
      link_children <- c(link_children, list(tab_title))
      
      nav_items <- c(nav_items, list(
        tag("li", attribs = list(class = "nav-item"),
          children = list(
            tag("a",
              attribs = list(
                class = paste("nav-link", if (is_active) "active" else ""),
                href = paste0("#", id, "-", tab_value),
                `data-bs-toggle` = "tab"
              ),
              children = link_children
            )
          )
        )
      ))
      
      tab_contents <- c(tab_contents, list(
        tag("div",
          attribs = list(
            class = paste("tab-pane fade", if (is_active) "show active" else ""),
            id = paste0(id, "-", tab_value)
          ),
          children = list(item)
        )
      ))
    } else {
      # Other content (e.g., text)
      nav_items <- c(nav_items, list(item))
    }
  }
  
  # Navbar brand
  brand <- tag("a", attribs = list(class = "navbar-brand", href = "#"), children = list(title))
  
  # Navbar toggler (for mobile)
  toggler <- tag("button",
    attribs = list(
      class = "navbar-toggler",
      type = "button",
      `data-bs-toggle` = "collapse",
      `data-bs-target` = paste0("#", id, "-collapse"),
      `aria-controls` = paste0(id, "-collapse"),
      `aria-expanded` = "false",
      `aria-label` = "Toggle navigation"
    ),
    children = list(
      tag("span", attribs = list(class = "navbar-toggler-icon"), children = list())
    )
  )
  
  # Collapsible content
  collapse <- tag("div",
    attribs = list(class = "collapse navbar-collapse", id = paste0(id, "-collapse")),
    children = list(
      tag("ul", attribs = list(class = "navbar-nav me-auto mb-2 mb-lg-0", id = id), children = nav_items)
    )
  )
  
  # Build navbar
  container_class <- if (fluid) "container-fluid" else "container"
  
  navbar <- tag("nav",
    attribs = list(class = navbar_class),
    children = list(
      tag("div", attribs = list(class = container_class),
        children = list(brand, toggler, collapse)
      )
    )
  )
  
  # Build page content
  content_children <- list()
  if (!is.null(header)) {
    content_children <- c(content_children, list(header))
  }
  content_children <- c(content_children, list(
    tag("div", attribs = list(class = "tab-content"), children = tab_contents)
  ))
  if (!is.null(footer)) {
    content_children <- c(content_children, list(footer))
  }
  
  content <- tag("div", attribs = list(class = container_class), children = content_children)
  
  # Assemble page
  page_style <- NULL
  if (position == "fixed-top") {
    page_style <- "padding-top: 70px;"
  } else if (position == "fixed-bottom") {
    page_style <- "padding-bottom: 70px;"
  }
  
  tag("div", attribs = list(class = "shiny-navbar-page", style = page_style),
    children = list(navbar, content)
  )
}

#' Create a dropdown menu for navbar
#'
#' @param title Menu title
#' @param ... Tab panels or menu items
#' @param menuName Internal name for menu
#' @param icon Optional icon
#' @return A navbar menu tag
#' @export
navbarMenu <- function(title, ..., menuName = title, icon = NULL) {
  items <- list(...)
  
  dropdown_items <- lapply(items, function(item) {
    if (inherits(item, "shiny.tab")) {
      tab_title <- attr(item, "title")
      tab_value <- attr(item, "value") %||% tab_title
      
      tag("li", children = list(
        tag("a",
          attribs = list(class = "dropdown-item", href = paste0("#", tab_value), `data-bs-toggle` = "tab"),
          children = list(tab_title)
        )
      ))
    } else if (identical(item, "----")) {
      tag("li", children = list(tag("hr", attribs = list(class = "dropdown-divider"), children = list())))
    } else {
      tag("li", children = list(item))
    }
  })
  
  title_children <- list()
  if (!is.null(icon)) {
    title_children <- c(title_children, list(icon, " "))
  }
  title_children <- c(title_children, list(title))
  
  menu <- tag("li", attribs = list(class = "nav-item dropdown"),
    children = list(
      tag("a",
        attribs = list(
          class = "nav-link dropdown-toggle",
          href = "#",
          role = "button",
          `data-bs-toggle` = "dropdown",
          `aria-expanded` = "false"
        ),
        children = title_children
      ),
      tag("ul", attribs = list(class = "dropdown-menu"), children = dropdown_items)
    )
  )
  
  class(menu) <- c("shiny.navbarmenu", class(menu))
  attr(menu, "menuName") <- menuName
  menu
}

# ============================================================================
# Navigation List Panel
# ============================================================================

#' Create a navigation list panel
#'
#' Vertical navigation with content panels.
#'
#' @param ... Tab panels
#' @param id Optional id
#' @param selected Initially selected tab
#' @param well Display in well panel?
#' @param fluid Use fluid layout?
#' @param widths Widths for nav and content columns
#' @return A navlist panel tag
#' @export
navlistPanel <- function(..., id = NULL, selected = NULL, well = TRUE,
                         fluid = TRUE, widths = c(4, 8)) {
  tabs <- list(...)
  
  if (is.null(id)) {
    id <- paste0("navlist_", sample.int(100000, 1))
  }
  
  # Process tabs
  nav_items <- list()
  tab_panes <- list()
  
  for (i in seq_along(tabs)) {
    item <- tabs[[i]]
    
    if (is.character(item)) {
      # Header text
      nav_items <- c(nav_items, list(
        tag("li", attribs = list(class = "nav-header h6 text-muted"), children = list(item))
      ))
    } else if (inherits(item, "shiny.tab")) {
      tab_title <- attr(item, "title")
      tab_value <- attr(item, "value") %||% tab_title
      tab_icon <- attr(item, "icon")
      
      is_active <- if (is.null(selected) && length(tab_panes) == 0) TRUE else tab_value == selected
      
      link_children <- list()
      if (!is.null(tab_icon)) {
        link_children <- c(link_children, list(tab_icon, " "))
      }
      link_children <- c(link_children, list(tab_title))
      
      nav_items <- c(nav_items, list(
        tag("a",
          attribs = list(
            class = paste("nav-link", if (is_active) "active" else ""),
            id = paste0(id, "-", tab_value, "-tab"),
            `data-bs-toggle` = "pill",
            href = paste0("#", id, "-", tab_value),
            role = "tab"
          ),
          children = link_children
        )
      ))
      
      tab_panes <- c(tab_panes, list(
        tag("div",
          attribs = list(
            class = paste("tab-pane fade", if (is_active) "show active" else ""),
            id = paste0(id, "-", tab_value),
            role = "tabpanel"
          ),
          children = list(item)
        )
      ))
    }
  }
  
  # Navigation column
  nav_col_class <- paste0("col-md-", widths[1])
  nav_col_style <- if (well) "background-color: #f8f9fa; border-radius: 0.375rem; padding: 1rem;" else NULL
  
  nav_col <- tag("div",
    attribs = list(class = nav_col_class, style = nav_col_style),
    children = list(
      tag("nav",
        attribs = list(class = "nav nav-pills flex-column", id = id, role = "tablist", `data-input-id` = id),
        children = nav_items
      )
    )
  )
  
  # Content column
  content_col <- tag("div",
    attribs = list(class = paste0("col-md-", widths[2])),
    children = list(
      tag("div", attribs = list(class = "tab-content"), children = tab_panes)
    )
  )
  
  tag("div", attribs = list(class = "row shiny-navlist"), children = list(nav_col, content_col))
}

# ============================================================================
# Helper
# ============================================================================

#' Null-coalescing operator
#' @param x Value to check
#' @param y Default value if x is NULL
#' @return x if not NULL, otherwise y
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
