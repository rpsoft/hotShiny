# HTML Tags System
# Foundation for all UI functions - implements Shiny-compatible tag system

#' Create an HTML tag
#'
#' @param name Tag name (e.g., "div", "span")
#' @param attribs Named list of attributes
#' @param children List of child elements
#' @param .noWS Character vector indicating where whitespace should be suppressed
#' @return A shiny.tag object
#' @export
tag <- function(name, attribs = list(), children = list(), .noWS = NULL) {
  # Ensure attribs is a list

if (!is.list(attribs)) {
    attribs <- as.list(attribs)
  }
  
  # Ensure children is a list
  if (!is.list(children)) {
    children <- list(children)
  }
  
  # Create tag structure
  result <- list(
    name = name,
    attribs = attribs,
    children = children
  )
  
  # Add .noWS if provided
  if (!is.null(.noWS)) {
    attr(result, ".noWS") <- .noWS
  }
  
  class(result) <- c("shiny.tag", "list")
  result
}

#' Create a tag function for a specific HTML element
#'
#' @param name HTML tag name
#' @return Function that creates tags of that type
makeTag <- function(name) {
  force(name)
  function(...) {
    args <- list(...)
    
    # Separate named arguments (attributes) from unnamed (children)
    arg_names <- names(args)
    
    attribs <- list()
    children <- list()
    
    if (length(args) > 0) {
      if (is.null(arg_names)) {
        # All unnamed - all are children
        children <- args
      } else {
        for (i in seq_along(args)) {
          if (is.null(arg_names[i]) || arg_names[i] == "") {
            # Unnamed argument - it's a child
            children <- c(children, list(args[[i]]))
          } else {
            # Named argument - it's an attribute
            attribs[[arg_names[i]]] <- args[[i]]
          }
        }
      }
    }
    
    tag(name, attribs, children)
  }
}

#' HTML Tags Object
#'
#' An environment containing functions for all standard HTML tags.
#' Access tags using \code{tags$tagname}, e.g., \code{tags$div()}, \code{tags$span()}.
#'
#' @export
tags <- local({
  # List of all standard HTML5 tags
  tagNames <- c(
    # Document metadata
    "head", "title", "base", "link", "meta", "style",
    # Sectioning root
    "body",
    # Content sectioning
    "address", "article", "aside", "footer", "header", "h1", "h2", "h3",
    "h4", "h5", "h6", "hgroup", "main", "nav", "section",
    # Text content
    "blockquote", "dd", "div", "dl", "dt", "figcaption", "figure", "hr",
    "li", "menu", "ol", "p", "pre", "ul",
    # Inline text semantics
    "a", "abbr", "b", "bdi", "bdo", "br", "cite", "code", "data", "dfn",
    "em", "i", "kbd", "mark", "q", "rp", "rt", "ruby", "s", "samp",
    "small", "span", "strong", "sub", "sup", "time", "u", "var", "wbr",
    # Image and multimedia
    "area", "audio", "img", "map", "track", "video",
    # Embedded content
    "embed", "iframe", "object", "param", "picture", "portal", "source",
    # SVG and MathML
    "svg", "math",
    # Scripting
    "canvas", "noscript", "script",
    # Demarcating edits
    "del", "ins",
    # Table content
    "caption", "col", "colgroup", "table", "tbody", "td", "tfoot", "th",
    "thead", "tr",
    # Forms
    "button", "datalist", "fieldset", "form", "input", "label", "legend",
    "meter", "optgroup", "option", "output", "progress", "select", "textarea",
    # Interactive elements
    "details", "dialog", "summary",
    # Web Components
    "slot", "template",
    # Deprecated but still used
    "html"
  )
  
  # Create environment with tag functions
  env <- new.env(parent = emptyenv())
  
  for (tagName in tagNames) {
    assign(tagName, makeTag(tagName), envir = env)
  }
  
  # Make it accessible via $
  class(env) <- c("shiny.tags", "environment")
  env
})

# Make tags work with $ access
`$.shiny.tags` <- function(x, name) {
  if (exists(name, envir = x, inherits = FALSE)) {
    get(name, envir = x)
  } else {
    # Create tag function on demand for unknown tags
    makeTag(name)
  }
}

#' Combine UI elements into a list
#'
#' @param ... UI elements to combine
#' @return A shiny.tag.list object
#' @export
tagList <- function(...) {
  lst <- list(...)
  class(lst) <- c("shiny.tag.list", "list")
  lst
}

#' Append attributes to a tag
#'
#' @param tag A shiny.tag object
#' @param ... Named attributes to add
#' @return Modified tag
#' @export
tagAppendAttributes <- function(tag, ...) {
  if (!inherits(tag, "shiny.tag")) {
    stop("tagAppendAttributes requires a shiny.tag object")
  }
  
  new_attribs <- list(...)
  tag$attribs <- c(tag$attribs, new_attribs)
  tag
}

#' Check if a tag has an attribute
#'
#' @param tag A shiny.tag object
#' @param attr Attribute name to check
#' @return Logical
#' @export
tagHasAttribute <- function(tag, attr) {
  if (!inherits(tag, "shiny.tag")) {
    return(FALSE)
  }
  attr %in% names(tag$attribs)
}

#' Get an attribute from a tag
#'
#' @param tag A shiny.tag object
#' @param attr Attribute name
#' @return Attribute value or NULL
#' @export
tagGetAttribute <- function(tag, attr) {
  if (!inherits(tag, "shiny.tag")) {
    return(NULL)
  }
  tag$attribs[[attr]]
}

#' Append a child to a tag
#'
#' @param tag A shiny.tag object
#' @param child Child element to append
#' @param .cssSelector Optional CSS selector (not implemented yet)
#' @return Modified tag
#' @export
tagAppendChild <- function(tag, child, .cssSelector = NULL) {
  if (!inherits(tag, "shiny.tag")) {
    stop("tagAppendChild requires a shiny.tag object")
  }
  tag$children <- c(tag$children, list(child))
  tag
}

#' Append multiple children to a tag
#'
#' @param tag A shiny.tag object
#' @param ... Children to append
#' @param .cssSelector Optional CSS selector (not implemented yet)
#' @param list Optional list of children
#' @return Modified tag
#' @export
tagAppendChildren <- function(tag, ..., .cssSelector = NULL, list = NULL) {
  if (!inherits(tag, "shiny.tag")) {
    stop("tagAppendChildren requires a shiny.tag object")
  }
  
  children <- c(list(...), list)
  tag$children <- c(tag$children, children)
  tag
}

#' Set children of a tag (replacing existing)
#'
#' @param tag A shiny.tag object
#' @param ... New children
#' @param .cssSelector Optional CSS selector (not implemented yet)
#' @param list Optional list of children
#' @return Modified tag
#' @export
tagSetChildren <- function(tag, ..., .cssSelector = NULL, list = NULL) {
  if (!inherits(tag, "shiny.tag")) {
    stop("tagSetChildren requires a shiny.tag object")
  }
  
  children <- c(list(...), list)
  tag$children <- children
  tag
}

#' Insert children at specific position
#'
#' @param tag A shiny.tag object
#' @param after Index after which to insert
#' @param ... Children to insert
#' @param .cssSelector Optional CSS selector
#' @return Modified tag
#' @export
tagInsertChildren <- function(tag, after, ..., .cssSelector = NULL) {
  if (!inherits(tag, "shiny.tag")) {
    stop("tagInsertChildren requires a shiny.tag object")
  }
  
  new_children <- list(...)
  
  if (after < 0) after <- 0
  if (after > length(tag$children)) after <- length(tag$children)
  
  if (after == 0) {
    tag$children <- c(new_children, tag$children)
  } else if (after >= length(tag$children)) {
    tag$children <- c(tag$children, new_children)
  } else {
    tag$children <- c(
      tag$children[1:after],
      new_children,
      tag$children[(after + 1):length(tag$children)]
    )
  }
  
  tag
}

#' Mark a character string as HTML
#'
#' This function marks a character string as HTML so it won't be escaped
#' when rendered.
#'
#' @param text Character string containing HTML
#' @return An object of class "html" and "character"
#' @export
HTML <- function(text) {
  if (is.null(text)) return(NULL)
  text <- as.character(text)
  class(text) <- c("html", "character")
  text
}

#' Check if object is HTML
#'
#' @param x Object to check
#' @return Logical
#' @export
is.HTML <- function(x) {
  inherits(x, "html")
}

#' Validate CSS unit
#'
#' Ensures a value is a valid CSS unit string.
#'
#' @param x Value to validate
#' @param unitType Not used (for compatibility)
#' @return Valid CSS string
#' @export
validateCssUnit <- function(x, unitType = NULL) {
  if (is.null(x) || is.na(x)) {
    return(NULL)
  }
  
  if (is.numeric(x)) {
    # Numeric without unit - assume pixels
    return(paste0(x, "px"))
  }
  
  x <- as.character(x)
  
  # Check if it's already a valid CSS unit
  # Valid patterns: number + unit, or special values like "auto", "inherit", etc.
  valid_pattern <- "^(auto|inherit|initial|unset|none|[0-9]*\\.?[0-9]+(px|em|rem|%|vh|vw|vmin|vmax|cm|mm|in|pt|pc|ex|ch|fr))$"
  
  if (grepl(valid_pattern, x, ignore.case = TRUE)) {
    return(x)
  }
  
  # Try to convert bare number to pixels
  if (grepl("^[0-9]*\\.?[0-9]+$", x)) {
    return(paste0(x, "px"))
  }
  
  # Return as-is and hope for the best
  x
}

#' Evaluate expression with tags in scope
#'
#' @param code Expression to evaluate
#' @return Result of expression
#' @export
withTags <- function(code) {
  eval(substitute(code), envir = tags, enclos = parent.frame())
}

#' Include only once
#'
#' Marks content to be included only once in the HTML output,
#' even if the function is called multiple times.
#'
#' @param x Content to mark as singleton
#' @return Singleton-marked content
#' @export
singleton <- function(x) {
  attr(x, "singleton") <- TRUE
  x
}

#' Check if object is a singleton
#'
#' @param x Object to check
#' @return Logical
#' @export
is.singleton <- function(x) {
  isTRUE(attr(x, "singleton"))
}

# ============================================================================
# Direct Tag Functions (convenience shortcuts)
# ============================================================================

#' Create a div element
#' @param ... Tag contents and attributes
#' @return A div tag
#' @export
div <- tags$div

#' Create a span element
#' @param ... Tag contents and attributes
#' @return A span tag
#' @export
span <- tags$span

#' Create a paragraph element
#' @param ... Tag contents and attributes
#' @return A p tag
#' @export
p <- tags$p

#' Create an h1 heading
#' @param ... Tag contents and attributes
#' @return An h1 tag
#' @export
h1 <- tags$h1

#' Create an h2 heading
#' @param ... Tag contents and attributes
#' @return An h2 tag
#' @export
h2 <- tags$h2

#' Create an h3 heading
#' @param ... Tag contents and attributes
#' @return An h3 tag
#' @export
h3 <- tags$h3

#' Create an h4 heading
#' @param ... Tag contents and attributes
#' @return An h4 tag
#' @export
h4 <- tags$h4

#' Create an h5 heading
#' @param ... Tag contents and attributes
#' @return An h5 tag
#' @export
h5 <- tags$h5

#' Create an h6 heading
#' @param ... Tag contents and attributes
#' @return An h6 tag
#' @export
h6 <- tags$h6

#' Create an anchor (link) element
#' @param ... Tag contents and attributes
#' @return An a tag
#' @export
a <- tags$a

#' Create a line break
#' @param ... Tag attributes
#' @return A br tag
#' @export
br <- tags$br

#' Create a horizontal rule
#' @param ... Tag attributes
#' @return An hr tag
#' @export
hr <- tags$hr

#' Create a preformatted text element
#' @param ... Tag contents and attributes
#' @return A pre tag
#' @export
pre <- tags$pre

#' Create a code element
#' @param ... Tag contents and attributes
#' @return A code tag
#' @export
code <- tags$code

#' Create an image element
#' @param ... Tag attributes (src, alt, etc.)
#' @return An img tag
#' @export
img <- tags$img

#' Create a strong (bold) element
#' @param ... Tag contents and attributes
#' @return A strong tag
#' @export
strong <- tags$strong

#' Create an emphasis (italic) element
#' @param ... Tag contents and attributes
#' @return An em tag
#' @export
em <- tags$em

#' Create a blockquote element
#' @param ... Tag contents and attributes
#' @return A blockquote tag
#' @export
blockquote <- tags$blockquote

#' Create an unordered list
#' @param ... Tag contents and attributes (li elements)
#' @return A ul tag
#' @export
ul <- tags$ul

#' Create an ordered list
#' @param ... Tag contents and attributes (li elements)
#' @return An ol tag
#' @export
ol <- tags$ol

#' Create a list item
#' @param ... Tag contents and attributes
#' @return A li tag
#' @export
li <- tags$li

# ============================================================================
# Include Functions
# ============================================================================

#' Include HTML from a file
#'
#' @param path Path to HTML file
#' @return HTML content
#' @export
includeHTML <- function(path) {
  if (!file.exists(path)) {
    warning("File not found: ", path)
    return(HTML(""))
  }
  
  content <- paste(readLines(path, warn = FALSE), collapse = "\n")
  HTML(content)
}

#' Include text from a file
#'
#' @param path Path to text file
#' @return Text content (escaped)
#' @export
includeText <- function(path) {
  if (!file.exists(path)) {
    warning("File not found: ", path)
    return("")
  }
  
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

#' Include Markdown from a file
#'
#' Renders markdown to HTML.
#'
#' @param path Path to markdown file
#' @return Rendered HTML
#' @export
includeMarkdown <- function(path) {
  if (!file.exists(path)) {
    warning("File not found: ", path)
    return(HTML(""))
  }
  
  content <- paste(readLines(path, warn = FALSE), collapse = "\n")
  
  # Try to use markdown package if available
  if (requireNamespace("markdown", quietly = TRUE)) {
    html <- markdown::markdownToHTML(text = content, fragment.only = TRUE)
    return(HTML(html))
  }
  
  # Fallback: wrap in pre tag
  tags$pre(content)
}

#' Include CSS from a file
#'
#' @param path Path to CSS file
#' @param ... Additional attributes for style tag
#' @return Style tag with CSS content
#' @export
includeCSS <- function(path, ...) {
  if (!file.exists(path)) {
    warning("CSS file not found: ", path)
    return(tags$style(type = "text/css", ""))
  }
  
  content <- paste(readLines(path, warn = FALSE), collapse = "\n")
  tags$style(type = "text/css", HTML(content), ...)
}

#' Include JavaScript from a file
#'
#' @param path Path to JavaScript file
#' @param ... Additional attributes for script tag
#' @return Script tag with JS content
#' @export
includeScript <- function(path, ...) {
  if (!file.exists(path)) {
    warning("Script file not found: ", path)
    return(tags$script(type = "text/javascript", ""))
  }
  
  content <- paste(readLines(path, warn = FALSE), collapse = "\n")
  tags$script(type = "text/javascript", HTML(content), ...)
}

# ============================================================================
# Markdown Support
# ============================================================================

#' Render inline Markdown
#'
#' @param text Markdown text
#' @param ... Additional arguments passed to markdown renderer
#' @return Rendered HTML
#' @export
markdown <- function(text, ...) {
  if (is.null(text) || text == "") {
    return(HTML(""))
  }
  
  # Try to use markdown package if available
  if (requireNamespace("markdown", quietly = TRUE)) {
    html <- markdown::markdownToHTML(text = text, fragment.only = TRUE, ...)
    return(HTML(html))
  }
  
  # Fallback: basic conversion
  # Convert headers
  text <- gsub("^### (.+)$", "<h3>\\1</h3>", text, perl = TRUE)
  text <- gsub("^## (.+)$", "<h2>\\1</h2>", text, perl = TRUE)
  text <- gsub("^# (.+)$", "<h1>\\1</h1>", text, perl = TRUE)
  
  # Convert bold and italic
  text <- gsub("\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>", text, perl = TRUE)
  text <- gsub("\\*(.+?)\\*", "<em>\\1</em>", text, perl = TRUE)
  
  # Convert links
  text <- gsub("\\[(.+?)\\]\\((.+?)\\)", "<a href=\"\\2\">\\1</a>", text, perl = TRUE)
  
  # Convert line breaks to paragraphs
  paragraphs <- strsplit(text, "\n\n+")[[1]]
  text <- paste0("<p>", paragraphs, "</p>", collapse = "")
  
  HTML(text)
}

# ============================================================================
# HTML Template Support
# ============================================================================

#' Process an HTML template
#'
#' @param filename Path to template file
#' @param ... Named values to substitute in template
#' @param document_ Wrap in full HTML document
#' @return Processed HTML
#' @export
htmlTemplate <- function(filename, ..., document_ = "auto") {
  if (!file.exists(filename)) {
    stop("Template file not found: ", filename)
  }
  
  content <- paste(readLines(filename, warn = FALSE), collapse = "\n")
  
  # Substitute {{ name }} patterns with provided values
  values <- list(...)
  for (name in names(values)) {
    pattern <- paste0("\\{\\{\\s*", name, "\\s*\\}\\}")
    value <- as.character(values[[name]])
    content <- gsub(pattern, value, content, perl = TRUE)
  }
  
  HTML(content)
}

# ============================================================================
# Dependency Management
# ============================================================================

#' Create an HTML dependency
#'
#' @param name Dependency name
#' @param version Version string
#' @param src Source directory
#' @param script JavaScript files
#' @param stylesheet CSS files
#' @param head Additional head content
#' @param attachment Attached files
#' @param package Package name
#' @param all_files Include all files
#' @return htmlDependency object
#' @export
htmlDependency <- function(name, version, src, script = NULL, stylesheet = NULL,
                           head = NULL, attachment = NULL, package = NULL,
                           all_files = TRUE) {
  dep <- list(
    name = name,
    version = version,
    src = src,
    script = script,
    stylesheet = stylesheet,
    head = head,
    attachment = attachment,
    package = package,
    all_files = all_files
  )
  class(dep) <- "html_dependency"
  dep
}

#' Bootstrap 5 library dependency
#'
#' @return HTML dependency for Bootstrap 5
#' @export
bootstrapLib <- function() {
  htmlDependency(
    name = "bootstrap",
    version = "5.3.0",
    src = "www/bootstrap5",
    stylesheet = "bootstrap.min.css",
    script = "bootstrap.bundle.min.js",
    package = "hotShiny"
  )
}

#' Suppress dependencies
#'
#' @param ... Dependency names to suppress
#' @return Object marking dependencies to suppress
#' @export
suppressDependencies <- function(...) {
  deps <- c(...)
  structure(deps, class = "suppress_dependencies")
}

# ============================================================================
# Print Methods
# ============================================================================

#' Print method for shiny.tag
#' @param x Tag object
#' @param ... Additional arguments
#' @export
print.shiny.tag <- function(x, ...) {
  cat(as.character(x), "\n")
}

#' Convert tag to character (HTML string)
#' @param x Tag object
#' @param ... Additional arguments
#' @export
as.character.shiny.tag <- function(x, ...) {
  tag_to_html(x)
}

#' Print method for shiny.tag.list
#' @param x Tag list object
#' @param ... Additional arguments
#' @export
print.shiny.tag.list <- function(x, ...) {
  cat(as.character(x), "\n")
}

#' Convert tag list to character
#' @param x Tag list object
#' @param ... Additional arguments
#' @export
as.character.shiny.tag.list <- function(x, ...) {
  paste(sapply(x, tag_to_html), collapse = "")
}

#' Convert tag or tag list to HTML string
#'
#' @param x Tag, tag list, or other content
#' @return HTML string
#' @export
tag_to_html <- function(x) {
  if (is.null(x)) {
    return("")
  }
  
  # Raw HTML
  if (inherits(x, "html")) {
    return(as.character(x))
  }
  
  # Character - escape HTML entities
  if (is.character(x) && !inherits(x, "html")) {
    return(htmlEscape(x))
  }
  
  # Tag list
  if (inherits(x, "shiny.tag.list")) {
    return(paste(sapply(x, tag_to_html), collapse = ""))
  }
  
  # Regular list (not a tag)
  if (is.list(x) && !inherits(x, "shiny.tag")) {
    if ("name" %in% names(x)) {
      # Looks like a tag structure
      return(tag_to_html_internal(x))
    }
    return(paste(sapply(x, tag_to_html), collapse = ""))
  }
  
  # Tag object
  if (inherits(x, "shiny.tag")) {
    return(tag_to_html_internal(x))
  }
  
  # Fallback: convert to string
  as.character(x)
}

#' Internal function to convert tag structure to HTML
#' @param tag Tag structure (list with name, attribs, children)
#' @return HTML string
tag_to_html_internal <- function(tag) {
  name <- tag$name
  attribs <- tag$attribs
  children <- tag$children
  
  # Build attributes string
  attr_str <- ""
  if (length(attribs) > 0 && !is.null(names(attribs))) {
    attr_parts <- character(0)
    for (i in seq_along(attribs)) {
      attr_name <- names(attribs)[i]
      attr_value <- attribs[[i]]
      
      if (!is.null(attr_name) && attr_name != "" && !is.null(attr_value)) {
        # Handle special R names that map to HTML attributes
        if (attr_name == "class_") attr_name <- "class"
        if (attr_name == "for_") attr_name <- "for"
        
        # Handle logical attributes
        if (is.logical(attr_value)) {
          if (isTRUE(attr_value)) {
            attr_parts <- c(attr_parts, attr_name)
          }
          # FALSE means omit the attribute
        } else {
          # Regular attribute with value
          escaped_value <- htmlEscape(as.character(attr_value), attribute = TRUE)
          attr_parts <- c(attr_parts, paste0(attr_name, '="', escaped_value, '"'))
        }
      }
    }
    if (length(attr_parts) > 0) {
      attr_str <- paste0(" ", paste(attr_parts, collapse = " "))
    }
  }
  
  # Self-closing tags
  void_elements <- c("area", "base", "br", "col", "embed", "hr", "img", "input",
                     "link", "meta", "param", "source", "track", "wbr")
  
  if (name %in% void_elements) {
    return(paste0("<", name, attr_str, " />"))
  }
  
  # Build children HTML
  children_html <- ""
  if (length(children) > 0) {
    children_html <- paste(sapply(children, tag_to_html), collapse = "")
  }
  
  paste0("<", name, attr_str, ">", children_html, "</", name, ">")
}

#' Escape HTML special characters
#'
#' @param text Text to escape
#' @param attribute Whether escaping for attribute value
#' @return Escaped text
#' @export
htmlEscape <- function(text, attribute = FALSE) {
  if (is.null(text)) return("")
  if (inherits(text, "html")) return(as.character(text))
  
  text <- as.character(text)
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  
  if (attribute) {
    text <- gsub('"', "&quot;", text, fixed = TRUE)
    text <- gsub("'", "&#39;", text, fixed = TRUE)
  }
  
  text
}

# ============================================================================
# Concatenation Support
# ============================================================================

#' Concatenate tags
#' @param ... Tags to concatenate
#' @export
c.shiny.tag <- function(...) {
  tagList(...)
}

#' Concatenate tag lists
#' @param ... Tag lists to concatenate
#' @export
c.shiny.tag.list <- function(...) {
  result <- list()
  for (item in list(...)) {
    if (inherits(item, "shiny.tag.list")) {
      result <- c(result, as.list(item))
    } else {
      result <- c(result, list(item))
    }
  }
  class(result) <- c("shiny.tag.list", "list")
  result
}
