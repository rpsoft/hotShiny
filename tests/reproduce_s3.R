devtools::load_all(".")

# Create a tag
t <- hotShiny::div("content", id="test")
print("Tag structure:")
print(str(t))

# Try as.character
s <- as.character(t)
print("as.character result:")
print(s)

# Check if it matches HTML
expected <- '<div id="test">content</div>'
if (s == expected) {
  print("SUCCESS: Converted correctly.")
} else {
  print("FAILURE: Did not convert to HTML.")
}

# Check S3 registration
print("Is as.character.shiny.tag visible?")
tryCatch({
  m <- getS3method("as.character", "shiny.tag")
  print("Yes, found method.")
}, error = function(e) {
  print("No, method not found via getS3method.")
})
