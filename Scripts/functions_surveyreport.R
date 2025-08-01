# circle_badge----
# a badge with a circle shape to embed in the report
circle_badge <- function(x, color = "#4CAF50", size = "2em") {
  glue::glue('<span style="
    display: inline-block;
    background-color: {color};
    color: white;
    border-radius: 50%;
    width: {size};
    height: {size};
    line-height: {size};
    text-align: center;
    font-weight: bold;
    font-family: sans-serif;">{round(x)}</span>')
}