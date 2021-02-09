#' Generates (pseudo)random strings of the specified char length
#'
#' @param char A integer, the number of chars to include in the output string.
#' @return A string.
#' @examples
#' sampleStr(10)
sampleStr <- function(char){

  x <- c()

  for(i in 1:char){
    x <- c(x, sample(c(letters, LETTERS, 0:9), 1))
  }

  return(
    paste0(x, collapse = "")
  )
}

#' Extract the values from each entry in a list of vectors at a specific index
#'
#' @param x A list, each item of the list should have equal length.
#' @param pos A integer, the position to extract from each entry in the list.
#' @return A list.
#' @examples
#' listExtract(list(col1 = c(1, 2, 3, 4, 5), col2 = c("a", "b", "c", "d", "e")), 3)
listExtract <- function(x, pos){
  return(
    lapply(x, `[[`, pos)
  )
}

#' Create a HTML document preconfigured to load Bootstrap5 from the CDN
#'
#' @param head A string of HTML that will be passed to the html5::head() function.
#' @param body A string of HTML that will be passed to the html5::body() function
#' @return A string of HTML.
#' @examples
#' bs_doc(
#' body = div(h1("Hello"), p("Welcome to this page."))
#' )
bs_doc <- function(
  head = NULL,
  body
){
  return(
    html_doc(
      html(
        lang = "en",
        html5::head(
          meta(
            charset = "utf-8"
          ),
          meta(
            name = "viewport",
            content = "width=device-width, initial-scale=1"
          ),
          link(
            href = "https://cdn.jsdelivr.net/npm/bootstrap@5.0.0-beta1/dist/css/bootstrap.min.css",
            rel = "stylesheet",
            integrity = "sha384-giJF6kkoqNQ00vy+HMDP7azOuL0xtbfIcaT9wjKHr8RbDVddVHyTfAAsrekwKmP1",
            crossorigin = "anonymous"
          ),
          head
        ),
        html5::body(
          body,
          script(
            src = "https://cdn.jsdelivr.net/npm/bootstrap@5.0.0-beta1/dist/js/bootstrap.bundle.min.js",
            integrity = "sha384-ygbV9kiqUc6oa4msXn9868pTtWMgiQaeYH7/t7LECLbyPA2x65Kgf80OJFdroafW",
            crossorigin = "anonymous"
          )
        )
      )
    )
  )
}

#' Create a Bootstrap container div
#'
#' @description Create a Bootstrap container div.
#' @param ... A string or strings of HTML content to include in the container div.
#' @param fluid TRUE/FALSE, if TRUE, the class becomes "container-fluid" which sets the width to 100\%.
#' @param breakpoint A string, sets the break-point for reshaping content based on screen size. One of "sm", "md", "lg", "xl", "xxl".
#' @param padding An integer, the padding to add, measured in px.
#' @param overflow_hidden TRUE/FALSE, if TRUE, adds "overflow-hidden" to the container class
#' @param custom_class A string, allows custom classes to be appended to the container class
#' @return A string of HTML.
#' @examples
#' bs_container(
#' div(),
#' fluid = TRUE
#' )
bs_container <- function(
  ...,
  fluid = FALSE,
  breakpoint = NULL,
  padding = NULL,
  overflow_hidden = FALSE,
  custom_class = NULL
){

  x <- "container"

  if(fluid == TRUE){
    x <- paste0(x, "-fluid")
  }else if(is.null(breakpoint) == FALSE){
    x <- paste0(x, "-", breakpoint)
  }

  if(is.null(padding) == FALSE){
    x <- paste0(x, " px-", padding)
  }

  if(overflow_hidden == TRUE){
    x <- paste0(x, "overflow-hidden")
  }

  if(is.null(custom_class) == FALSE){
    x <- paste0(x, " ", paste0(custom_class, collapse = " "))
  }

  return(
    div(
      class = x,
      ...
    )
  )
}

#' Create a Bootstrap row div
#'
#' @param ... A string or strings of HTML content to include in the row div.
#' @param row_cols An integer, the number of columns to use for rendering content.
#' @param align_items A string, likely one of "start", "center", or "end", that gets appended to "align-items-".
#' @param justify_content A string, such as "md-center", that gets appended to "justify-content-".
#' @param horizontal_gutter An integer, controls the horizontal gutter width, gets appended to "gx-".
#' @param vertical_gutter An integer, controls the vertical gutter width, gets appended to "gy-".
#' @param gutter An integer, controls the horizontal and vertical gutter width, gets appended to "g-".
#' @param custom_class A string, allows custom classes to be appended to the row class
#' @return A string of HTML.
#' @examples
#' bs_row(
#' bs_col(p("Col 1")),
#' bs_col(p("Col 2"))
#' )
bs_row <- function(
  ...,
  row_cols = NULL,
  align_items = NULL,
  justify_content = NULL,
  horizontal_gutter = NULL,
  vertical_gutter = NULL,
  gutter = NULL,
  custom_class = NULL
){

  x <- "row"

  if(is.null(row_cols) == FALSE){
    x <- paste0(x, " ", paste0("row-cols-", row_cols, collapse = " "))
  }

  if(is.null(align_items) == FALSE){
    x <- paste0(x, " ", "align-items-", align_items)
  }

  if(is.null(justify_content) == FALSE){
    x <- paste0(x, " ", "justify-content-", justify_content)
  }

  if(is.null(horizontal_gutter) == FALSE){
    x <- paste0(x, " ", "gx-", horizontal_gutter)
  }

  if(is.null(vertical_gutter) == FALSE){
    x <- paste0(x, " ", "gy-", vertical_gutter)
  }

  if(is.null(gutter) == FALSE){
    x <- paste0(x, " ", "g-", gutter)
  }

  if(is.null(custom_class) == FALSE){
    x <- paste0(x, " ", paste0(custom_class, collapse = " "))
  }

  return(
    div(
      class = x,
      ...
    )
  )
}

#' Create a Bootstrap col div
#'
#' @param ... A string or strings of HTML content to include in the col div.
#' @param size An integer, 1-12, determines how many column units a particular column takes up.
#' @param align_self A string, likely one of "start", "center", or "end", that gets appended to "align-self-".
#' @param order An integer, sets the order of the column in a row.
#' @param offset A string, an offset value (ex. "md-3") to append to the "offset-" class.
#' @param margin A string, a valid Bootstrap margin class (ex. "me-auto")
#' @param custom_class A string, allows custom classes to be appended to the col class
#' @return A string of HTML.
#' @examples
#' bs_col(p("Col 1"))
bs_col <- function(
  ...,
  size = NULL,
  align_self = NULL,
  order = NULL,
  offset = NULL,
  margin = NULL,
  custom_class = NULL
){

  x <- "col"

  if(is.null(size) == FALSE){
    x <- paste0(paste0(x, "-", size), collapse = " ")
  }

  if(is.null(align_self) == FALSE){
    x <- paste0(x, " align-self-", align_self)
  }

  if(is.null(order) == FALSE){
    x <- paste0(x, " order-", order)
  }

  if(is.null(offset) == FALSE){
    x <- paste0(x, " ", paste0("offset-", offset, collapse = " "))
  }

  if(is.null(margin) == FALSE){
    x <- paste0(x, " ", margin)
  }

  if(is.null(custom_class) == FALSE){
    x <- paste0(x, " ", paste0(custom_class, collapse = " "))
  }

  return(
    div(
      class = x,
      ...
    )
  )
}

#' Create a div with class "w-100" to force columns onto a new line
#'
#' @return A string of HTML.
#' @examples
#' bs_col_break()
bs_col_break <- function(){
  return(
    div(
      class = "w-100"
    )
  )
}

#' Create a div with class "clearfix"
#'
#' @param ... Valid string(s) of HTML.
#' @return A string of HTML.
#' @examples
#' bs_clearfix(
#' p("Test")
#' )
bs_clearfix <- function(
  ...
){

  return(
    div(
      class = "clearfix",
      ...
    )
  )
}


#' Create HTML img tag configured with Bootstrap classes
#'
#' @param src A string, the value to pass to the "src" param of html5::img().
#' @param alt A string, the value to pass to the "alt" param of html5::img().
#' @param fluid TRUE/FALSE, if TRUE, adds the class "img-fluid".
#' @param thumbnail TRUE/FALSE, if TRUE (and fluid is FALSE), adds the class "img-thumbnail".
#' @param rounded TRUE/FALSE, if TRUE, adds the class "rounded".
#' @param float_start TRUE/FALSE, if TRUE, adds the class "float-start".
#' @param float_end TRUE/FALSE, if TRUE (and float_start is FALSE), adds the class "float-end".
#' @param center  TRUE/FALSE, if TRUE, wraps img tag in a div with class "text-center".
#' @param custom_class A string, a custom class to append to the img tag.
#' @return A string of HTML.
#' @examples
#' bs_img(
#' src = tempfile()
#' )
bs_img <- function(
  src,
  alt = NULL,
  fluid = TRUE,
  thumbnail = FALSE,
  rounded = FALSE,
  float_start = FALSE,
  float_end = FALSE,
  center = FALSE,
  custom_class = NULL
){

  x <- ""

  if(fluid == TRUE){
    x <- "img-fluid"
  }else if(thumbnail == TRUE){
    x <- "img-thumbnail"
  }

  if(rounded == TRUE){
    x <- if(nchar(x) > 0){paste0(x, " rounded")}else{"rounded"}
  }

  if(float_start == TRUE){
    x <- if(nchar(x) > 0){paste0(x, " float-start")}else{"float-start"}
  }else if(float_end == TRUE){
    x <- if(nchar(x) > 0){paste0(x, " float-end")}else{"float-end"}
  }

  if(is.null(custom_class) == FALSE){
    x <- if(nchar(x) > 0){paste0(x, " ", custom_class)}else{custom_class}
  }

  return(
    if(center == TRUE){
      div(
        class = "text-center",
        html5::img(src = src, alt = alt, class = x)
      )
    }else{
      html5::img(src = src, alt = alt, class = x)
    }
  )
}

#' Create HTML figure tag configured with Bootstrap classes
#'
#' @param src A string, the value to pass to the "src" param of html5::img().
#' @param alt A string, the value to pass to the "alt" html5::img().
#' @param caption A string, gets passed to html5::figcaption().
#' @param figure_class A string, the class of the figure tag.
#' @param img_class A string, the class of the img tag.
#' @param figcaption_class A string, the class of the figcaption tag.
#' @return A string of HTML.
#' @examples
#' bs_figure(
#' src = tempfile(),
#' caption = "This is an example"
#' )
bs_figure <- function(
  src,
  alt = NULL,
  caption = NULL,
  figure_class = "figure",
  img_class = "figure-img img-fluid",
  figcaption_class = "figure-caption"
){

  return(
    figure(
      class = figure_class,
      img(
        class = img_class,
        src = src,
        alt = alt
      ),
      if(is.null(caption) == FALSE){
        figcaption(
          class = figcaption_class,
          caption
        )
      }else{
        ""
      }
    )
  )

}

#' Create a HTML table configured with Bootstrap classes
#'
#' @param x A list with entries of equal length (use as.list(dataframe) to convert a data frame to a list).
#' @param headers A vector of names to use as table headers instead of the list names (or 1, 2, 3, etc. if list is not named).
#' @param table_class A string, the class of the table tag.
#' @param row_class A string, the class of the tr tag.
#' @param cell_class A string, the class of the th/td tag.
#' @param responsive TRUE/FALSE, if TRUE, wraps the table in a div with class "table-responsive".
#' @param responsive_breakpoint A string, gets appended to class "table-responsive-" in a div wrapping the table.
#'  One of "sm", "md", "lg", "xl", "xxl".
#' @param thead_class A string, the class of the thead tag.
#' @param tbody_class A string, the class of the tbody tag.
#' @param striped TRUE/FALSE, if TRUE, adds "table-striped" to the table class.
#' @param hoverable TRUE/FALSE, if TRUE, adds "table-hover" to the table class.
#' @param bordered TRUE/FALSE, if TRUE, adds "table-bordered" to the table class.
#' @param border_theme A string, one of the Bootstrap color themes (ex. "primary", "secondary", "warning", etc.).
#' @param borderless TRUE/FALSE, if TRUE (and bordered is FALSE), adds "table-borderless" to the table class.
#' @return A string of HTML.
#' @examples
#' bs_table(
#' x = list(
#' col1 = c(1, 2, 3, 4, 5),
#' col2 = c("a", "b", "c", "d", "e"),
#' col3 = c(1, 2, 3, 4, 5),
#' col4 = c("test", "test", "test", "test", "test")
#' ),
#' headers = c("First", "Second", "Third", "Fourth"),
#' table_class = "table table-primary",
#' row_class = "table table-primary",
#' cell_class = "table table-primary"
#' )
bs_table <- function(
  x = list(),
  headers = NULL,
  table_class = "table",
  row_class = "table",
  cell_class = "table",
  responsive = FALSE,
  responsive_breakpoint = NULL,
  thead_class = NULL,
  tbody_class = NULL,
  striped = FALSE,
  hoverable = FALSE,
  bordered = FALSE,
  border_theme = NULL,
  borderless = FALSE
){

  if(is.null(headers) == TRUE){

    if(length(names(x)) > 0){
      headers <- names(x)
    }else{
      headers <- 1:length(x)
    }

  }

  h <- tr(
    class = row_class,
    paste0(
      th(
        scope = "col",
        class = cell_class,
        headers
      ),
      collapse = ""
    )
  )

  b <- lapply(x, td, class = cell_class)

  b_rows <- list()

  for(i in 1:length(b[[1]])){

    b_rows[[i]] <- tr(paste0(listExtract(b, i), collapse = ""), class = row_class)

  }

  b <- paste0(unlist(b_rows), collapse = "")

  if(striped == TRUE){
    table_class <- paste(table_class, "table-striped")
  }

  if(hoverable == TRUE){
    table_class <- paste(table_class, "table-hover")
  }

  if(bordered == TRUE){

    table_class <- paste(table_class, "table-bordered")

    if(is.null(border_theme) == FALSE){
      table_class <- paste(table_class, paste0("border-", border_theme))
    }

  }else if(borderless == TRUE){

    table_class <- paste(table_class, "table-borderless")

  }

  x <- html5::table(
    class = table_class,
    thead(
      class = thead_class,
      h
    ),
    tbody(
      class = tbody_class,
      b
    )
  )

  if(responsive == TRUE & is.null(responsive_breakpoint) == FALSE){
    x <- div(
      class = paste0("table-responsive-", responsive_breakpoint),
      x
    )
  }else if(responsive == TRUE & is.null(responsive_breakpoint) == TRUE){
    x <- div(
      class = "table-responsive",
      x
    )
  }

  return(
    x
  )

}

#' Create a HTML input tag configured with Bootstrap classes
#'
#' @param type A string, the type of input to create, passed to the type param of html5::input().
#' @param id A string, the id/name for the input, passed to the id and name params of html5::input().
#' @param value A string, the value of the input, passed to the value param of html5::input().
#' @param label A string, the label to use for the input, passed to html5::label().
#' @param placeholder A string, passed to the placeholder param of html5::input().
#' @param div_class A string, the class of the div tag surrounding the the label and input tags.
#' @param input_class A string, the class of the input tag.
#' @param label_class A string, the class of the label tag.
#' @param text A string, text or HTML to display with the input and label tags.
#' @param text_class A string, the class of the text or HTML to display with the input and label tags.
#' @param input_args A named list, names should correspond to a parameter of the html5::input() function and values will be passed to that parameter.
#' @param inline TRUE/FALSE, if TRUE, adds a div and classes to display the label and input and text in the same line.
#' @return A string of HTML.
#' @examples
#' bs_input(
#' id = "example1",
#' label = "Example Text Input",
#' text = "This is an example input created with the bs_input function."
#' )
bs_input <- function(
  type = "text",
  id,
  value = NULL,
  label = NULL,
  placeholder = NULL,
  div_class = NULL,
  input_class = "form-control",
  label_class = "form-label",
  text = NULL,
  text_class = "form-text",
  input_args = list(),
  inline = FALSE
){

  if(type %in% c("checkbox", "radio")){

    if(is.null(div_class) == TRUE){
      div_class <- "form-check"
    }

    if(input_class == "form-control"){
      input_class <- "form-check-input"
    }

    if(label_class == "form-label"){
      label_class <- "form-check-label"
    }

  }else if(type %in% c("range")){
    if(input_class == "form-control"){
      input_class <- "form-range"
    }
  }

  inp <- input(
    accept = input_args[["accept"]],
    accesskey = input_args[["accesskey"]],
    alt = input_args[["alt"]],
    autocapitalize = input_args[["autocapitalize"]],
    autocomplete = input_args[["autocomplete"]],
    autofocus = input_args[["autofocus"]],
    capture = input_args[["capture"]],
    checked = input_args[["checked"]],
    class = input_class,
    contenteditable = input_args[["contenteditable"]],
    dir = input_args[["dir"]],
    dirname = input_args[["dirname"]],
    disabled = input_args[["disabled"]],
    draggable = input_args[["draggable"]],
    form = input_args[["form"]],
    formaction = input_args[["formaction"]],
    formenctype = input_args[["formenctype"]],
    formmethod = input_args[["formmethod"]],
    formnovalidate = input_args[["formnovalidate"]],
    formtarget = input_args[["formtarget"]],
    height = input_args[["height"]],
    hidden = input_args[["hidden"]],
    id = id,
    inputmode = input_args[["inputmode"]],
    is = input_args[["is"]],
    itemid = input_args[["itemid"]],
    itemprop = input_args[["itemprop"]],
    itemref = input_args[["itemref"]],
    itemscope = input_args[["itemscope"]],
    itemtype = input_args[["itemtype"]],
    lang = input_args[["lang"]],
    list = input_args[["list"]],
    max = input_args[["max"]],
    maxlength = input_args[["maxlength"]],
    min = input_args[["min"]],
    minlength = input_args[["minlength"]],
    multiple = input_args[["multiple"]],
    name = id,
    part = input_args[["part"]],
    pattern = input_args[["pattern"]],
    placeholder = placeholder,
    readonly = input_args[["readonly"]],
    required = input_args[["required"]],
    size = input_args[["size"]],
    slot = input_args[["slot"]],
    spellcheck = input_args[["spellcheck"]],
    src = input_args[["src"]],
    step = input_args[["step"]],
    style = input_args[["style"]],
    tabindex = input_args[["tabindex"]],
    title = input_args[["title"]],
    type = type,
    value = value,
    width = input_args[["width"]],
    custom_attr = input_args[["custom_attr"]]
  )

  lab <- if(is.null(label) == FALSE){
    html5::label(
      class = label_class,
      for_attr = id,
      label
    )
  }else{
    ""
  }

  text <- if(is.null(text) == FALSE){
    div(
      class = text_class,
      text
    )
  }else{
    ""
  }

  if(inline == TRUE){

    inp <- div(class = "col-auto", inp)
    lab <- div(class = "col-auto", lab)
    text <- div(class = "col-auto", text)

    div_class <- paste0("row", if(is.null(div_class) == FALSE){paste0(" ", div_class)}else{""})

  }

  if(type == "hidden"){

    x <- inp

  }else{

    if(type %in% c("checkbox", "radio")){

      x <- div(
        class = div_class,
        inp,
        lab,
        text
      )

    }else{

      x <- div(
        class = div_class,
        lab,
        inp,
        text
      )

    }

  }

  return(x)

}

#' Create a HTML textarea tag configured with Bootstrap classes
#'
#' @param id A string, the id/name for the textarea, passed to the id and name params of html5::textarea().
#' @param value A string, the value of the textarea, passed to the value param of html5::textarea().
#' @param label A string, the label to use for the textarea, passed to html5::label().
#' @param placeholder A string, passed to the placeholder param of html5::textarea().
#' @param rows An integer, the number of rows for the textarea, passed to the rows param of html5::textarea().
#' @param custom_height A string, should indicate a number and unit (ex. "100px") that gets appended to "height: " and passed to the style param of html5::textarea().
#' @param div_class A string, the class of the div tag surrounding the the label and textarea tags.
#' @param textarea_class A string, the class of the textarea tag.
#' @param label_class A string, the class of the label tag.
#' @param text A string, text or HTML to display with the textarea and label tags.
#' @param text_class A string, the class of the text or HTML to display with the textarea and label tags.
#' @param textarea_args A named list, names should correspond to a parameter of the html5::textarea() function and values will be passed to that parameter.
#' @param inline TRUE/FALSE, if TRUE, adds a div and classes to display the label and textarea and text in the same line.
#' @return A string of HTML.
#' @examples
#' bs_textarea(
#' id = "example1",
#' label = "Example Text Area Input"
#' )
bs_textarea <- function(
  id,
  value = NULL,
  label = NULL,
  placeholder = NULL,
  rows = NULL,
  custom_height = NULL,
  div_class = NULL,
  textarea_class = "form-control",
  label_class = "form-label",
  text = NULL,
  text_class = "form-text",
  textarea_args = list(),
  inline = FALSE
){

  ta <- html5::textarea(
    value,
    accesskey = textarea_args[["accesskey"]],
    autocapitalize = textarea_args[["autocapitalize"]],
    autocomplete = textarea_args[["autocomplete"]],
    autofocus = textarea_args[["autofocus"]],
    class = textarea_class,
    cols = textarea_args[["cols"]],
    contenteditable = textarea_args[["contenteditable"]],
    dir = textarea_args[["dir"]],
    disabled = textarea_args[["disabled"]],
    draggable = textarea_args[["draggable"]],
    form = textarea_args[["form"]],
    hidden = textarea_args[["hidden"]],
    id = id,
    inputmode = textarea_args[["inputmode"]],
    is = textarea_args[["is"]],
    itemid = textarea_args[["itemid"]],
    itemprop = textarea_args[["itemprop"]],
    itemref = textarea_args[["itemref"]],
    itemscope = textarea_args[["itemscope"]],
    itemtype = textarea_args[["itemtype"]],
    lang = textarea_args[["lang"]],
    maxlength = textarea_args[["maxlength"]],
    minlength = textarea_args[["minlength"]],
    name = id,
    part = textarea_args[["part"]],
    placeholder = placeholder,
    readonly = textarea_args[["readonly"]],
    required = textarea_args[["required"]],
    rows = rows,
    slot = textarea_args[["slot"]],
    spellcheck = textarea_args[["spellcheck"]],
    style = paste(paste0("height: ", custom_height), textarea_args[["style"]]),
    tabindex = textarea_args[["tabindex"]],
    title = textarea_args[["title"]],
    wrap = textarea_args[["wrap"]],
    custom_attr = textarea_args[[""]]
  )

  lab <- if(is.null(label) == FALSE){
    html5::label(
      class = label_class,
      for_attr = id,
      label
    )
  }else{
    ""
  }

  text <- if(is.null(text) == FALSE){
    div(
      class = text_class,
      text
    )
  }else{
    ""
  }

  if(inline == TRUE){

    ta <- div(class = "col-auto", ta)
    lab <- div(class = "col-auto", lab)
    text <- div(class = "col-auto", text)

    div_class <- paste0("row", if(is.null(div_class) == FALSE){paste0(" ", div_class)}else{""})

  }

  x <- div(
    class = div_class,
    lab,
    ta,
    text
  )

  return(x)

}

#' Create a HTML select tag configured with Bootstrap classes
#'
#' @param ... A string or strings of HTML to pass to html5::select(), most likely a sequence of option tags.
#' @param id A string, the id/name for the select, passed to the id and name params of html5::select().
#' @param label A string, the label to use for the select, passed to html5::label().
#' @param div_class A string, the class of the div tag surrounding the the label and select tags.
#' @param select_class A string, the class of the select tag.
#' @param label_class A string, the class of the label tag.
#' @param text A string, text or HTML to display with the select and label tags.
#' @param text_class A string, the class of the text or HTML to display with the select and label tags.
#' @param multiple TRUE/FALSE, if TRUE, adds the "multiple" attribute to the select tag.
#' @param select_args A named list, names should correspond to a parameter of the html5::select() function and values will be passed to that parameter.
#' @return A string of HTML.
#' @examples
#' bs_select(
#' option(value = 1, "Option One"),
#' option(value = 2, "Option Two"),
#' id = "example1",
#' label = "Example Select Input"
#' )
bs_select <- function(
  ...,
  id,
  label = NULL,
  div_class = NULL,
  select_class = "form-select",
  label_class = "form-label",
  text = NULL,
  text_class = "form-text",
  multiple = FALSE,
  select_args = list()
){

  sel <- html5::select(
    ...,
    accesskey = select_args[["accesskey"]],
    autocapitalize = select_args[["autocapitalize"]],
    autocomplete = select_args[["autocomplete"]],
    autofocus = select_args[["autofocus"]],
    class = select_class,
    contenteditable = select_args[["contenteditable"]],
    dir = select_args[["dir"]],
    disabled = select_args[["disabled"]],
    draggable = select_args[["draggable"]],
    form = select_args[["form"]],
    hidden = select_args[["hidden"]],
    id = id,
    inputmode = select_args[["inputmode"]],
    is = select_args[["is"]],
    itemid = select_args[["itemid"]],
    itemprop = select_args[["itemprop"]],
    itemref = select_args[["itemref"]],
    itemscope = select_args[["itemscope"]],
    itemtype = select_args[["itemtype"]],
    lang = select_args[["lang"]],
    multiple = if(multiple == FALSE){NULL}else{multiple},
    name = id,
    part = select_args[["part"]],
    required = select_args[["required"]],
    size = select_args[["size"]],
    slot = select_args[["slot"]],
    spellcheck = select_args[["spellcheck"]],
    style = select_args[["style"]],
    tabindex = select_args[["tabindex"]],
    title = select_args[["title"]],
    custom_attr = select_args[["custom_attr"]]
  )

  lab <- if(is.null(label) == FALSE){
    html5::label(
      class = label_class,
      for_attr = id,
      label
    )
  }else{
    ""
  }

  text <- if(is.null(text) == FALSE){
    div(
      class = text_class,
      text
    )
  }else{
    ""
  }

  x <- div(
    class = div_class,
    lab,
    sel,
    text
  )

  return(x)

}

#' Create a Bootstrap navbar
#'
#' @param brand A named list of length 1, name should be a value suitable for the href param of html5::a()
#' and the value is the HTML or text to display in the a tag.
#' @param from_left A list, creates navbar elements arranged to the left. If a list item is named,
#' names should be links and values should be text to display in the navbar for that link. Names will be passed to the
#' href param and values to the main content of the html5::a() function which will be wrapped by the html5::li() function,
#' each of which have Bootstrap classes added by default.If an item in the list is not named, the item must be
#' valid HTML with appropriate Bootstrap classes added manually. For example, to add a drop-down, add an unnamed item to the list
#' with HTML defining the drop-down as the value of the item.
#' @param from_right A list, creates navbar elements arranged to the right. If a list item is named,
#' names should be links and values should be text to display in the navbar for that link. Names will be passed to the
#' href param and values to the main content of the html5::a() function which will be wrapped by the html5::li() function,
#' each of which have Bootstrap classes added by default. If an item in the list is not named, the item must be
#' valid HTML with appropriate Bootstrap classes added manually. For example, to add a drop-down, add an unnamed item to the list
#' with HTML defining the drop-down as the value of the item.
#' @param id A string, used to control the collapse/expand functionality. Defaults to a random string.
#' @param navbar_class A string, the class passed to the nav tag.
#' @param background_class A string, a Bootstrap background class such as "bg-primary" that gets appended to navbar_class.
#' @param text_class A string, a Bootstrap navbar text class such as "navbar-dark" that gets appended to navbar_class.
#' @param background_color A string, a CSS color (ex. "#00FFFF"). Background-class will overrule this, so setting background_class to NULL
#' will ensure the background color is applied.
#' @param expand A string, the break-point at which the navbar should expand or collapse.  One of "sm", "md", "lg", "xl", "xxl".
#' If NULL, does not add classes to expand or collapse.
#' @param fluid TRUE/FALSE, if TRUE, makes the container wrapping the navbar elements fluid (use the entire screen width or not).
#' @param ul_class A string, default class used for the "ul" tag wrapping the navbar elements.
#' @param li_class A string, default class used for the "li" tags wrapping the navbar elements if the input list item is named.
#' @param a_class A string, default class used for the "a" tags wrapping the navbar elements if the input list item is named.
#' @return A string of HTML.
#' @examples
#' bs_navbar(
#' fluid = TRUE,
#' brand = list("#" = "Sample Brand"),
#' from_left = list(
#'   "/" = "Home",
#'   "/about" = "About"
#' ),
#' from_right = list(
#'   "/options" = "Options"
#' )
#' )
bs_navbar <- function(
  brand = list(),
  from_left = list(),
  from_right = list(),
  id = sampleStr(10),
  navbar_class = "navbar",
  background_class = "bg-primary",
  text_class = "navbar-dark",
  background_color = NULL,
  expand = "lg",
  fluid = FALSE,
  ul_class = "navbar-nav",
  li_class = "nav-item",
  a_class = "nav-link"
){

  if(is.null(expand) == FALSE){

    navbar_class <- paste(navbar_class, text_class, background_class, paste0("navbar-expand-", expand))

    toggle_button <- button(
      class = "navbar-toggler",
      type = "button",
      custom_attr = list(
      'data-bs-toggle' = "collapse",
      'data-bs-target' = paste0("#", id)
      ),
      span(
        class = "navbar-toggler-icon"
      )
    )

    div_expand_class <- "collapse navbar-collapse"

    # div_expand_id <- paste0("#", id)

    div_expand_id <- id

  }else{
    toggle_button <- NULL
    div_expand_class <- NULL
    div_expand_id <- NULL
  }

  if(length(brand) > 0){
    brand <- a(
      class = "navbar-brand",
      href = names(brand),
      unlist(brand)
    )
  }

  if(is.null(background_color) == FALSE){
    background_color <- paste0("background-color: ", background_color, ";")
  }

  if(length(from_left) > 0){

    from_left_names <- names(from_left)

    if(is.null(from_left_names) == FALSE){

      left_li <- list()

      for(i in 1:length(from_left)){

        if(nchar(from_left_names[i]) > 0){
          left_li[[i]] <- li(
            class = li_class,
            a(
              class = a_class,
              href = from_left_names[i],
              from_left[[i]]
            )
          )
        }else{
          left_li[[i]] <- from_left[[i]]
        }

      }

      left_nav <- ul(
        class = paste(ul_class, "me-auto"),
        paste0(unlist(left_li), collapse = "")
      )

    }else{

      left_nav <- paste0(from_left, collapse = "")

    }
  }else{

    if(length(from_right) > 0){
      left_nav <- div()
    }else{
      left_nav <- NULL
    }

  }

  if(length(from_right) > 0){
    from_right_names <- names(from_right)

    if(is.null(from_right_names) == FALSE){

      right_li <- list()

      for(i in 1:length(from_right)){

        if(nchar(from_right_names[i]) > 0){
          right_li[[i]] <- li(
            class = li_class,
            a(
              class = a_class,
              href = from_right_names[i],
              from_right[[i]]
            )
          )
        }else{
          right_li[[i]] <- from_right[[i]]
        }

      }

      right_nav <- ul(
        class = ul_class,
        paste0(unlist(right_li), collapse = "")
      )

    }else{

      right_nav <- paste0(from_right, collapse = "")

    }
  }else{
    right_nav <- NULL
  }

  x <- nav(
    class = navbar_class,
    style = background_color,
    bs_container(
      fluid = fluid,
      brand,
      toggle_button,
      div(
        class = div_expand_class,
        id = div_expand_id,
        paste0(
          left_nav,
          right_nav,
          collapse = ""
        )
      )
    )
  )

  return(x)

}

#' Create a Bootstrap nav
#'
#' @param items A list, creates nav elements. If a list item is named, names should be links
#' and values should be text to display in the nav for that link. Names will be passed to the
#' href param and values to the main content of the html5::a() function which will be wrapped by the html5::li() function,
#' each of which have Bootstrap classes added by default.If an item in the list is not named, the item must be
#' valid HTML with appropriate Bootstrap classes added manually. For example, to add a drop-down, add an unnamed item to the list
#' with HTML defining the drop-down as the value of the item.
#' @param vertical TRUE/FALSE, if TRUE,
#' @param tabs TRUE/FALSE, if TRUE,
#' @param pills TRUE/FALSE, if TRUE,
#' @param fill TRUE/FALSE, if TRUE,
#' @param justified TRUE/FALSE, if TRUE,
#' @param nav_class A string, default class used for the "nav" tag wrapping the nav elements.
#' @param background_class A string, a Bootstrap background class such as "bg-primary" that gets appended to nav_class.
#' @param text_class A string, a Bootstrap text class such as "navbar-dark" that gets appended to nav_class.
#' @param background_color A string, a CSS color (ex. "#00FFFF"). Background-class will overrule this, so setting background_class to NULL
#' will ensure the background color is applied.
#' @param ul_class A string, default class used for the "ul" tag wrapping the nav elements.
#' @param li_class A string, default class used for the "li" tags wrapping the nav elements if the input list item is named.
#' @param a_class A string, default class used for the "a" tags wrapping the nav elements if the input list item is named.
#' @return A string of HTML.
#' @examples
#' bs_nav(
#' items = list(
#'   "#" = "Option 1",
#'   "#" = "Option 2",
#'   "#" = "Option 3"
#' )
#')
bs_nav <- function(
  items = list(),
  vertical = FALSE,
  tabs = FALSE,
  pills = FALSE,
  fill = FALSE,
  justified = FALSE,
  nav_class = "nav",
  background_class = NULL,
  text_class = NULL,
  background_color = NULL,
  ul_class = "nav",
  li_class = "nav-item",
  a_class = "nav-link"
){

  if(length(items) > 0){

    item_names <- names(items)

    if(length(item_names) > 0){

      items_li <- list()

      for(i in 1:length(items)){

        if(nchar(item_names[i]) > 0){
          items_li[[i]] <- li(
            class = li_class,
            a(
              class = a_class,
              href = item_names[i],
              items[[i]]
            )
          )
        }else{
          items_li[[i]] <- items[[i]]
        }

      }

      if(vertical == TRUE){
        ul_class <- paste(ul_class, "flex-column")
      }

      item_nav <- ul(
        class = ul_class,
        paste0(unlist(items_li), collapse = "")
      )

    }else{

      item_nav <- paste0(items, collapse = "")

    }
  }else{

    item_nav <- NULL

  }

  if(vertical == TRUE){
    nav_class <- paste(nav_class, "flex-column")
  }

  if(tabs == TRUE){
    nav_class <- paste(nav_class, "nav-tabs")
  }else if(pills == TRUE){
    nav_class <- paste(nav_class, "nav-pills")
  }

  if(fill == TRUE){
    nav_class <- paste(nav_class, "nav-fill")
  }else if(justified == TRUE){
    nav_class <- paste(nav_class, "nav-justified")
  }

  nav_class <- paste(nav_class, text_class)
  nav_class <- paste(nav_class, background_class)
  if(is.null(background_color) == FALSE){
    background_color <- paste0("background-color: ", background_color, ";")
  }

  return(
    nav(
      class = nav_class,
      style = background_color,
      item_nav
    )
  )

}

#' Create a Bootstrap modal
#'
#' @param id A string, an id for the modal (to launch the modal, reference this id in the HTML element
#' that will launch the modal. See example below).
#' @param title A string, likely HTML, displayed as the title of the modal.
#' @param body A string, likely HTML, displayed as the body of the modal.
#' @param footer A string, likely HTML, displayed at the bottom of the modal.
#' @param modal_class A string, the class added to the the div wrapping the dialog.
#' @param dialog_class A string, the class added to the the div wrapping the content wrapper.
#' @param content_class A string, the class added to the the div wrapping the content (header, body, footer).
#' @param header_class A string, the class added to the the div wrapping the header.
#' @param title_class A string, the class added to the the div wrapping the title.
#' @param body_class A string, the class added to the div wrapping the body.
#' @param footer_class A string, the class added to the div wrapping the footer.
#' @return A string of HTML.
#' @examples
#' div(
#' button(
#' type = "button",
#' class = "btn btn-primary",
#' custom_attr = list(
#'   "data-bs-toggle" = "modal",
#'   "data-bs-target" = "#modal1"
#' ),
#' "Launch Modal"
#' ),
#' bs_modal(
#'   id = "modal1",
#'   body = p("Here is the modal.")
#' )
#' )
bs_modal <- function(
  id,
  title = h5("Note"),
  body,
  footer = button(
    type = "button",
    class = "btn btn-secondary",
    custom_attr = list("data-bs-dismiss" = "modal"),
    "Close"
  ),
  modal_class = "modal",
  dialog_class = "modal-dialog",
  content_class = "modal-content",
  header_class = "modal-header",
  title_class = "modal-title",
  body_class = "modal-body",
  footer_class = "modal-footer"
){
  return(
    div(
      class = modal_class,
      id = id,
      tabindex = "-1",
      div(
        class = dialog_class,
        div(
          class = content_class,
          div(
            class = header_class,
            div(
              class = title_class,
              title
            )
          ),
          div(
            class = body_class,
            body
          ),
          if(is.null(footer) == FALSE){
            div(
              class = footer_class,
              footer
            )
          }else{
            ""
          }
        )
      )
    )
  )
}

#' Create a Bootstrap pagination nav
#'
#' @param items A list, creates nav elements. If a list item is named, names should be links
#' and values should be text to display in the pagination bar for that link. Names will be passed to the
#' href param and values to the main content of the html5::a() function which will be wrapped by the html5::li() function,
#' each of which have Bootstrap classes added by default.If an item in the list is not named, the item must be
#' valid HTML with appropriate Bootstrap classes added manually.
#' @param ul_class A string, default class used for the "ul" tag wrapping the nav elements.
#' @param li_class A string, default class used for the "li" tags wrapping the nav elements if the input list item is named.
#' @param a_class A string, default class used for the "a" tags wrapping the nav elements if the input list item is named.
#' @return A string of HTML.
#' @examples
#' bs_pagination(
#' items = list(
#'   "#" = "Previous",
#'   "#" = "1",
#'   "#" = "2",
#'   "#" = "3",
#'   "#" = "Next"
#' )
#' )
bs_pagination <- function(
  items = list(),
  ul_class = "pagination",
  li_class = "page-item",
  a_class = "page-link"
){

  if(length(items) > 0){

    item_names <- names(items)

    if(length(item_names) > 0){

      items_li <- list()

      for(i in 1:length(items)){

        if(nchar(item_names[i]) > 0){
          items_li[[i]] <- li(
            class = li_class,
            a(
              class = a_class,
              href = item_names[i],
              items[[i]]
            )
          )
        }else{
          items_li[[i]] <- items[[i]]
        }

      }

      item_nav <- ul(
        class = ul_class,
        paste0(unlist(items_li), collapse = "")
      )

    }else{

      item_nav <- paste0(items, collapse = "")

    }
  }else{

    item_nav <- NULL

  }

  return(
    nav(
      item_nav
    )
  )

}

#' Create a Bootstrap collapse
#'
#' @param id A string, the id to use for the collapse div.
#' @param button_label A string, the text to display in the button controlling the collapsible content.
#' @param collapse_content A string, the HTML to display or collapse.
#' @param button_class A string, the class added to the button controlling the collapse.
#' @param div_class A string, the class added to the div wrapping the collapsible content.
#' @return A string of HTML.
#' @examples
#' bs_collapse(
#' id = "collapse1",
#' button_label = "Click to Expand",
#' collapse_content = p("Hello")
#' )
bs_collapse <- function(
  id,
  button_label,
  collapse_content,
  button_class = "btn btn-primary",
  div_class = "collapse"
){

  return(
    paste0(
      div(
        button(
          class = button_class,
          type = "button",
          custom_attr = list(
            "data-bs-toggle" = "collapse",
            "data-bs-target" = paste0("#", id)
          ),
          button_label
        )
      ),
      div(
        class = div_class,
        id = id,
        collapse_content
      )
    )
  )

}

#' Create a Bootstrap carousel
#'
#' @param id A string, the id to use for the carousel div.
#' @param items A list, entries should be HTML to display in the carousel.
#' @param carousel_class A string, the class of the div wrapping the carousel content.
#' @param inner_class A string, the class of the div wrapping the carousel items.
#' @param item_class A string, the class of the div wrapping each carousel item.
#' @param controls TRUE/FALSE, if TRUE, adds code to display arrows to click through the carousel.
#' @return A string of HTML.
#' @examples
#' bs_carousel(
#' id = "c1",
#' items = list(
#'   h1("First slide"),
#'   h1("Second slide"),
#'   h1("Third Slide")
#' )
#' )
bs_carousel <- function(
  id,
  items = list(),
  carousel_class = "carousel slide",
  inner_class = "carousel-inner",
  item_class = "carousel-item",
  controls = TRUE
){

  if(length(items) > 0){

    div_items <- list()

    for(i in 1:length(items)){

      div_items[[i]] <- div(
        class = if(i == 1){paste(item_class, "active")}else{item_class},
        items[[i]]
      )

    }

    items <- paste0(unlist(div_items), collapse = "")

  }else{

    items <- NULL

  }

  return(
    div(
      id = id,
      class = carousel_class,
      custom_attr = list(
        "data-bs-ride" = "carousel"
      ),
      div(
        class = inner_class,
        items
      ),
      if(controls == TRUE){
        paste0(
          a(
            class = "carousel-control-prev",
            href = paste0("#", id),
            custom_attr = list(
              "data-bs-slide" = "prev",
              "role" = "button"
            ),
            span(class = "carousel-control-prev-icon", custom_attr = list("aria-hidden" = "true")),
            span(class = "visually-hidden", "Previous")
          ),
          a(
            class = "carousel-control-next",
            href = paste0("#", id),
            custom_attr = list(
              "data-bs-slide" = "next",
              "role" = "button"
            ),
            span(class = "carousel-control-next-icon", custom_attr = list("aria-hidden" = "true")),
            span(class = "visually-hidden", "Next")
          )
        )
      }else{
        ""
      }
    )
  )

}

#' Create a Bootstrap accordion
#'
#' @param id A string, the id to use for the accordion, must be unique within a page (if you have multiple accordions on a page).
#' @param items A named list, names become the label for each panel, values should be the HTML content to display
#' when the panel is toggled.
#' @param accordion_class A string, the class of the div wrapping the accordion content.
#' @param item_class A string, the class of the div wrapping the accordion items.
#' @param item_header_class A string, the class of the accordion panel headers.
#' @param button_class A string, the class of the div wrapping the accordion panel header button items.
#' @param div_class A string, the class of the div wrapping the accordion panel content.
#' @param body_class A string, the class of the div wrapping the accordion panel content body.
#' @return A string of HTML.
#' @examples
#' bs_accordion(
#' id = "acc1",
#' items = list(
#'   "One" = p("Check it out."),
#'   "Two" = p("Does it work?"),
#'   "Three" = p("I hope so.")
#' )
#' )
bs_accordion <- function(
  id,
  items = list(),
  accordion_class = "accordion",
  item_class = "accordion-item",
  item_header_class = "accordion-header",
  button_class = "accordion-button",
  div_class = "accordion-collapse collapse",
  body_class = "accordion-body"
){

  item_names <- names(items)

  if(length(item_names) == 0){

    item_names <- 1:length(items)

  }

  div_items <- list()

  for(i in 1:length(items)){

    div_items[[i]] <- div(
      class = item_class,
      h2(
        class = item_header_class,
        id = paste0("heading_", id, "_", i),
        button(
          class = button_class,
          type = "button",
          custom_attr = list(
            "data-bs-toggle" = "collapse",
            "data-bs-target" = paste0("#collapse_", id, "_", i),
            "aria-expanded" = "false",
            "aria-controls" = paste0("collapse_", id, "_", i)
          ),
          item_names[i]
        )
      ),
      div(
        id = paste0("collapse_", id, "_", i),
        class = div_class,
        custom_attr = list(
          "aria-labelledby" = paste0("heading_", id, "_", i),
          "data-bs-parent" = id
        ),
        div(
          class = body_class,
          items[[i]]
        )
      )
    )

  }

  items <- paste0(unlist(div_items), collapse = "")

  return(
    div(
      class = accordion_class,
      id = id,
      items
    )
  )

}

#' Create a Bootstrap dropdown
#'
#' @param id A string, the id to use for the dropdown, must be unique within a page (if you have multiple dropdowns on a page).
#' @param items A named list, names become the href for each item, values should be the text to display for each href. If an item is
#' unnamed, the value does not get passed to the li/a tags and is instead displayed as-is (useful for displaying custom HTML content in the
#' dropdown).
#' @param button_label A string, the label to use for the dropdown toggle button.
#' @param dropdown_class A string, the class of the div wrapping the dropdown elements.
#' @param button_class A string, the class of the button controlling the dropdown toggle.
#' @param ul_class A string, default class used for the "ul" tag wrapping the dropdown nav elements.
#' @param li_class A string, default class used for the "li" tags wrapping the dropdown nav elements if the input list item is named.
#' @param a_class A string, default class used for the "a" tags wrapping the dropdown nav elements if the input list item is named.
#' @return A string of HTML.
#' @examples
#' bs_dropdown(
#' id = "drop",
#' items = list(
#'   "#" = "Home",
#'   "#" = "About",
#'   "#" = "Other"
#' )
#' )
bs_dropdown <- function(
  id,
  items = list(),
  button_label = "Dropdown Button",
  dropdown_class = "dropdown",
  button_class = "btn btn-secondary dropdown-toggle",
  ul_class = "dropdown-menu",
  li_class = NULL,
  a_class = "dropdown-item"
){

  if(length(items) > 0){

    item_names <- names(items)

    if(length(item_names) > 0){

      items_li <- list()

      for(i in 1:length(items)){

        if(nchar(item_names[i]) > 0){
          items_li[[i]] <- li(
            class = li_class,
            a(
              class = a_class,
              href = item_names[i],
              items[[i]]
            )
          )
        }else{
          items_li[[i]] <- items[[i]]
        }

      }

      item_list <- paste0(unlist(items_li), collapse = "")

    }else{

      item_list <- paste0(items, collapse = "")

    }
  }else{

    item_list <- NULL

  }

  return(
    div(
      class = dropdown_class,
      button(
        class = button_class,
        type = "button",
        id = id,
        custom_attr = list(
          "data-bs-toggle" = "dropdown",
          "aria-expanded" ="false"
        ),
        button_label
      ),
      ul(
        class = ul_class,
        custom_attr = list(
          "aria-labelledby" = id
        ),
        item_list
      )
    )
  )

}

#' Create a Bootstrap card
#'
#' @param body A string, the HTML to display in the body of the card.
#' @param img_src A string, the path of an image to display with the card. Passed to "src" param of html5::img().
#' @param img_alt A string, the alt attribute of an image to display with the card. Passed to "alt" param of html5::img().
#' @param div_class A string, the class of the div wrapping the card content.
#' @param body_class A string, the class of the div wrapping the card body.
#' @param img_class A string, the class of the img tag if an image is to be displayed.
#' @return A string of HTML.
#' @examples
#' bs_card(
#' body = "This is a card"
#' )
bs_card <- function(
  body,
  img_src = NULL,
  img_alt = NULL,
  div_class = "card",
  body_class = "card-body",
  img_class = "card-img-top"
){

  return(
    div(
      class = div_class,
      if(is.null(img_src) == FALSE){
        img(
          class = img_class,
          src = img_src,
          alt = img_alt
        )
      }else{
        ""
      },
      div(
        class = body_class,
        body
      )
    )
  )

}

