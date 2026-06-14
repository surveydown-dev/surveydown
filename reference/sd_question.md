# Create a survey question

This function creates various types of survey questions for use in a
Surveydown survey.

## Usage

``` r
sd_question(
  id,
  type = NULL,
  label = NULL,
  option = NULL,
  options = NULL,
  option_attr = NULL,
  cols = "80",
  direction = "horizontal",
  status = "default",
  width = "100%",
  height = NULL,
  selected = NULL,
  label_select = "Choose an option...",
  grid = TRUE,
  individual = TRUE,
  justified = FALSE,
  force_edges = TRUE,
  placeholder = NULL,
  resize = NULL,
  row = NULL,
  default = NULL,
  image = NULL,
  yml = "questions.yml",
  matrix_question_width = NULL,
  ...
)
```

## Arguments

- id:

  A unique identifier for the question, which will be used as the
  variable name in the resulting survey data.

- type:

  Specifies the type of question. Possible values are `"select"`,
  `"mc"`, `"mc_multiple"`, `"mc_buttons"`, `"mc_multiple_buttons"`,
  `"mc_image"`, `"mc_multiple_image"`, `"text"`, `"textarea"`,
  `"numeric"`, `"slider"`, `"slider_numeric"`, `"date"`, `"daterange"`,
  `"matrix"`, and `"matrix_multiple"`. Defaults to `NULL`.

- label:

  Character string. The label for the UI element, which can be formatted
  with markdown. Defaults to `NULL`

- option:

  Named vector for the `"select"`, `"radio"`, `"checkbox"`, and
  `"slider"` question types, or numeric vector for `"slider_numeric"`
  question type. Can be provided in multiple formats:

  - Named vector:
    `c("Display A" = "value_a", "Display B" = "value_b")` - Names are
    shown in UI, values are stored in database

  - Unnamed character vector: `c("Option 1", "Option 2")` - Values are
    used as both display labels and stored values (e.g., "Option 1" is
    shown and stored as "Option 1")

  - Unnamed numeric vector: `c(1, 2, 3)` - For non-slider questions,
    converted to `c("1" = "1", "2" = "2", "3" = "3")`. For
    `slider_numeric`, kept as numeric.

- options:

  Alias for `option`. Either `option` or `options` can be used. If both
  are provided, `option` takes precedence. Supports the same formats as
  `option`.

- option_attr:

  Character vector. Optional HTML attributes to apply to individual
  options for `"mc"` and `"mc_multiple"` question types. Each element
  corresponds to an option in the same position. Defaults to `NULL`.

- cols:

  Integer. Number of columns for the `"textarea"` question type.
  Defaults to `80`.

- direction:

  Character string. The direction for button groups (`"horizontal"` or
  `"vertical"`). Defaults to `"horizontal"`.

- status:

  Character string. The status for button groups. Defaults to
  `"default"`.

- width:

  Character string. The width of the UI element. Defaults to `"100%"`.

- height:

  Character string. The height of the input for the `"textarea"`
  question type. Defaults to `"100px"`.

- selected:

  Value. The selected value(s) for certain input elements.

- label_select:

  Character string. The label for the select input. Defaults to
  `"Choose an option..."`.

- grid:

  Logical. Whether to show tick marks and labels under each position of
  a `"slider"` question. Defaults to `TRUE`. Only the major (labeled)
  tick marks are shown; the small minor tick marks between positions are
  hidden by surveydown's styling.

- individual:

  Logical. Whether buttons in a group should be individually styled.
  Defaults to `TRUE`.

- justified:

  Logical. Whether buttons in a group should fill the width of the
  parent div. Defaults to `FALSE`.

- force_edges:

  Logical. Whether to force edges for slider input. Defaults to `TRUE`.

- placeholder:

  Character string. Placeholder text for `"text"` and `"textarea"`
  question types.

- resize:

  Character string. Resize option for textarea input. Defaults to
  `NULL`.

- row:

  List. Used for `"matrix"` and `"matrix_multiple"` type questions.
  Contains the row labels and their corresponding IDs.

- default:

  Numeric, length 1 (for a single sided slider), or 2 for a two sided
  (range based) slider. Values to be used as the starting default for
  the slider. Defaults to the median of values.

- image:

  Character vector. Required for `"mc_image"` and `"mc_multiple_image"`
  question types: image paths or URLs, one per option, in the same order
  as `option`. Paths are used as-is in the image `src`, so they should
  resolve against the survey's `images` or `www` folder (e.g.
  `"images/cat.png"`) or be full URLs.

  For image-choice questions, the `option` names control captions:

  - Named options (e.g. `c("Cat" = "cat", "Dog" = "dog")`) show the
    names as text captions beneath each image.

  - An unnamed option vector (e.g. `c("cat", "dog")`) shows the images
    only, with no captions. The values are still stored in the data as
    usual.

  This differs from the other multiple-choice types, where an unnamed
  option vector uses the values as both the displayed labels and the
  stored values.

- yml:

  Character string. The name of the YAML file to load question
  configurations from. Defaults to `"questions.yml"`. Custom YAML files
  can be specified, either in the root directory or subdirectories
  (e.g., `"folder/custom.yml"`).

- matrix_question_width:

  The width of the matrix question column. Accepts numeric (e.g., `40`),
  character without percent (e.g., `"40"`), or character with percent
  (e.g., `"40%"`) - all are treated equivalently as percentages.
  Defaults to `NULL`, which auto-calculates the width based on the
  longest row label (using a heuristic of 20% base + 0.5% per character,
  bounded between 30% and 80%). The remaining width is automatically
  distributed equally among the option columns.

- ...:

  Additional arguments, often specific to different input types.
  Examples include `pre`, `sep`, `step`, and `animate` for `"slider"`
  and `"slider_numeric"` question types, etc.

## Value

A 'shiny' UI element wrapped in a div with a data attribute for question
ID.

## Details

The function supports various question types:

- `"select"`: A dropdown selection

- `"mc"`: Multiple choice (single selection)

- `"mc_multiple"`: Multiple choice (multiple selections allowed)

- `"mc_buttons"`: Multiple choice with button-style options (single
  selection)

- `"mc_multiple_buttons"`: Multiple choice with button-style options
  (multiple selections allowed)

- `"mc_image"`: Multiple choice where each option is an image card
  (single selection)

- `"mc_multiple_image"`: Multiple choice where each option is an image
  card (multiple selections allowed)

- `"text"`: Single-line text question

- `"textarea"`: Multi-line text question

- `"numeric"`: Numeric question

- `"slider"`: Slider question

- `"slider_numeric"`: Extended numeric slider question

- `"date"`: Date question

- `"daterange"`: Date range question

- `"matrix"`: Matrix-style question with rows and columns (single
  selection per row, radio buttons)

- `"matrix_multiple"`: Matrix-style question where each row allows
  multiple selections (checkboxes)

For `"matrix"` and `"matrix_multiple"` type questions, use the `row`
parameter to define the rows of the matrix. Each element in the `row`
list should have a name (used as the row ID) and a value (used as the
row label). Each row becomes its own sub-question with the ID
`<question_id>_<row_id>`, stored as a separate column in the data.

## Examples

``` r
if (interactive()) {
  library(surveydown)

  # Use sd_question() to create questions in R chunks of survey.qmd:
  # sd_question(
  #   id    = "favorite_penguin_static",
  #   type  = "mc",
  #   label = "Which type of penguin do you like the best?",
  #   option = c(
  #     "Adélie"    = "adelie",
  #     "Chinstrap" = "chinstrap",
  #     "Gentoo"    = "gentoo"
  #   )
  #  )

  # Use sd_question() to create reactive questions in app.R under server:
  # server <- function(input, output, session) {
  #   sd_question(
  #     id    = "favorite_penguin_reactive",
  #     type  = "mc",
  #     label = "Which type of penguin do you like the best?",
  #     option = c(
  #       "Adélie"    = "adelie",
  #       "Chinstrap" = "chinstrap",
  #       "Gentoo"    = "gentoo"
  #     )
  #    )
  #   sd_server()
  # }

  # Find a working directory and start from a template:
  sd_create_survey(template = "default")
  # This creates survey.qmd and app.R - launch the survey using app.R
}
```
