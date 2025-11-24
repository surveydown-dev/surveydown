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
  yml = "questions.yml",
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
  `"text"`, `"textarea"`, `"numeric"`, `"slider"`, `"slider_numeric"`,
  `"date"`, `"daterange"`, and `"matrix"`. Defaults to `NULL`.

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
    shown in UI and automatically converted to snake_case for database
    storage (e.g., "option_1", "option_2")

  - Unnamed numeric vector: `c(1, 2, 3)` - For non-slider questions,
    converted to `c("1" = "1", "2" = "2", "3" = "3")`. For
    `slider_numeric`, kept as numeric.

- options:

  Alias for `option`. Either `option` or `options` can be used. If both
  are provided, `option` takes precedence. Supports the same formats as
  `option`.

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

  Logical. Whether to show a grid for slider input. Defaults to `TRUE`.

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

  List. Used for `"matrix"` type questions. Contains the row labels and
  their corresponding IDs.

- default:

  Numeric, length 1 (for a single sided slider), or 2 for a two sided
  (range based) slider. Values to be used as the starting default for
  the slider. Defaults to the median of values.

- yml:

  Character string. The name of the YAML file to load question
  configurations from. Defaults to `"questions.yml"`. Custom YAML files
  can be specified, either in the root directory or subdirectories
  (e.g., `"folder/custom.yml"`).

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

- `"text"`: Single-line text question

- `"textarea"`: Multi-line text question

- `"numeric"`: Numeric question

- `"slider"`: Slider question

- `"slider_numeric"`: Extended numeric slider question

- `"date"`: Date question

- `"daterange"`: Date range question

- `"matrix"`: Matrix-style question with rows and columns

For `"matrix"` type questions, use the `row` parameter to define the
rows of the matrix. Each element in the `row` list should have a name
(used as the row ID) and a value (used as the row label).

## Examples

``` r
if (interactive()) {
  library(surveydown)

  # Get path to example survey file
  survey_path <- system.file("examples", "basic_survey.qmd",
                             package = "surveydown")

  # Copy to a temporary directory
  temp_dir <- tempdir()
  file.copy(survey_path, file.path(temp_dir, "survey.qmd"))
  orig_dir <- getwd()
  setwd(temp_dir)

  # Define a minimal server
  server <- function(input, output, session) {
    sd_server()
  }

  # Run the app
  shiny::shinyApp(ui = sd_ui(), server = server)

  # Clean up
  setwd(orig_dir)
}
```
