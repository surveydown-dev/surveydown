-- Function to get the path of a file in an R package
local function get_package_file_path(package_name, file_name, subdirectory)
  local cmd = string.format(
    "Rscript -e \"cat(system.file('%s', '%s', package = '%s'))\"",
    subdirectory or "", file_name, package_name
  )
  return pandoc.pipe("sh", {"-c", cmd}, ""):gsub("%s+$", "")
end

local function ensure_html_deps()
  local package_name = "surveydown"

  local surveydown_css_path = get_package_file_path(package_name, "surveydown.css", "css")
  local page_nav_js_path = get_package_file_path(package_name, "page_nav.js", "js")
  local keep_alive_js_path = get_package_file_path(package_name, "keep_alive.js", "js")
  local update_progress_js_path = get_package_file_path(package_name, "update_progress.js", "js")
  local required_questions_js_path = get_package_file_path(package_name, "required_questions.js", "js")

  quarto.doc.add_html_dependency({
    name = 'surveydowncss',
    stylesheets = {surveydown_css_path}
  })
  quarto.doc.add_html_dependency({
    name = 'pagenavjs',
    scripts = {page_nav_js_path}
  })
  quarto.doc.add_html_dependency({
    name = 'keepalivejs',
    scripts = {keep_alive_js_path}
  })
  quarto.doc.add_html_dependency({
    name = 'updateprogressjs',
    scripts = {update_progress_js_path}
  })
  quarto.doc.add_html_dependency({
    name = 'requiredquestionsjs',
    scripts = {required_questions_js_path}
  })
end

function Pandoc(doc)
  ensure_html_deps()
  -- Define Bootswatch theme primary colors
  local theme_colors = {
    cerulean  = "#2FA4E7",
    cosmo     = "#2780E3",
    cyborg    = "#2A9FD6",
    darkly    = "#375A7F",
    flatly    = "#18BC9C",
    journal   = "#EB6864",
    litera    = "#007BFF",
    lumen     = "#F08D49",
    lux       = "#343A40",
    materia   = "#2196F3",
    minty     = "#78C2AD",
    morph     = "#218C74",
    paper     = "#2196F3",
    pulse     = "#593196",
    quartz    = "#8C9EFF",
    readable  = "#3273DC",
    sandstone = "#93C54B",
    simplex   = "#D9230F",
    sketchy   = "#333333",
    slate     = "#007AFF",
    spacelab  = "#3398DC",
    superhero = "#DF691A",
    united    = "#E95420",
    vapor     = "#9B59B6",
    yeti      = "#008CBA"
  }
  -- Fetch metadata values with defaults
  local barcolor = pandoc.utils.stringify(doc.meta['barcolor'] or 'theme')
  local barposition = pandoc.utils.stringify(doc.meta['barposition'] or 'top')
  local theme = pandoc.utils.stringify(doc.meta['theme'] and doc.meta['theme'][1] or 'raleway')
  local backgroundcolor = pandoc.utils.stringify(doc.meta['backgroundcolor'] or '#f2f6f9')
  -- Function to check if a string is a valid hex color
  local function is_hex_color(color)
    return color:match("^#%x%x%x%x%x%x$") ~= nil
  end
  -- Determine the color
  local color
  if is_hex_color(barcolor) then
    color = barcolor
  else
    color = barcolor == 'theme' and theme_colors[theme] or theme_colors['cosmo']
  end
  -- Ensure valid position
  local position = barposition == 'bottom' and 'bottom' or 'top'
  -- Replace placeholders in CSS template
  local css = [[
  <style>
  #progressbar.]] .. position .. [[ {
    ]] .. position .. [[: 0;
  }
  body {
    background-color: ]] .. backgroundcolor .. [[;
  }
  </style>
  ]]
  -- Define the HTML for the progress bar
  local progressbar = string.format([[
  <div id="progressbar" class="%s">
    <div id="progress" style="background-color: %s;"></div>
  </div>
  ]], position, color)
  -- Define CSS for Raleway font and link to Google Fonts
  local raleway_html = [[
  <link href="https://fonts.googleapis.com/css2?family=Raleway:ital,wght@0,400;0,800;1,400;1,800&display=swap" rel="stylesheet">
  <style>
  body, .h1, .h2, .h3, .h4, .h5, .h6, h1, h2, h3, h4, h5, h6 {
    font-family: 'Raleway', sans-serif;
  }
  </style>
  ]]
  -- Insert the CSS and progress bar HTML into the document
  table.insert(doc.blocks, 1, pandoc.RawBlock('html', css))
  table.insert(doc.blocks, 2, pandoc.RawBlock('html', progressbar))
  -- Insert Raleway CSS if no theme is specified or if theme is 'raleway'
  if theme == 'raleway' then
    table.insert(doc.blocks, 3, pandoc.RawBlock('html', raleway_html))
  end
  return doc
end
