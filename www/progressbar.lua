function Pandoc(doc)
  -- Define the CSS template with placeholders for color and position
  local css_template = [[
  <style>
  #progressbar {
    background-color: #ECE8DF;
    padding: 3px;
    width: 100%;
    position: fixed;
    left: 0;
    z-index: 1000;
  }
  #progressbar.POSITION_PLACEHOLDER {
    POSITION_PLACEHOLDER: 0;
  }
  #progressbar > div {
    width: 0%;
    height: 10px;
    border-radius: 0;
  }
  body {
    padding-top: 20px;
  }
  </style>
  ]]

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
  local theme = pandoc.utils.stringify(doc.meta['theme'] and doc.meta['theme'][1] or 'cosmo')

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
  local css = css_template:gsub("POSITION_PLACEHOLDER", position)

  -- Define the HTML for the progress bar
  local progressbar = string.format([[
  <div id="progressbar" class="%s">
    <div id="progress" style="background-color: %s;"></div>
  </div>
  ]], position, color)

  -- Insert the CSS and progress bar HTML into the document
  table.insert(doc.blocks, 1, pandoc.RawBlock('html', css))
  table.insert(doc.blocks, 2, pandoc.RawBlock('html', progressbar))

  return doc
end