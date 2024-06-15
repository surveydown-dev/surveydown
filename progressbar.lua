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
    background-color: COLOR_PLACEHOLDER;
    width: 0%;
    height: 10px;
    border-radius: 0;
  }
  body {
    padding-top: 20px;
  }
  </style>
  ]]

  -- Define available colors
  local colors = {
    green  = "#4CAF50",
    orange = "#FFA500",
    blue   = "#2196F3",
    purple = "#5654A2"
  }

  -- Fetch metadata values with defaults
  local barcolor = pandoc.utils.stringify(doc.meta['barcolor'] or 'green')
  local barposition = pandoc.utils.stringify(doc.meta['barposition'] or 'top')

  -- Ensure valid color and position
  local color = colors[barcolor] or colors['green']
  local position = barposition == 'bottom' and 'bottom' or 'top'

  -- Replace placeholders in CSS template
  local css = css_template:gsub("COLOR_PLACEHOLDER", color):gsub("POSITION_PLACEHOLDER", position)

  -- Define the HTML for the progress bar
  local progressbar = string.format([[
  <div id="progressbar" class="%s">
    <div id="progress"></div>
  </div>
  ]], position)

  -- Insert the CSS and progress bar HTML into the document
  table.insert(doc.blocks, 1, pandoc.RawBlock('html', css))
  table.insert(doc.blocks, 2, pandoc.RawBlock('html', progressbar))

  return doc
end
