function Pandoc(doc)
  -- Define the CSS styles
  local css = [[
  <style>
  #progressbar {
    background-color: black;
    border-radius: 13px;
    padding: 3px;
  }
  #progressbar > div {
    background-color: orange;
    width: 40%;
    height: 20px;
    border-radius: 10px;
  }
  </style>
  ]]

  -- Define the HTML for the progress bar
  local progressbar = [[
  <div id="progressbar">
    <div></div>
  </div>
  ]]

  -- Convert the CSS to a pandoc element
  local css_block = pandoc.RawBlock('html', css)

  -- Insert the CSS at the beginning of the document
  table.insert(doc.blocks, 1, css_block)

  -- Get the progress bar position from the metadata
  local position = pandoc.utils.stringify(doc.meta['progressbar'] or 'none')

  -- Insert the progress bar at the specified position
  if position == "top" then
    table.insert(doc.blocks, 2, pandoc.RawBlock('html', progressbar))
  elseif position == "bottom" then
    table.insert(doc.blocks, pandoc.RawBlock('html', progressbar))
  end

  -- Remove the placeholder if present
  for i, block in ipairs(doc.blocks) do
    if block.t == "Para" and block.content[1].text == "{{progressbar}}" then
      table.remove(doc.blocks, i)
      break
    end
  end

  return doc
end
