function Pandoc(doc)
  -- Insert a div with class 'sd-page' at the start of the document
  table.insert(doc.blocks, 1, pandoc.Div({}, {class = "sd-page"}))

  -- Find the position to insert the closing div (end of document)
  local pos_to_close = #doc.blocks + 1

  -- Insert a closing div at the end of the document
  table.insert(doc.blocks, pos_to_close, pandoc.Div({}, {class = "sd-page"}))

  return doc
end
