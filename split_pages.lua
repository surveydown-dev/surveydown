-- Initialize a table to hold page names and their numbers
local pages = {}
local currentPageNumber = 0

-- Second pass to append Next buttons and collect page data
function Div(elem)
  if elem.classes:includes("sd-page") then
    currentPageNumber = currentPageNumber + 1
    local pageName = elem.identifier

    -- Append page's information to the 'pages' table
    table.insert(pages, {name = pageName, number = currentPageNumber})

    -- Generate a Next button with an ID based on the page name, except for the last page

    local nextButtonHtml = string.format('<button onclick="Shiny.setInputValue(\'next-%s\', true);">Next</button>', pageName)
    table.insert(elem.content, pandoc.RawBlock('html', nextButtonHtml))
  end
  return elem
end

function Pandoc(doc)
  local filePath = ".survey_pages.json"

  -- Open the file for writing
  local file = io.open(filePath, "w")
  if not file then
    error("Could not open file " .. filePath)
  end

  -- Write the table as a JSON string
  file:write('{"pages":[')
  for i, page in ipairs(pages) do
    file:write('{"name":"' .. page.name .. '","number":' .. page.number .. '}')
    if i ~= #pages then
      file:write(',')
    end
  end
  file:write(']}')

  file:close()

  return doc
end
