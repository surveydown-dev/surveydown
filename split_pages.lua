-- Initialize table to collect page identifiers
local pages = {}

-- Function to handle Div elements
function Div(elem)
    if elem.classes:includes("sd-page") then
        -- Collect page identifiers
        table.insert(pages, elem.identifier)
    end
end

-- After all Divs are processed, add Next buttons except for the last page
function Pandoc(doc)
    for i, identifier in ipairs(pages) do
        if i < #pages then
            local nextButtonHtml = '<button onclick="Shiny.setInputValue(\'next-' .. pages[i] .. '\', true);">Next</button>'
            -- Find the Div element and append the Next button
            for _, block in ipairs(doc.blocks) do
                if block.t == "Div" and block.identifier == identifier then
                    table.insert(block.content, pandoc.RawBlock('html', nextButtonHtml))
                    break
                end
            end
        end
    end

    -- Prepare to write the JSON file with page information
    local jsonStr = '{"pages":['
    for i, page in ipairs(pages) do
        jsonStr = jsonStr .. '{"name":"' .. page .. '","number":' .. i .. '}'
        if i ~= #pages then
            jsonStr = jsonStr .. ','
        end
    end
    jsonStr = jsonStr .. ']}'

    -- Write JSON file
    local file = io.open(".survey_pages.json", "w")
    file:write(jsonStr)
    file:close()

    return doc
end
