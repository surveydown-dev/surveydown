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
            local nextButtonHtml = string.format('<button onclick="Shiny.setInputValue(\'next-%s\', true);">Next</button>', pages[i])
            -- Find the Div element and append the Next button
            for _, block in ipairs(doc.blocks) do
                if block.t == "Div" and block.identifier == identifier then
                    table.insert(block.content, pandoc.RawBlock('html', nextButtonHtml))
                    break
                end
            end
        end
    end

    return doc
end
