-- Initialize a counter for page numbering
local pageCount = 0
-- Flag to indicate the first content block
local firstBlock = true

function Pandoc(doc)
    local blocks = {}

    for i, block in ipairs(doc.blocks) do
        -- Check if we're processing the first content block
        if firstBlock then
            -- This is the first content block, start the first page
            pageCount = 1
            local divStart = '<div id="page-' .. pageCount .. '" class="page page-visible">'
            table.insert(blocks, pandoc.RawBlock('html', divStart))
            firstBlock = false -- Reset the flag
        end

        if block.t == "HorizontalRule" then
            -- Insert a "Next" button before closing the current page div
            -- Note: Adjust the button's HTML to match your needs
            local nextButtonHtml = string.format('<button onclick="Shiny.setInputValue(\'next%d\', true);">Next</button>', pageCount)
            table.insert(blocks, pandoc.RawBlock('html', nextButtonHtml))

            -- Close the previous div and start a new one for subsequent pages
            table.insert(blocks, pandoc.RawBlock('html', '</div>')) -- Close the previous page div
            pageCount = pageCount + 1
            local divStart = string.format('<div id="page-%d" class="page page-hidden" style="display: none;">', pageCount)
            table.insert(blocks, pandoc.RawBlock('html', divStart))
        else
            -- For all other blocks, just add them to the blocks table
            table.insert(blocks, block)
        end
    end

    -- Close the last div if any pages were started
    -- Note: No "Next" button for the last page, assuming a different action like "Submit"
    if pageCount > 0 then
        table.insert(blocks, pandoc.RawBlock('html', '</div>')) -- Close the last page div
    end

    return pandoc.Pandoc(blocks, doc.meta)
end
