-- Initialize a counter for page numbering
local pageCount = 0
-- Flag to indicate the first content block
local firstBlock = true

function Pandoc(doc)
    local blocks = {}

    for _, block in ipairs(doc.blocks) do
        -- Check if we're processing the first content block
        if firstBlock then
            -- This is the first content block, start the first page
            pageCount = 1
            local divStart = '<div id="page-1" class="page page-visible">'
            table.insert(blocks, pandoc.RawBlock('html', divStart))
            firstBlock = false -- Reset the flag
        end

        if block.t == "HorizontalRule" then
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
    if pageCount > 0 then
        table.insert(blocks, pandoc.RawBlock('html', '</div>')) -- Close the last page div
    end

    return pandoc.Pandoc(blocks, doc.meta)
end
