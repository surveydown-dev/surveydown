-- Define a global variable to keep track of the section count
section_count = 0

-- Start with the first section before any content
function DocStart()
    section_count = section_count + 1
    return pandoc.RawBlock('html', '<section id="pg' .. section_count .. '">')
end

function HorizontalRule(elem)
    -- Increment the section count
    section_count = section_count + 1

    -- Close the previous section and start a new one
    return {
        pandoc.RawBlock('html', '</section>'),
        pandoc.RawBlock('html', '<section id="pg' .. section_count .. '">')
    }
end

function Pandoc(doc)
    -- Add a closing section tag at the end of the document
    table.insert(doc.blocks, pandoc.RawBlock('html', '</section>'))
    return doc
end
