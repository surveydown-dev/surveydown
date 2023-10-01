-- Define a global table to store sections
sections = {}
current_section = {}

function HorizontalRule(elem)
  -- Add the current section to the sections table
  table.insert(sections, current_section)
  -- Reset the current section
  current_section = {}
  -- We return an empty table to effectively remove the HorizontalRule from the output
  return {}
end

function Block(element)
  table.insert(current_section, element)
  return element
end

function Pandoc(doc)
  -- Capture any remaining content after the last HorizontalRule
  table.insert(sections, current_section)
  -- We can then process the sections table as needed
  -- For this example, we just return the original doc, but in real-world usage
  -- you might modify and return parts of the sections as per your requirements
  return doc
end
