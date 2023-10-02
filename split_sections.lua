local count = 0

function HorizontalRule()
  count = count + 1
  local section_start = '<section id="pg' .. count .. '">\n'
  local nav_buttons = '</section>\n' ..
                      '<div class="navigation-buttons">\n' ..
                      '<button class="prev-btn">Previous</button>\n' ..
                      '<button class="next-btn">Next</button>\n' ..
                      '</div>\n'
  return pandoc.RawBlock("html", section_start .. nav_buttons)
end
