-- Function to get file paths using system.file from R
function get_package_file(file, type)
    local cmd = string.format('Rscript -e "cat(system.file(\'%s/%s\', package = \'surveydown\'))"', type, file)
    local handle = io.popen(cmd)
    local path = handle:read("*a")
    handle:close()
    return path
end

function Meta(meta)
    -- Define standard resources
    local js_files = {
        "auto_scroll.js",
        "cookies.js",
        "countdown.js",
        "enter_key.js",
        "keep_alive.js",
        "update_progress.js"
    }

    local css_files = {
        "surveydown.css"
    }

    -- Add default theme if not explicitly disabled
    if meta.default_theme ~= false then
        table.insert(css_files, "default_theme.css")
    end

    -- Initialize header-includes if it doesn't exist
    meta['header-includes'] = meta['header-includes'] or pandoc.MetaList{}

    -- Add JavaScript files
    for _, file in ipairs(js_files) do
        local path = get_package_file(file, "js")
        if path ~= "" then
            meta['header-includes'][#meta['header-includes'] + 1] =
                pandoc.RawBlock('html', string.format('<script src="%s"></script>', path))
        end
    end

    -- Add CSS files
    for _, file in ipairs(css_files) do
        local path = get_package_file(file, "css")
        if path ~= "" then
            meta['header-includes'][#meta['header-includes'] + 1] =
                pandoc.RawBlock('html', string.format('<link rel="stylesheet" href="%s">', path))
        end
    end

    return meta
end
