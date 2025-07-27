# surveydown (development version)

# surveydown 0.12.9

- Enhanced: `sd_store_value()` - Now session-persistent with database integration, compatible with local testing. It also saves the value into the server so it matches the database.
- Enhanced: Now cookie is working for both db mode and local csv.

# surveydown 0.12.8

- New argument for `sd_server()`: `capture_metadata` automatically captures and stores browser information and IP address. Defaults to `TRUE`.

# surveydown 0.12.7

- Bug fix: The `sd_redirect()` function used to trigger Shiny Client error of duplicated IDs for input and output. Now it's fixed by adding a unique ID to the button.
- Allow for environment variables to come from system (if available), PR #212

# surveydown 0.12.6

- UI update: Now `"matrix"` question type has better layout.
- Bug fix: `"text"`, `"textarea"` and `"numeric"` types could bypass the "required" question check with mouse clicking on their areas. Now it's fixed.

# surveydown 0.12.5

- New argument: `gssencmode` now has values of `"auto"`, `"prefer"`, and `"disable"`, with `"auto"` being default.
- New feature: Now re-rendering will be triggered by changing of either the `survey.qmd` or `app.R` file.
- Bug fix: `slider`, `slider_numeric`, `date`, and `daterage` questions now will be marked as answered without user interaction if the Next/Close button is pushed, and will have their default value saved.
- Bug fix: `mc_buttons` and `mc_multiple_buttons` now can properly save data.

# surveydown 0.12.4

- In `sd_question()`, `"numeric"` question type will only allow numeric inputs and `+`/`-` signs.
- Progress bar won't force to 100% if the ending page has questions. It is useful for those one-page surveys without any Next button or even finish button. In our previous versions, this case will force the bar to 100% even with unanswered questions.

# surveydown 0.12.3

- New argument for `sd_server()`: `highlight_unanswered` gives color shading for the unanswered questions, default to `TRUE`, can be turned off by changing to `FALSE`.
- New argument for `sd_server()`: `highlight_color` changes the shading color, default to `"gray"` (or `"grey"` as an acceptable spelling), can be changed to `"blue"`, `"orange"`, `"green"`, or `"purple"`.

# surveydown 0.12.2

- `sd_create_survey()` now accepts a new `ask` argument, defaults to `TRUE`. If `ask = FALSE`, the survey creation will proceed without asking.

# surveydown 0.12.1

- Bug fix: In the `0.11.1` version, `sd_show_if()` works for pages as well, but it will make `sd_skip_forward()` totally ignored. Now this is fixed.

# surveydown 0.12.0

- Required question highlighting: Now upon clicking the "Next" button, the unanswered required questions will be highlighted.

# surveydown 0.11.1

- `sd_create_survey()` now accepts `template` as the first argument, and `path` as the second. This follows intuition that we firstly choose a template, and then define the location.
- `sd_show_if()` now works for both questions and pages with the same syntax. Therefore, we also added an ID check for the uniqueness of the `page_id` and `question_id` altogether. If there is a duplicated ID, the survey will stop and show an error.

# surveydown 0.11.0

- Questions can now be defined using an external yml file, defaulting to `"questions.yml"`.
- New `yml` argument in `sd_question()`, defaults to `"questions.yml"`, to allow users which yml file to use for questions. For details, refer to the [Defining Questions](https://surveydown.org/docs/defining-questions) documentation page. 
- Update to `sd_create_survey()`: now the the `template` argument is by default `"default"`.
- Update to `sd_create_survey()`: new `template = "questions_yml"` option to create a survey with the `questions_yml` template.

# surveydown 0.10.1

- `sd_create_survey()` now accepts two arguments. The `template` argument is by default `"plain_template"`, which creates a default plain template of surveydown. It also accepts a list of templates that we created in the [templates repo](https://github.com/surveydown-dev/templates/tree/main). The `path` argument is unchanged. It defines the relative path of the template location.

# surveydown 0.10.0

- New feature: gadgets added for creating survey pages and questions. It can be triggered under the "Addins" menu on the top of RStudio window, or by keyboard shortcut. To set up keyboard shortcut, go to "Tools -> Addins -> Browse Addins..."
- Recommended page fence structure changed to: `.sd_page id=page_id`. The previous structure also works.
- Updated template (triggered by `sd_create_survey()`) to match with new recommended page structure.
- Remove `sd_show_password()` function entirely (previously was depreciated)
- Remove the dependency on `usethis` package
- Created internal `yesno()` function to replace `usethis::ui_yeah()`

# surveydown 0.9.1

- `sd_dashboard()` now accepts an argument of `gssencmode`, defaults to `"prefer"`. This is same logic as the `gssencmode` argument in `sd_db_connect()`.
- To further explain this update, both `sd_dashboard()` and `sd_db_connect()` accepts the `gssencmode` argument to embrace the different `gssencmode` situations of the internet connection. It is not embedded into `sd_db_config()` because `gssencmode` is not accessible for all connections (some may have `NULL`). This extra argument for `sd_dashboard()` and `sd_db_connect()` can be considered as a work-around.

# surveydown 0.9.0

- Overhauled how page skipping logic is handled for improved performance and consistency with different skipping conditions users provide.
- The `sd_skip_if()` function is depreciated and replaced with `sd_skip_forward()` for clarity of what the function does.
- Skipping logic is now strictly forward to avoid unintended skipping logic loops.

# surveydown 0.8.4

- Add support for `sd_skip_if()` to be able to use static values and reactive expressions as conditions.
- Add support for `sd_show_if()` to be able to use static values and reactive expressions as conditions.

# surveydown 0.8.3

- Added `sd_reactive()` function to store reactive values in the data, addressing #179.

# surveydown 0.8.2

- Added `"slider_numeric"` type to `sd_question()`, which supports both single and dual numeric sliders.
- Added the `...` argument to all question types in `sd_question()` so that users can pass other input arguments specific to each input type.
- Move the `gssencmode` parameter out of the database settings and made it an argument in `sd_db_connect()` so that users can pass `NULL` if needed to completely ignore it when making connections.

# surveydown 0.8.1

- Updated the template used in `sd_create_survey()` to match the new database configuration introduced in v0.8.0.
- Fixed [Issue 170](https://github.com/surveydown-dev/surveydown/issues/170): Now the matrix question type can have all subquestions defined as required with `sd_server(required_questions = "matrix_question")`.

# surveydown 0.8.0

- New function for setting up the database configuration: `sd_db_config()`. Stores database parameters in a local .env file. Replaces `sd_set_password()` and `sd_show_password()`, which are now depreciated.
- New function for connecting to database: `sd_db_connect`. Replaces older `sd_database()` function, which is now depreciated.
- New `sd_dashboard()` that locally runs an interactive shiny app for monitoring data in database.
- Bug fix: Fixed the slider question type options. They we previously in the opposite order from all other question types (label = value), see #166.

# surveydown 0.7.2

- Bug fix: The `mc_multiple` question type could not resume its UI if multiple options are selected. Now it's solved.
- New feature: a new `sd_question_custom()` function is created for custom question definition. See the [`leaflet-map`](https://github.com/surveydown-dev/templates/tree/main/custom_leaflet_map) and [`plotly`](https://github.com/surveydown-dev/templates/tree/main/custom_plotly_chart) template surveys for more details.

# surveydown 0.7.1

- `custom` type added for `sd_question()`. Now users can create customized question types according to their own needs. A demo of leaflet map is created as well to showcase this ability.

# surveydown 0.7.0

- Heavy revision to how css and us dependencies are loaded. Now the `sd_ui()` function handles the rendering of the `survey.qmd` file and extracting the header contents. All surveydown css and js dependencies are loaded via a simple lua filter when rendering. This simplifies how these dependencies get loaded into the resulting shiny app.

# surveydown 0.6.2

- Update: Now `ignore = TRUE` in `sd_server()` will turn off cookies, regardless of the value of `use_cookies`.

# surveydown 0.6.1

- Cookies now contain not only the `session_id` but a complete JSON object of the current page questions, answers, and latest time stamp.
- Increased checkpoints of data updates. Now they are: upon starting, upon proceeding to the next page, upon submitting rating or clicking the exit button, and finally, upon abruptly quitting the survey.

# surveydown 0.6.0

- Cookies functionality fully working, applied to both local testing and online db.
- Bug fix: The matrix subquestions used to be counted as the last questions in order regardless of their location in the survey. It's now solved.

# surveydown 0.5.2

- Cookies feature fixed and back online.
- Now upon window closure, the data will be immediately uploaded to db. A backup uploading will trigger upon session ends, which is usually after 5-10 secs upon closing the window.

# surveydown 0.5.1

- Revert mostly to v0.4.2 to completely remove the cookies feature added in v0.5.0. Will re-implement a new approach in a later version.
- Fixed typo in error message (`data.csv` -> `preview_data.csv`).

# surveydown 0.5.0

- New cookies feature! Now `sd_server()` accepts a new argument `use_cookies`, defaults to `TRUE`. It enables cookies so that reopening the survey will resume the participant to where the survey was left. The database will trace to the original `session_id` and continuous the data update based on user input. If changed to `FALSE`, the survey will start as brand new session upon reopening.
- Off-line data storage upgrade. Now if ignore mode is on, the data will be stored on the project directory with file name called `preview_data.csv`. This file works exactly the same as online database, and is compatible with cookies functionality.
- Simplified Chinese supported in `sd_server()`. You can trigger it by: `sd_server(language = "zh-CN")`.
- Improvement: default language behavior in UI functions cleaner when separately rendering survey.qmd file.
- Bug fix: There used to be a bug if multiple themes are defined in YAML. Now it's solved.

# surveydown 0.4.2

- New "translations" feature now supported (see PR #138). This allows system messages and date formats to be specified by a language using the `language` argument in `sd_server()`. Custom language messages can be further modified by providing a `translations.yml` file in the root project folder.
- Added new `sd_create_translations()` function to generate a template `translations.yml` file to use to edit the system messages.
- Added Stefan Munnes as contributor in DESCRIPTION file (`ctb`)
- Bug fix: The `is_matrix` metadata was not recorded for matrix sub-questions when exported into the `_survey/questions.yml` file, which caused an error if any matrix question was required. This is now corrected when obtaining the question structure from the stored `_survey/questions.yml` file.

# surveydown 0.4.1

- Modified survey rendering to move all rendered files into "_survey" folder.
- Export survey question metadata to "_survey/questions.yml" file (see #132).
- Survey content is now extracted and saved to '_survey/pages.rds', '_survey/head.rds', and '_survey/questions.yml' files for faster loading.
- Survey will load content from stored files if no changes detected in 'survey.qmd' or 'app.R' files.

# surveydown 0.4.0

- All examples updated to include run-able examples (where possible).
- Added example survey.qmd files in inst/examples (for use in function examples).
- All roxygen documentation reviewed for errors / typos.
- Removed `sd_update()` function.
- Removed `sd_deploy()` function.
- Installation instructions updated.

# surveydown 0.3.7

- Updated `sd_output()` to now be able to output the chosen question values, chosen question option label(s), and the question label itself. Addresses feature request #128.
- Enhance: `sd_update()` now directly force update the package without checking for version difference anymore.
- New: `sd_is_answered()` function to check if a question is answered or not and returns `TRUE` or `FALSE` accordingly. For `"matrix"` type, only if all sub-questions are answered will it be marked as `TRUE`.

# surveydown 0.3.6

- Enhance: `sd_add_question()` now has a `chunk` argument, if `TRUE` it will include the outer chunk wrapper. Default is `FALSE`.
- Enhance: The `matrix` question type now will have its root question id shown in the data sheet. This id is essential in other logic, for example it can be used as a handle for the `sd_show_if()` logic. This root id is also removed from the `all_question_required` logic, since it's only a handle and doesn't provide option to answer.
- Enhance: Removed the `use_html` argument in `sd_server()`. Now the survey.qmd file will always be rendered when the app runs only if one of two conditions are met: 1) the survey.html file is not detected, or 2) the survey.html file is outdated. Otherwise, it will use the rendered survey.html file.
- Bug fix: `auto_scroll` now works better and smoother.
- Bug fix: the enter key will not trigger page turn if working on `text`, `textarea`, `numeric`, `select`, `date`, and `daterange`.
- Enhance: the Enter key can now used to trigger the "Submit and Exit" button for the rating pop-up modal.

# surveydown 0.3.5

- Enhance: The database updating is simplified to only write to the database on each page turn and after the survey closes.
- Bug fix: if you added new questions or values to the survey after creating the initial database table, those new values would not have been added to the table. Now they are added.
- Enhance: `auto_scroll` changed to `FALSE` by default.

# surveydown 0.3.4

- Bug fix: reactive questions now work with show_if conditions too
- Bug fix: show_if conditions now work even if the target question is on a different page.
- Bug fix: reactive question ids (those defined in the server) were not being considered in `check_skip_show()` checks, so you'd get an error that the question id was invalid.
- Enhance: `sd_server()` accepts a new parameter called `"rate_survey"`, default to `FALSE`. If `TRUE`, the Exit button will trigger a rating question. If `FALSE`, the Exit button will only trigger a double confirmation to ensure the user really wants to exit.
- Enhance: Now the survey will check if there exists an `sd_close()` function call.
- Enhance: Now the survey will check if the reserved question IDs are used, including `session_id`, `time_start`, `time_end`, and `exit_survey_rating`. There could be more in the future.
- Enhance: Now the `sd_output()` function will take care of the styling setting of reactive questions. For example, `sd_output(id = "some_question_id", type = "question", width = "80%")` will define with of 80% for a reactive question.
- Enhance: Now the `sd_show_if()` can take care of cross-page conditional reactive questions.

# surveydown 0.3.3

- Enhance: `sd_server()` now has a new parameter called `auto_scroll`. It's default to `TRUE`, which enables auto scrolling that tracks the user's input, can be turned off by changing to `FALSE`. Thanks to the contribution from [Zain Hoda](https://github.com/zainhoda1).
- Enhance: `sd_question()` now has the `"matrix"` type. 
- Enhance: Asterisk, as an indication of required questions, is now moved to the top right corner of question containers.
- Enhance: Replaced the default shiny alert with `sweetalert`. 

# surveydown 0.3.2

- New: Added `sd_completion_code()` function.
- Bug fix: make stored values accessible with `sd_output()` with `type = 'value'` argument. Previously only question values could be displayed in the UI with `sd_output()`.
- New: `sd_add_question()` to instantly create a question template. Defaults to type of `"mc"` but also accepts all other types (`"text"`, `"textarea"`, `"numeric"`, etc.). The function call will delete itself after running.
- New: `sd_add_page()` to instantly create a page template. Make sure to run this function outside any division or code chunk. The function call will delete itself after running.

# surveydown 0.3.1

- Enhance: Improved efficiency in `database_uploading()` so only the changed fields get written, and also the writing happens after checking the `show_if` conditions (addresses #100).
- Enhance: Modified default rendering behavior to not delete the rendered html file.
- Enhance: Modified the `refresh_interval` argument in `sd_get_data()` as defaulting to `NULL`, which means the data will not be reactively fetched, regardless of the context it is used in. The data will only continuously refresh if `sd_get_data()` is called within a reactive context and `refresh_interval > 0`.
- Enhance: Modified messaging from `sd_set_password()` to not print out user's password and provide clearer instructions.
- New: `sd_show_password()` added to show a stored password. The user will be prompted to double confirm that they want to show it. If there is no password, the user will be prompted so, along with a message of using `sd_set_password()` to create the password.

# surveydown 0.3.0

- Introduced `sd_ui()` function to set placeholders for the shiny app ui.
- Heavily revised how `skip_if` and `show_if` works, removing `skip_if_custom` and `show_if_custom`. Now they work similar to the `case_when()` function, where you provide the formula `condition ~ target` for each condition in either function. These are also provided globally inside the `server()` function using `sd_skip_if()` and `sd_show_if()`.
- Require that the survey file be named `"survey.qmd"`.
- Added `sd_include_folder()` function so users can add a folder to the shiny resource path.
- Automatically include the `images`, `js`, `css`, and `www` folders as well as folders to quarto files to the shiny resource path when the package loads.
- Heavily modified how `sd_next()` works to improve page navigation and ensure that each `sd_next()` button has a unique id based on the current page.
- Removed the quarto extension.
- `sd_create_survey()` changed to sourcing template directly from the package. Two parameters are provided. The first parameter is `path`, which defines the relative path of the target. If left blank, the path will be the current working directory. The second parameter is `structure`, which defines which structure of the template the user wants to choose, default as `"single"` and can be changed to `"multi"`.
- `sd_deploy()` as a wrapper function of `rsconnect::deployApp()` to deploy the survey.
- `sd_update()` as a replacement of `sd_update_surveydown()` to update the package.
- `sd_version()` as a replacement of `sd_check_versions()` to check for the current version and the latest version.
- `sd_close()` function to create a close button for the survey.

# surveydown 0.2.4

- No need to use `<br>` above the Next button anymore. The Next buttons can now be in the same chunk as the questions as spacing has been added.
- Modified margins on top and bottom. Solved the large gap created by hidden questions.
- Fixed an issue with `sd_store_value()`. There used to be a problem if a value is a list with multiple entries. Now they are collapsed with commas due to the application of `format_question_value()` on the `value` variable.
- Removed the `reactive` argument for `sd_get_data()`. Now all functions that should be compatible with both reactive (server) and static (ui) conditions will automatically match, without necessity of explicitly specifying them.
- `sd_redirect()` updated with a parameter called `newtab`, defaults to `FALSE`. If `TRUE`, the redirection link will be opened in a new tab. If used together with `delay`, the new tab might be blocked by the browser after count-down, but the user click will not be blocked.
- Improved the `database_uploading()` function to secure it from possible SQL injection attacks.

# surveydown 0.2.3

- Solved the speed problem for database connections: refactored `sd_server()` for efficiency; converted local data storage to lists instead of data frames.
- Assigned `session_id` as the leading column, and removed the `respondent_id` column.

# surveydown 0.2.2

- `sd_redirect()` to create redirection to external links, either by providing a button, a countdown, or both. This function can be called both in the survey body and the server chunk.
- `sd_get_url_pars()` to reactively get the parameters in the URL.
- The above 3 functions work together to retrieve and redirect URL parameters so that surveydown is compatible with survey panels.
- Now `sd_next()` and `sd_redirect()` both support the "Enter" key for a better user experience.
- Add automatic clean up in `sd_database()`.
- `sd_output()` function that replaces the original `sd_display_question()` (by specifying `type = "question"`) and `sd_display_value()` (by specifying `type = "value"`). The original 2 functions are depreciated and will be removed in future updates. If `sd_output()` is only used with `"id"` specified, it works the same as `shiny::uiOutput()`.

# surveydown 0.2.1

- In `sd_database()`, now `db_name` is changed to `dbname`, and `table_name` is changed to `table`, for consistency with Supabase, and simplicity of parameter names.
- Now the error message of `sd_database()` will also prompt "If you have verified all connection details are correct but still cannot access the database, consider setting the `gssencmode` parameter to `"disable"` in the `sd_database()` function." We don't encourage users to disable `gssencmode`, since it's more secure to have it on, but if users encounter with connection error, this will be a possible solution.
- Bug fix: A bug was introduced when restructuring the `sd_server()` function in the last update that affected numeric questions being determined as answered or not, that is now fixed.
- Fixed [issue #80](https://github.com/surveydown-dev/surveydown/issues/80): If `sd_display_value()` was used multiple times on different question IDs, it could only fetch the last user input. Now it's fixed.
- Renamed main lua file in `inst/quarto/filters` from `surveydown.lua` to `sd_main.lua` and updated the extension v0.2.5 to call this via a more robust, cross-platform approach.
- (Extension) Renamed the extension lua filter to `sd_wrapper.lua` to avoid naming conflict. Now the extension is only used as a wrapper. The `sd_main.lua` and related CSS and JS files are all in the R package.

# surveydown 0.2.0

- Moved `sd_store_value()` and `sd_copy_value()` to the server.R file (these are server operations).
- Added `respondent_id` variable that tracks the respondent based on their starting the survey.
- Restructure the server to be more efficient.
- Now the required questions can successfully bypass the hidden required questions of all types.

# surveydown 0.1.2

- Moved the main surveydown.lua file into the package at inst/quarto/filters. The surveydown Quarto extension is now just a wrapper to load this file, so the extension won't likely need to update any further as all updates can be made in the package lua file.
- Changed `jhelvy` to `surveydown-dev` in version control functions.
- Now if there are duplicated `page_id` and `question_id`, the survey will stop and show error.
- Removed roxygen2 documentation from non-exported functions.

# surveydown 0.1.1

- Added `sd_get_data()` with a `reactive` argument to enable reactive data fetching.
- `sd_question()` now has a `reactive` argument to enable reactive questions, and `sd_question_reactive()` was removed.
- Made `sd_admin_enable()` internal, changed to `admin_enable()`.
- Removed `sd_admin_ui()` and `sd_add_admin()` from the package.
- Moved the surveydown.css and page_nav.js files into the R package and out of the Quarto extension.
- Added keep alive functionality to the survey.
- Bug fixes: 
  - The admin page password was looking for the `SUPABASE_PASSWORD` environment variable, but it should be `SURVEYDOWN_PASSWORD`.
  - The data fetching was not working to download the data as a CSV file in the admin page, now uses `sd_get_data()` and works.
  - The `show_if_custom` logic was not working for multiple conditions, now it does.
  - The `skip_if_custom` logic could error if a condition was `NULL` (fixed with `isTRUE()`).
  - In `sd_config()`, items were being assigned with the ` <- ` operator, causing them to be deleted when the thing being assigned was `NULL`. Now changed to ` = `.

# surveydown 0.1.0

- Added `sd_display_value()` for displaying a question value based on the question `id`.
- Added `sd_copy_value()` for making a copy of a question value, which may be needed if you need to display something more than once since you can't make more than one object with the same `id`.
- Changed `sd_reactive_output()` to `sd_display_question()`.
- Changed how `sd_store_value()` works to avoid a global environment, and changed the arg name from `name` to `id`.
- Added `sd_add_admin()` function to react to admin_page condition, if set to true an 'Admin' page will be created for survey creators.
- Added `sd_admin_ui()` function to create the UI for the Admin page button.
- (extension) JS files removed from extension to R package. Now the extension only contains the `.lua` file and the css styles.

# surveydown 0.0.9

- Required questions are now moved to `sd_config()` as a separate `required_questions` argument, instead of being defined in `sd_question()`.
- Two new functions for supporting reactive questions: `sd_question_reactive()` and `sd_reactive_output()`.
- `sd_store_value()` function to store variables into the database. For example, `sd_store_value(respondentID, "respID")` will store the value of `respondentID` into a column named `"respID"`. If the name is not provided, the column will be named as its original variable name, in this case, `respondentID`.
- Enhanced robustness for progress bar. Now the progress bar will update based on user's input.
- Added a `timestamp` column in the beginning of the result dataframe.
- Now both Mac and PC will have no problem connecting with supabase.
- Updated all roxygen2 documentation texts.

# surveydown 0.0.8

- `sd_database()` function added with `pause` argument default to `FALSE`. If `pause = FALSE`, database will be properly connected; if `pause = TRUE`, a local CSV will be generated and survey results will be stored there.
- `sd_config()` function has `preview` removed due to `pause` in `sd_database`.
- For `pause = FALSE` (aka default), warning messages will be shown for missing or incorrect password, and will prompt the usage of `sd_set_password()`.
- Initiation of the `admin_page` argument in `config.R`.

# surveydown 0.0.7

- `sd_set_password()` function to set the supabase password as the survey environment variable. This function takes in a string, which should be your supabase password. We recommend you to only run it in the R Console so that your password does not appear in the `.qmd` file.
- (Continue) Upon running `sd_set_password()`, an `.Renviron` file will be created in your survey project root directory. In this file, `SUPABASE_PASSWORD=your_password` will be created, with `your_password` being whatever your input of `sd_set_password()`. Then, `.Renviron` will be added to your `.gitignore` file to avoid being pushed to GitHub.
- (Continue) If there is already an `.Renviron` file, `SUPABASE_PASSWORD=your_password` will be concatenated to the end. If there is already a definition of `SUPABASE_PASSWORD`, it will be overwritten. If there is no `.gitignore` file, it will be created. If there is already an `.Renviron` in `.gitignore`, it won't be duplicated.
- (Continue) All the above explanation means that you simply run `sd_set_password()` once to define supabase password for your survey project. It takes care of the rest of necessary operations, and you can rerun `sd_set_password()` to change the password, with the previous value safely overwritten.

# surveydown 0.0.6

- In `sd_database`, a `gssencmode` argument is added and set to "prefer" by default. In some cases, local deployment may fail due to network environments such as VPN settings. It can be solved by setting `gssencmode = "disable"` in the survey `qmd` file.
- Now the survey can be updated even AFTER its deployment. You may add, delete, or modify a question. The old question column and its time stamp, will stay where they were. The new question column and time stamp will be concatenated in the end of the table.
- (extension) Now the `example.qmd` survey has instructions for supabase configuration and shinyapps deployment.
- (extension) Now the `.gitignore` file has `.Renviron` included. This file will store supabase password and is essential for shinyapps deployment. Eliminating this file from pushing to GitHub will ensure that your password is only saved locally.


# surveydown 0.0.5

- `create_survey()` changed to `sd_create_survey()` for function name consistency.
- `sd_update_extension()` function to update the surveydown Quarto extension.
- `sd_check_versions()` function to check the version of both the R package and the Quarto extension. If any of them don't match with the latest version, there will be a suggestion to run the `sd_update_surveydown()` function.
- `sd_update_surveydown()` function to update both the R package and the Quarto extension.

# surveydown 0.0.4

- (extension) [Raleway Font](https://fonts.google.com/specimen/Raleway) is set as the default font of the survey pages. Can be overwritten in the YAML header with the `theme` command using the [bootswatch themes](https://bootswatch.com) or a custom `scss` file.
- (extension) Default page background is set as "#f2f6f9" (a light blue color). Can be overwritten in the YAML header with the `backgroundcolor`.
- (extension) Questions are now encapsulated in a container that distinguishes from the descriptive texts.
- (extension) Options of `mc_button` and `mu_multiple_buttons` types of questions are now centered.
- Now the `surveydown::create_survey()` function will download the whole extension repo, containing the extension, an example survey, and an RStudio project.


# surveydown 0.0.3

- Now `show_if` can be applied to multiple options of the same question, which allows users to have more than one option in a question that triggers the revealing of a hidden question.
- Now if the "required" question is hidden, it will not trigger the page warning of "not all questions are answered".
- (extension) Now users can have a page with the same name of its ID and its header, regardless of capitalization. For example, users can now have a page ID being "#educational", and its header being "#Educational". This sounds as it should be, but in previous versions, the page hiding logic will show bug if page ID is the same as page header.

# surveydown 0.0.2

- Support added for the `required` argument in `sd_question()`, which allows users to force a question to be required. If not answered, a popup window blocks the user from continuing to the next page.
- The supabase backend is now connected and documentation on how to set up the database is complete.
- Initial setup for tests added

# surveydown 0.0.1

Initial version!
