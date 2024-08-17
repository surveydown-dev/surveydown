# surveydown (development version)

- Added `sd_display_value()` for displaying a question value based on the question `id`.
- Added `sd_copy_value()` for making a copy of a question value, which may be needed if you need to display something more than once since you can't make more than one object with the same `id`.
- Changed `sd_reactive_output()` to `sd_display_question()`.
- Changed how `sd_store_value()` works to avoid a global environment, and changed the arg name from `name` to `id`.
- `sd_add_admin` function to react to admin_page condition, if set to true an 'Admin' page will be created for survey creators.
- `sd_admin_ui` creates the UI for the Admin page button.
- `hide_sd_pages` function to hide all classes with the tag ".sd-page".
- `show_sd_pages` function to show all classes with the tag ".sd-page".

**Admin Page** 

1. Creation: Admin page will be created by setting `admin_page` to TRUE inside the `sd_config()` function.
2. Login: Consists of a single "Password" parameter, this is set to your `SUPABASE_PASSWORD` which was previously used for db setup.
3. Admin Actions: \
    3.1. Pause Survey: Once clicked, the shiny app pauses all instances replacing questions with a placeholder "Paused Survey" page. **Status: Non-functional** \
    3.2. Pause DB: Once clicked, the db arugment for `sd_server` will be set to NULL pausing all database uploads. **Status: Non-functional** \
    3.3. Download Survey: Once clicked, the most recent table instance is pulled from the database and downloaded locally as a .csv file. **Status: Functional** \
    3.4. Admin Logout and Return to Survey: Once clicked, the admin page will derender itself and return to start of survey. **Status: Functional** \
    3.5. Survey Data: Pulls most recent instance of the database and displays the data. **Status: Functional**

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
- Now the `surveydown::create_survey()` function will download the whole [extension repo](https://github.com/jhelvy/surveydown-ext/tree/main), containing the extension, an example survey, and an RStudio project.


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
