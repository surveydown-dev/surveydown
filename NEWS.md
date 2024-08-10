# surveydown (development version)

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
