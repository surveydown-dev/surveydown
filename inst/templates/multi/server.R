server <- function(input, output, session) {

  # Reactive definitions here...

  config <- sd_config(
    # Config settings here...
  )

  # sd_server() initiates your survey - don't change it
  sd_server(
    input   = input,
    output  = output,
    session = session,
    config  = config,
    db      = db
  )

}
