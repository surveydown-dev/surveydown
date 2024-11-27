get_translations <- function() {
  path <- file.path('_survey', 'translations.yml')
  if (fs::file_exists(path)) {
    return(yaml::read_yaml("_survey/translations.yml"))
  }
  # If 'app.R' has not yet been run, then no `_survey/translations.yml` file
  # will exist. In this case, just return the English as default.
  return(get_translations_default()[['en']])
}

get_translations_default <- function() {
  return(list(
    "en" = list(
      #server.R
      "cancel" = 'Cancel',
      "confirm_exit" = 'Confirm Exit',
      "sure_exit" = 'Are you sure you want to exit the survey?',
      "submit_exit" = 'Submit and Exit',
      "warning" = 'Warning',
      "required" = 'Please answer all required questions before proceeding.',
      "rating_title" = 'Before you go...',
      "rating_text"  = 'Rate your survey experience:',
      "rating_scale" = 'from 1-poor to 5-excellent',
      # ui.R
      "next" = 'Next',
      "exit" = 'Exit Survey',
      "close_tab" = 'Please close this tab manually to exit the survey.',
      "choose_option" = "Choose an option...",
      "click" = 'Click here',
      "redirect" = 'Redirecting in',
      "seconds" = 'seconds',
      "new_tab" = 'Opens in a new tab',
      "redirect_error" = "Error: This text won't trigger any redirection..."
    ),
    "de" = list(
      #server.R
      "cancel" = 'Abbrechen',
      "confirm_exit" = 'Beenden best\u00E4tigen',
      "sure_exit" = 'Sind Sie sicher, dass Sie die Umfrage beenden m\u00F6chten?',
      "submit_exit" = 'Absenden und beenden',
      "warning" = 'Warnung',
      "required" = 'Bitte beantworten Sie alle erforderlichen Fragen, bevor Sie fortfahren.',
      "rating_title" = 'Bevor Sie gehen...',
      "rating_text" = 'Bewerten Sie Ihre Umfrageerfahrung:',
      "rating_scale" = 'von 1-schlecht bis 5-ausgezeichnet',
      # ui.R
      "next" = 'Weiter',
      "exit" = 'Umfrage beenden',
      "close_tab" = 'Bitte schlie\u00DFen Sie diesen Tab manuell, um die Umfrage zu beenden.',
      "choose_option" = "Option ausw\u00E4hlen...",
      "click" = 'Hier klicken',
      "redirect" = 'Weiterleitung in',
      "seconds" = 'Sekunden',
      "new_tab" = '\u00D6ffnet sich in einem neuen Tab',
      "redirect_error" = "Fehler: Dieser Text wird keine Weiterleitung ausl\u00F6sen..."
    ),
    "es" = list(
      #server.R
      "cancel" = 'Cancelar',
      "confirm_exit" = 'Confirmar Salida',
      "sure_exit" = '\u00BFEst\u00E1 seguro de que desea salir de la encuesta?',
      "submit_exit" = 'Enviar y Salir',
      "warning" = 'Advertencia',
      "required" = 'Por favor, responda todas las preguntas obligatorias antes de continuar.',
      "rating_title" = 'Antes de irse...',
      "rating_text"  = 'Califique su experiencia en la encuesta:',
      "rating_scale" = 'de 1-malo a 5-excelente',
      # ui.R
      "next" = 'Siguiente',
      "exit" = 'Salir de la Encuesta',
      "close_tab" = 'Por favor, cierre esta pesta\u00F1a manualmente para salir de la encuesta.',
      "choose_option" = "Elija una opci\u00F3n...",
      "click" = 'Haga clic aqu\u00ED',
      "redirect" = 'Redireccionando en',
      "seconds" = 'segundos',
      "new_tab" = 'Se abre en una nueva pesta\u00F1a',
      "redirect_error" = "Error: Este texto no activar\u00E1 ninguna redirecci\u00F3n..."
    ),
    "fr" = list(
      #server.R
      "cancel" = 'Annuler',
      "confirm_exit" = 'Confirmer la sortie',
      "sure_exit" = '\u00CAtes-vous s\u00FBr de vouloir quitter le sondage?',
      "submit_exit" = 'Soumettre et quitter',
      "warning" = 'Avertissement',
      "required" = 'Veuillez r\u00E9pondre \u00E0 toutes les questions obligatoires avant de continuer.',
      "rating_title" = 'Avant de partir...',
      "rating_text"  = '\u00C9valuez votre exp\u00E9rience du sondage :',
      "rating_scale" = 'de 1-mauvais \u00E0 5-excellent',
      # ui.R
      "next" = 'Suivant',
      "exit" = 'Quitter le sondage',
      "close_tab" = 'Veuillez fermer cet onglet manuellement pour quitter le sondage.',
      "choose_option" = "Choisissez une option...",
      "click" = 'Cliquez ici',
      "redirect" = 'Redirection dans',
      "seconds" = 'secondes',
      "new_tab" = "S'ouvre dans un nouvel onglet",
      "redirect_error" = "Erreur : Ce texte n'activera aucune redirection..."
    ),
    "it" = list(
      #server.R
      "cancel" = 'Annulla',
      "confirm_exit" = 'Conferma Uscita',
      "sure_exit" = 'Sei sicuro di voler uscire dal sondaggio?',
      "submit_exit" = 'Invia ed Esci',
      "warning" = 'Avviso',
      "required" = 'Per favore, rispondi a tutte le domande obbligatorie prima di procedere.',
      "rating_title" = 'Prima di andare...',
      "rating_text"  = 'Valuta la tua esperienza con il sondaggio:',
      "rating_scale" = 'da 1-scarso a 5-eccellente',
      # ui.R
      "next" = 'Avanti',
      "exit" = 'Esci dal Sondaggio',
      "close_tab" = 'Per favore, chiudi questa scheda manualmente per uscire dal sondaggio.',
      "choose_option" = "Scegli un'opzione...",
      "click" = 'Clicca qui',
      "redirect" = 'Reindirizzamento in',
      "seconds" = 'secondi',
      "new_tab" = 'Si apre in una nuova scheda',
      "redirect_error" = "Errore: Questo testo non attiver\u00E0 alcun reindirizzamento..."
    )
  ))
}

get_valid_languages <- function() {
  return(c(
    "ar", "az", "bg", "bs", "ca", "cs", "cy", "da", "de", "el", "en",
    "en-AU", "en-GB", "eo", "es", "et", "eu", "fa", "fi", "fo", "fr-CH",
    "fr", "gl", "he", "hr", "hu", "hy", "id", "is", "it-CH", "it", "ja",
    "ka", "kh", "kk", "ko", "kr", "lt", "lv", "me", "mk", "mn", "ms", "nb",
    "nl-BE", "nl", "no", "pl", "pt-BR", "pt", "ro", "rs-latin", "rs", "ru",
    "sk", "sl", "sq", "sr-latin", "sr", "sv", "sw", "th", "tr", "uk", "vi",
    "zh-CN", "zh-TW"
  ))
}
