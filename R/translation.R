get_translations <- function() {
    # Initialize translations list from '_survey/translations.yml' file
    translations <- get_translations_yml()
    if (is.null(translations)) {
        # '_survey/translations.yml' file missing, so just load English
        translations <- get_translations_default()
        language <- 'en'
    } else {
        language <- names(translations)
    }
    return(list(
        translations = translations[[language]],
        language = language
    ))
}

get_translations_yml <- function() {
    path <- file.path("_survey", "translations.yml")
    if (fs::file_exists(path)) {
        return(yaml::read_yaml("_survey/translations.yml"))
    }
    return(NULL)
}

get_translations_default <- function() {
    return(list(
        "en" = list(
            #server.R
            "cancel" = "Cancel",
            "confirm_exit" = "Confirm Exit",
            "sure_exit" = "Are you sure you want to exit the survey?",
            "submit_exit" = "Submit and Exit",
            "warning" = "Warning",
            "required" = "Please answer all required questions before proceeding.",
            "rating_title" = "Before you go...",
            "rating_text" = "Rate your survey experience:",
            "rating_scale" = "from 1-poor to 5-excellent",
            # ui.R
            "previous" = "Previous",
            "next" = "Next",
            "exit" = "Exit Survey",
            "close_tab" = "Please close this tab manually to exit the survey.",
            "choose_option" = "Choose an option...",
            "click" = "Click here",
            "redirect" = "Redirecting in",
            "seconds" = "seconds",
            "new_tab" = "Opens in a new tab",
            "redirect_error" = "Error: This text won't trigger any redirection..."
        ),
        "de" = list(
            #server.R
            "cancel" = "Abbrechen",
            "confirm_exit" = "Beenden best\u00E4tigen",
            "sure_exit" = "Sind Sie sicher, dass Sie die Umfrage beenden m\u00F6chten?",
            "submit_exit" = "Absenden und beenden",
            "warning" = "Warnung",
            "required" = "Bitte beantworten Sie alle erforderlichen Fragen, bevor Sie fortfahren.",
            "rating_title" = "Bevor Sie gehen...",
            "rating_text" = "Bewerten Sie Ihre Umfrageerfahrung:",
            "rating_scale" = "von 1-schlecht bis 5-ausgezeichnet",
            # ui.R
            "previous" = "Zur\u00FCck",
            "next" = "Weiter",
            "exit" = "Umfrage beenden",
            "close_tab" = "Bitte schlie\u00DFen Sie diesen Tab manuell, um die Umfrage zu beenden.",
            "choose_option" = "Option ausw\u00E4hlen...",
            "click" = "Hier klicken",
            "redirect" = "Weiterleitung in",
            "seconds" = "Sekunden",
            "new_tab" = "\u00D6ffnet sich in einem neuen Tab",
            "redirect_error" = "Fehler: Dieser Text wird keine Weiterleitung ausl\u00F6sen..."
        ),
        "es" = list(
            #server.R
            "cancel" = "Cancelar",
            "confirm_exit" = "Confirmar Salida",
            "sure_exit" = "\u00BFEst\u00E1 seguro de que desea salir de la encuesta?",
            "submit_exit" = "Enviar y Salir",
            "warning" = "Advertencia",
            "required" = "Por favor, responda todas las preguntas obligatorias antes de continuar.",
            "rating_title" = "Antes de irse...",
            "rating_text" = "Califique su experiencia en la encuesta:",
            "rating_scale" = "de 1-malo a 5-excelente",
            # ui.R
            "previous" = "Anterior",
            "next" = "Siguiente",
            "exit" = "Salir de la Encuesta",
            "close_tab" = "Por favor, cierre esta pesta\u00F1a manualmente para salir de la encuesta.",
            "choose_option" = "Elija una opci\u00F3n...",
            "click" = "Haga clic aqu\u00ED",
            "redirect" = "Redireccionando en",
            "seconds" = "segundos",
            "new_tab" = "Se abre en una nueva pesta\u00F1a",
            "redirect_error" = "Error: Este texto no activar\u00E1 ninguna redirecci\u00F3n..."
        ),
        "fr" = list(
            #server.R
            "cancel" = "Annuler",
            "confirm_exit" = "Confirmer la sortie",
            "sure_exit" = "\u00CAtes-vous s\u00FBr de vouloir quitter le sondage?",
            "submit_exit" = "Soumettre et quitter",
            "warning" = "Avertissement",
            "required" = "Veuillez r\u00E9pondre \u00E0 toutes les questions obligatoires avant de continuer.",
            "rating_title" = "Avant de partir...",
            "rating_text" = "\u00C9valuez votre exp\u00E9rience du sondage :",
            "rating_scale" = "de 1-mauvais \u00E0 5-excellent",
            # ui.R
            "previous" = "Pr\u00E9c\u00E9dent",
            "next" = "Suivant",
            "exit" = "Quitter le sondage",
            "close_tab" = "Veuillez fermer cet onglet manuellement pour quitter le sondage.",
            "choose_option" = "Choisissez une option...",
            "click" = "Cliquez ici",
            "redirect" = "Redirection dans",
            "seconds" = "secondes",
            "new_tab" = "S'ouvre dans un nouvel onglet",
            "redirect_error" = "Erreur : Ce texte n'activera aucune redirection..."
        ),
        "it" = list(
            #server.R
            "cancel" = "Annulla",
            "confirm_exit" = "Conferma Uscita",
            "sure_exit" = "Sei sicuro di voler uscire dal sondaggio?",
            "submit_exit" = "Invia ed Esci",
            "warning" = "Avviso",
            "required" = "Per favore, rispondi a tutte le domande obbligatorie prima di procedere.",
            "rating_title" = "Prima di andare...",
            "rating_text" = "Valuta la tua esperienza con il sondaggio:",
            "rating_scale" = "da 1-scarso a 5-eccellente",
            # ui.R
            "previous" = "Indietro",
            "next" = "Avanti",
            "exit" = "Esci dal Sondaggio",
            "close_tab" = "Per favore, chiudi questa scheda manualmente per uscire dal sondaggio.",
            "choose_option" = "Scegli un'opzione...",
            "click" = "Clicca qui",
            "redirect" = "Reindirizzamento in",
            "seconds" = "secondi",
            "new_tab" = "Si apre in una nuova scheda",
            "redirect_error" = "Errore: Questo testo non attiver\u00E0 alcun reindirizzamento..."
        ),
        "zh-CN" = list(
            #server.R
            "cancel" = "\u53d6\u6d88", # 取消
            "confirm_exit" = "\u786e\u8ba4\u9000\u51fa", # 确认退出
            "sure_exit" = "\u60a8\u786e\u8ba4\u8981\u9000\u51fa\u5417\uff1f", # 您确认要退出吗？
            "submit_exit" = "\u63d0\u4ea4\u5e76\u9000\u51fa", # 提交并退出
            "warning" = "\u8b66\u544a", # 警告
            "required" = "\u8bf7\u56de\u7b54\u6240\u6709\u5fc5\u586b\u95ee\u9898\u3002", # 请回答所有必填问题。
            "rating_title" = "\u7a0d\u7b49\u4e00\u4e0b\u2026", # 稍等一下…
            "rating_text" = "\u8bf7\u60a8\u4e3a\u6b64\u6b21\u95ee\u5377\u6253\u5206\uff1a", # 请您为此次问卷打分：
            "rating_scale" = "1\u4e3a\u975e\u5e38\u4e0d\u6ee1\u610f\uff0c5\u4e3a\u975e\u5e38\u6ee1\u610f", # 1为非常不满意，5为非常满意
            # ui.R
            "previous" = "\u4e0a\u4e00\u9875", # 上一页
            "next" = "\u4e0b\u4e00\u9875", # 下一页
            "exit" = "\u9000\u51fa\u95ee\u5377", # 退出问卷
            "close_tab" = "\u8bf7\u624b\u52a8\u5173\u95ed\u672c\u9875\u9762\u3002", # 请手动关闭本页面。
            "choose_option" = "\u8bf7\u9009\u62e9\u4e00\u9879\u2026", # 请选择一项…
            "click" = "\u5355\u51fb\u6b64\u5904", # 单击此处
            "redirect" = "\u8df3\u8f6c\u5012\u8ba1\u65f6", # 跳转倒计时
            "seconds" = "\u79d2", # 秒
            "new_tab" = "\u5728\u65b0\u9875\u9762\u5f00\u542f", # 在新页面开启
            "redirect_error" = "\u9519\u8bef\uff1a\u6b64\u6587\u672c\u65e0\u6cd5\u89e6\u53d1\u4efb\u4f55\u8df3\u8f6c\u2026" # 错误：此文本无法触发任何跳转…
        )
    ))
}

get_valid_languages <- function() {
    return(c(
        "ar",
        "az",
        "bg",
        "bs",
        "ca",
        "cs",
        "cy",
        "da",
        "de",
        "el",
        "en",
        "en-AU",
        "en-GB",
        "eo",
        "es",
        "et",
        "eu",
        "fa",
        "fi",
        "fo",
        "fr-CH",
        "fr",
        "gl",
        "he",
        "hr",
        "hu",
        "hy",
        "id",
        "is",
        "it-CH",
        "it",
        "ja",
        "ka",
        "kh",
        "kk",
        "ko",
        "kr",
        "lt",
        "lv",
        "me",
        "mk",
        "mn",
        "ms",
        "nb",
        "nl-BE",
        "nl",
        "no",
        "pl",
        "pt-BR",
        "pt",
        "ro",
        "rs-latin",
        "rs",
        "ru",
        "sk",
        "sl",
        "sq",
        "sr-latin",
        "sr",
        "sv",
        "sw",
        "th",
        "tr",
        "uk",
        "vi",
        "zh-CN",
        "zh-TW"
    ))
}
