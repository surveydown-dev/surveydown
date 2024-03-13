from shiny import App, render, ui

# Define the custom functions equivalent to sd_question and sd_next in Python
def sd_question(name, qtype, label, options=None, dependence=None, dependence_value=None):
    if qtype == "select":
        return ui.select_input(name, label, options)
    elif qtype == "mc":
        return ui.radio_buttons(name, label, options)
    elif qtype == "text":
        return ui.text_input(name, label)

def sd_next(page_id, label='Next'):
    return ui.button(f"next{page_id}", label)
