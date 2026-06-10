#!/usr/bin/env bash
# update_templates.sh
#
# Applies common changes to all surveydown template repos in the
# surveydown-dev GitHub org. Uses the GitHub API via gh CLI — no cloning
# required.
#
# Usage:
#   bash inst/scripts/update_templates.sh           # dry run (prints diffs only)
#   bash inst/scripts/update_templates.sh --push    # apply and push to GitHub
#
# Requirements: gh CLI authenticated with write access to surveydown-dev org.

set -euo pipefail

PUSH=false
if [[ "${1:-}" == "--push" ]]; then
  PUSH=true
fi

ORG="surveydown-dev"

TEMPLATES=(
  "conditional_showing"
  "conditional_skipping"
  "conditional_stopping"
  "conjoint_buttons"
  "conjoint_tables"
  "custom_leaflet_map"
  "custom_plotly_chart"
  "default"
  "external_redirect"
  "live_polling"
  "option_shuffling"
  "question_types"
  "questions_yml"
  "random_options"
  "random_options_predefined"
  "reactive_drilldown"
  "reactive_questions"
)

# ---------------------------------------------------------------------------
# patch_app_r <repo_name> <current_content>
# Prints the patched app.R content to stdout.
# ---------------------------------------------------------------------------
patch_app_r() {
  local content="$1"

  # Replace the ignore = TRUE comment block and call with the new mode approach.
  # The old block spans from "For this demo..." through "sd_db_connect(ignore = TRUE)".
  python3 - "$content" <<'PYEOF'
import sys, re

content = sys.argv[1]

old_block = (
    r"# For this demo, we set ignore = TRUE in the following code, which will ignore\n"
    r"# the connection settings and won't attempt to connect to the database. This is\n"
    r"# helpful if you don't want to record testing data in the database table while\n"
    r"# doing local testing. Once you're ready to collect survey responses, set\n"
    r"# ignore = FALSE or just delete this argument.\n"
    r"\n"
    r"db <- sd_db_connect\(ignore = TRUE\)"
)

new_block = (
    "# This template runs in preview mode (set via `mode: preview` in survey.qmd),\n"
    "# which saves responses locally instead of to a database. To collect real\n"
    "# responses, run sd_db_config() to store your database credentials, then\n"
    "# change `mode` to `database` in the survey.qmd YAML header.\n"
    "\n"
    "db <- sd_db_connect()"
)

result = re.sub(old_block, new_block, content)
print(result, end="")
PYEOF
}

# ---------------------------------------------------------------------------
# patch_survey_qmd <content>
# Adds `mode: preview` into the survey-settings block if not already present.
# ---------------------------------------------------------------------------
patch_survey_qmd() {
  local content="$1"

  python3 - "$content" <<'PYEOF'
import sys, re

content = sys.argv[1]

# Skip if mode already set
if re.search(r'^\s+mode\s*:', content, re.MULTILINE):
    print(content, end="")
    sys.exit(0)

# Insert `mode: preview` as the first key under survey-settings:
# Find the line "survey-settings:" and insert after it, matching indentation
# of the next key.
def insert_mode(m):
    block_header = m.group(0)
    return block_header + "\n  mode: preview"

result = re.sub(r'^survey-settings:', insert_mode, content, flags=re.MULTILINE, count=1)
print(result, end="")
PYEOF
}

# ---------------------------------------------------------------------------
# update_file <repo> <file_path> <new_content> <commit_message>
# ---------------------------------------------------------------------------
update_file() {
  local repo="$1"
  local file_path="$2"
  local new_content="$3"
  local commit_msg="$4"

  # Get current file SHA (needed by the API to update)
  local sha
  sha=$(gh api "repos/${ORG}/${repo}/contents/${file_path}" --jq '.sha' 2>/dev/null || true)

  if [[ -z "$sha" ]]; then
    echo "  [SKIP] ${file_path} not found in ${repo}"
    return
  fi

  local encoded
  encoded=$(printf '%s' "$new_content" | base64)

  gh api \
    --method PUT \
    "repos/${ORG}/${repo}/contents/${file_path}" \
    --field "message=${commit_msg}" \
    --field "content=${encoded}" \
    --field "sha=${sha}" \
    --silent

  echo "  [OK]   ${file_path} updated"
}

# ---------------------------------------------------------------------------
# Main loop
# ---------------------------------------------------------------------------
for tmpl in "${TEMPLATES[@]}"; do
  repo="template_${tmpl}"
  echo ""
  echo "=== ${repo} ==="

  # --- app.R ---
  raw_app=$(gh api "repos/${ORG}/${repo}/contents/app.R" --jq '.content' | base64 -d)
  new_app=$(patch_app_r "$raw_app")

  # --- survey.qmd ---
  raw_qmd=$(gh api "repos/${ORG}/${repo}/contents/survey.qmd" --jq '.content' | base64 -d)
  new_qmd=$(patch_survey_qmd "$raw_qmd")

  if [[ "$PUSH" == "true" ]]; then
    update_file "$repo" "app.R"      "$new_app" "update for v1.2.0: use mode instead of ignore"
    update_file "$repo" "survey.qmd" "$new_qmd" "update for v1.2.0: add mode: preview to survey-settings"
  else
    echo "  [DRY RUN] app.R diff:"
    diff <(echo "$raw_app") <(echo "$new_app") || true
    echo "  [DRY RUN] survey.qmd diff:"
    diff <(echo "$raw_qmd") <(echo "$new_qmd") || true
  fi
done

echo ""
if [[ "$PUSH" == "true" ]]; then
  echo "All templates updated."
else
  echo "Dry run complete. Run with --push to apply changes."
fi
