#!/usr/bin/env bash
# Shared helpers for the project-status-automation workflow.
# Source this file inside a `run:` step after checking out the repository.

# Resolves the "Status" field ID and a named option ID for a GitHub ProjectV2.
# On success, sets FIELD_ID and OPTION_ID in the caller's scope.
# Limitation: fetches up to the first 50 fields. If the "Status" field is positioned
# beyond the 50th field in the project, it will not be found and the function will
# return a non-zero exit code with an error annotation.
# Usage: resolve_status_ids <project_node_id> <status_option_name>
resolve_status_ids() {
  local project_id="$1" status_name="$2"
  local fields
  fields=$(gh api graphql \
    -f query='query($id:ID!){node(id:$id){...on ProjectV2{fields(first:50){nodes{...on ProjectV2SingleSelectField{id name options{id name}}}}}}}' \
    -f id="$project_id")
  FIELD_ID=$(echo "$fields" | jq -r '.data.node.fields.nodes[]|select(.name=="Status")|.id')
  OPTION_ID=$(echo "$fields" | jq -r --arg s "$status_name" \
    '.data.node.fields.nodes[]|select(.name=="Status")|.options[]|select(.name==$s)|.id')
  [ -z "$FIELD_ID"  ] && { echo "::error::\"Status\" field not found in project (node: $project_id)."; return 1; }
  [ -z "$OPTION_ID" ] && { echo "::error::\"$status_name\" option not found in the Status field."; return 1; }
}

# Finds the project item for an issue in the specified project.
# Outputs JSON {itemId: "...", projectId: "..."} to stdout, or nothing if not found.
# Note: fetches up to 100 project memberships per issue.
# Usage: find_issue_project_item <issue_node_id> <project_number>
find_issue_project_item() {
  local issue_id="$1" project_number="$2"
  gh api graphql \
    -f query='query($id:ID!){node(id:$id){...on Issue{projectItems(first:100){nodes{id project{...on ProjectV2{id number}}}}}}}' \
    -f id="$issue_id" \
    | jq --argjson n "$project_number" \
         '.data.node.projectItems.nodes[]|select(.project.number==$n)|{itemId:.id,projectId:.project.id}'
}
