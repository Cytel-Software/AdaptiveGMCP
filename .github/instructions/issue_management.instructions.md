# Issue Management Instructions

Follow these rules when creating, updating, or reviewing issues, epics, and project items in this repository:

1. When creating a new epic issue, always apply the `epic` label.
2. When creating any new issue, classify it as `Feature`, `Task`, or `Bug` and set the issue Type accordingly.
3. As work progresses, update issue state correctly (for example: open, in progress, closed) so tracking remains accurate.
4. Use explicit parent-child relationships between issues where they exist. Stories or tasks intended to belong to an epic should be created or added as sub-issues of that epic.
5. Do not add or use a markdown checkbox list of child issues inside an epic description to track epic progress. Manual checkbox lists drift as child issue status changes, so they are not a reliable source of truth.
6. Use real sub-issue relationships as the source of truth for hierarchy and rollup progress.
7. Verify the parent link and project membership before marking issue-management work complete.
8. Keep epics and their children in the same Project so automation and reporting stay consistent and visible in one place.

## Reliable Issue Creation and Linking Workflow

Use this sequence first when creating issues and linking sub-issues. This order is mandatory because it has been consistently reliable in this repository.

### Try First (Known to Work)

1. Create the issue with GitHub CLI, not via MCP issue-create retries:
	- `gh issue create --repo Cytel-Software/AdaptiveGMCP --title "..." --body "..." --label ...`
2. Verify creation immediately:
	- `gh issue view <issue_number> --repo Cytel-Software/AdaptiveGMCP --json number,title,state,url`
3. For parent-child linking, use GraphQL variables (never inline quoted owner/repo payloads in PowerShell):
	- Query IDs with variables:
	  - `gh api graphql -f query="query($owner:String!,$repo:String!){repository(owner:$owner,name:$repo){parent:issue(number:87){id number} child:issue(number:108){id number}}}" -F owner='Cytel-Software' -F repo='AdaptiveGMCP'`
	- Link with variables:
	  - `gh api graphql -f query="mutation($parentId:ID!,$childId:ID!){addSubIssue(input:{issueId:$parentId,subIssueId:$childId}){issue{number} subIssue{number}}}" -F parentId='<parent_node_id>' -F childId='<child_node_id>'`
4. Verify link and governance fields:
	- `gh issue view <issue_number> --repo Cytel-Software/AdaptiveGMCP --json number,title,state,parent,labels,projectItems,url`
5. If issue type must be set, use GraphQL updateIssue with `ID` scalar for `issueTypeId`:
	- `gh api graphql -f query="mutation($issueId:ID!,$issueTypeId:ID!){updateIssue(input:{id:$issueId,issueTypeId:$issueTypeId}){issue{number issueType{name}}}}" -F issueId='<issue_node_id>' -F issueTypeId='<issue_type_id>'`

### Do Not Try First (Known Failure-Prone Patterns)

1. Do not start with repeated MCP issue-create attempts when the first call behaves unexpectedly.
	- Switch immediately to `gh issue create` after the first MCP failure/hiccup.
2. Do not use placeholder or unrelated fallback calls (for example, Copilot job-status checks) to recover issue creation flow.
	- They do not create issues and add noise/confusion.
3. Do not embed complex GraphQL with heavily escaped inline quotes in PowerShell command strings.
	- This frequently breaks parsing for owner/repo/query payload.
4. Do not proceed without immediate post-action verification (`gh issue view ... --json ...`).
	- Missing verification causes hidden failures in hierarchy/project metadata.

### Recovery Rule

If issue creation or linking fails once in the preferred path, report the exact failing command and error, then retry once with corrected variables/IDs. If it fails again, stop and ask for direction instead of chaining unrelated recovery attempts.
