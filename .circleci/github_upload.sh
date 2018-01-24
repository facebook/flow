#!/bin/sh

AUTH="Authorization: token $DOC_BOT_TOKEN"
REPO="$CIRCLE_PROJECT_USERNAME/$CIRCLE_PROJECT_REPONAME"
response=$(curl -sH "$AUTH" "https://api.github.com/repos/$REPO/releases/tags/$CIRCLE_TAG")
id=$(echo "$response" | grep '^  "id": ' | sed 's/\s*"id":\s*\(\d*\),.*/\1/')

curl -H "$AUTH" \
  -H "Accept: application/vnd.github.manifold-preview" \
  -H "Content-Type: application/zip" \
  --data-binary @$1 \
  "https://uploads.github.com/repos/$REPO/releases/$id/assets?name=$2"
