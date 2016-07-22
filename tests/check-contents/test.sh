FLOW=$1

echo "syntax_error.js"
$FLOW check-contents --strip-root syntax_error.js < syntax_error.js
