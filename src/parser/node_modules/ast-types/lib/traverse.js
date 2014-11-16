var visit = require("./path-visitor").visit;
var warnedAboutDeprecation = false;

function traverseWithFullPathInfo(node, callback) {
    if (!warnedAboutDeprecation) {
        warnedAboutDeprecation = true;
        console.warn(
            "\033[33m", // yellow
            'DEPRECATED(ast-types): Please use require("ast-types").visit ' +
                "instead of .traverse for syntax tree manipulation." +
            "\033[0m" // reset
        );
    }

    return visit(node, {
        visitNode: function(path) {
            if (callback.call(path, path.value) !== false) {
                this.traverse(path);
            }

            return false;
        }
    });
}

traverseWithFullPathInfo.fast = traverseWithFullPathInfo;
module.exports = traverseWithFullPathInfo;
