---
layout: guide
---

Flow has worked with Nuclide directly on adding support for the new warning
severity level. Certain features are likely to be in other editors, but others
might not yet be implemented.

### Nuclide <a class="toc" id="toc-nuclide" href="#toc-nuclide"></a>

In Nuclide, Flow warnings are distinct from Flow errors and rendered in a different color.

Nuclide v0.243.0 onward has support for working with Flow to limit the reported warnings to the working fileset.
This allows Nuclide and Flow to work efficiently on large codebases with tens of thousands of unsuppressed warnings.

### Other Editors <a class="toc" id="toc-other-editors" href="#toc-other-editors"></a>

In most editors, Flow warnings are likely to be rendered the same way as other
warnings are rendered by that editor.

Most editors will likely display all Flow warnings, which is fine for small- to
medium-scale projects, or projects with fewer unsuppressed warnings.
