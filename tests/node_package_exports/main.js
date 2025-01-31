import mainExports from "main-exports";

mainExports as empty;

import subpathExportsRoot from "subpath-exports";
import subpathExports from "subpath-exports/subpath";

subpathExportsRoot as empty;
subpathExports as empty;

import defaultExportCondition from "default-export-condition";

conditionalExports1 as empty;

import scopedSubpathExportsRoot from "@scope/subpath-exports";
import scopedSubpathExports from "@scope/subpath-exports/subpath";

scopedSubpathExportsRoot as empty;
scopedSubpathExports as empty;

import specifiedExportCondition from "specified-export-condition";

specifiedExportCondition as empty;

import exportConditionOrder from "export-condition-order";

exportConditionOrder as empty;

import subpathPatternA from "subpath-pattern/feature/a";
import subpathPatternB from "subpath-pattern/feature/b.js";

subpathPatternA as empty;
subpathPatternB as empty;

import innerSubpathPatternA from "subpath-pattern/feature/inner/a";

innerSubpathPatternA as empty;

import privateSubpathPatternA from "subpath-pattern/feature/private/a";

import nestedConditions from "nested-conditions";

nestedConditions as empty;
