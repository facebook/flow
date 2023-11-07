/* @flow */

declare var trustedTypes: TrustedTypePolicyFactory;

const policy: TrustedTypePolicy = trustedTypes.createPolicy(
  'insecurePassthroughPolicy',
  {
    createHTML: s => s,
    createScript: s => s,
  },
);

const html: TrustedHTML = policy.createHTML('<what></what>');
const script: TrustedScript = policy.createScript('<what></what>');
const script_url: TrustedScriptURL = policy.createScriptURL('<what></what>');

// Check that type predicates refine the types
const html_mixed: mixed = html;
if (trustedTypes.isHTML(html_mixed)) {
  html_mixed as TrustedHTML;
}

const script_mixed: mixed = script;
if (trustedTypes.isScript(script_mixed)) {
  script_mixed as TrustedScript;
}

const script_url_mixed: mixed = script_url;
if (trustedTypes.isScriptURL(script_url_mixed)) {
  script_url_mixed as TrustedScriptURL;
}
