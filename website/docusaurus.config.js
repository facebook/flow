/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

const {fbContent} = require('docusaurus-plugin-internaldocs-fb/internal');
const webpack = require('webpack');
const MonacoWebpackPlugin = require('monaco-editor-webpack-plugin');
const allFlowVersions = require('./src/js/flow-versions');

/** @type {import('@docusaurus/types').DocusaurusConfig} */
module.exports = {
  title: 'Flow',
  tagline: 'A Static Type Checker for JavaScript',
  url: 'https://flow.org',
  baseUrl: process.env.DOCUSAURUS_BASE_URL || '/',
  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',
  favicon: 'img/favicon.png',
  organizationName: 'facebook', // Usually your GitHub org/user name.
  projectName: 'flow', // Usually your repo name.
  trailingSlash: true,
  markdown: {
    mermaid: true,
  },
  themes: ['@docusaurus/theme-mermaid'],
  plugins: [
    function polyfillNodeBuiltinsForFlowJS(context, options) {
      return {
        name: 'polyfillNodeBuiltinsForFlowJS',
        configureWebpack() {
          return {resolve: {fallback: {fs: false, constants: false}}};
        },
      };
    },
    function enableWasm() {
      return {
        name: 'enableWasm',
        configureWebpack() {
          return {
            experiments: {syncWebAssembly: true},
            module: {
              rules: [
                {
                  test: /\.wasm$/i,
                  loader: 'file-loader',
                  type: 'javascript/auto',
                },
              ],
            },
          };
        },
      };
    },
    function enableSomeEnvVarsAsBuildTimeConstants() {
      return {
        name: 'enableSomeEnvVarsAsBuildTimeConstants',
        configureWebpack() {
          return {
            plugins: [
              new webpack.EnvironmentPlugin({
                INTERNAL_STATIC_DOCS: process.env.INTERNAL_STATIC_DOCS || false,
              }),
            ],
          };
        },
      };
    },
    function enableMonacoEditorPlugin() {
      return {
        name: 'enableMonacoEditorPlugin',
        configureWebpack() {
          return {
            // https://stackoverflow.com/questions/69265357/monaco-editor-worker
            plugins: [new MonacoWebpackPlugin()],
          };
        },
      };
    },
  ],
  themeConfig: {
    algolia: process.env.INTERNAL_STATIC_DOCS
      ? undefined
      : {
          appId: 'P6T3E8XPGT',
          apiKey: '01f111c0b2980e54f1307e982fa2c218',
          indexName: 'flow',
          contextualSearch: true,
        },
    prism: {
      theme: require('prism-react-renderer/themes/github'),
    },
    colorMode: {
      defaultMode: 'light',
      disableSwitch: true,
      respectPrefersColorScheme: false,
    },
    navbar: {
      logo: {
        alt: 'My Facebook Project Logo',
        src: 'img/logo.svg',
      },
      items: [
        {
          to: 'en/docs/getting-started',
          activeBasePath: 'en/docs/getting-started',
          label: 'Getting Started',
          position: 'left',
        },
        {
          to: 'en/docs/',
          activeBasePath: 'en/docs',
          label: 'Docs',
          position: 'left',
        },
        process.env.INTERNAL_STATIC_DOCS
          ? {to: 'https://flow.org/try', label: 'Try', position: 'left'}
          : {
              to: 'try/',
              activeBasePath: 'try',
              label: 'Try',
              position: 'left',
            },
        {
          to: 'blog/',
          label: 'Blog',
          position: 'left',
        },
        {
          href: 'https://twitter.com/flowtype',
          'aria-label': 'Twitter',
          position: 'right',
          className: 'navbar__icon twitter__link',
        },
        {
          href: 'http://stackoverflow.com/questions/tagged/flowtype',
          'aria-label': 'Stack Overflow',
          position: 'right',
          className: 'navbar__icon stackoverflow__link',
        },
        // Please keep GitHub link to the right for consistency.
        {
          href: 'https://github.com/facebook/flow',
          'aria-label': 'GitHub',
          position: 'right',
          className: 'navbar__icon github__link',
        },
      ],
    },
    footer: {
      style: 'dark',
      links: [
        {
          title: 'Learn',
          items: [
            {
              label: 'Getting Started',
              to: 'en/docs/getting-started',
            },
          ],
        },
        {
          title: 'Community',
          items: [
            {
              label: 'Twitter',
              href: 'https://twitter.com/flowtype',
            },
            {
              label: 'Discord',
              href: 'https://discordapp.com/invite/8ezwRUK',
            },
          ],
        },
        {
          title: 'More',
          items: [
            {
              label: 'Medium',
              to: 'https://medium.com/flow-type',
            },
            {
              label: 'GitHub',
              href: 'https://github.com/facebook/flow',
            },
          ],
        },
        {
          title: 'Legal',
          // Please do not remove the privacy and terms, it's a legal requirement.
          items: [
            {
              label: 'Privacy',
              href: 'https://opensource.facebook.com/legal/privacy/',
            },
            {
              label: 'Terms',
              href: 'https://opensource.facebook.com/legal/terms/',
            },
            {
              label: 'Data Policy',
              href: 'https://opensource.facebook.com/legal/data-policy/',
            },
            {
              label: 'Cookie Policy',
              href: 'https://opensource.facebook.com/legal/cookie-policy/',
            },
          ],
        },
      ],
      logo: {
        alt: 'Facebook Open Source Logo',
        src: 'img/oss_logo.png',
        href: 'https://opensource.facebook.com',
      },
      // Please do not remove the credits, help to publicize Docusaurus :)
      copyright: `Copyright © ${new Date().getFullYear()} Meta Platforms, Inc. Built with Docusaurus.`,
    },
  },
  customFields: {
    /* The latest release is in the second slot, but it might be unavailable if
       INCLUDE_PAST_RELEASES env var is not on. In that case, we default to master. */
    flowVersion: allFlowVersions[1] || allFlowVersions[0],
    allFlowVersions,
    fbRepoName: 'fbsource',
  },
  presets: [
    [
      require.resolve('docusaurus-plugin-internaldocs-fb/docusaurus-preset'),
      {
        docs: {
          routeBasePath: 'en/docs',
          sidebarPath: require.resolve('./sidebars.js'),
          editUrl: fbContent({
            internal:
              'https://www.internalfb.com/code/fbsource/fbcode/flow/website/',
            external: 'https://github.com/facebook/flow/edit/main/website/',
          }),
          remarkPlugins: [
            [require('@docusaurus/remark-plugin-npm2yarn'), {sync: true}],
            require('./src/js/flow-check-remark-plugin'),
          ],
        },
        staticDocsProject: 'flow',
        blog: {
          path: 'blog',
          postsPerPage: 50,
          showReadingTime: false,
          blogSidebarCount: 'ALL',
          blogSidebarTitle: 'All our posts',
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
        googleAnalytics: process.env.INTERNAL_STATIC_DOCS
          ? undefined
          : {trackingID: 'UA-49208336-4', anonymizeIP: true},
      },
    ],
  ],
};
