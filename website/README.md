# Website

This website is built using [Docusaurus 2](https://docusaurus.io/), a modern static website generator.

## Local Development

```bash
$ ../scripts/serve-website.sh
```

This command builds Flow binary and starts a local development server and opens up a browser window.
Most changes are reflected live without having to restart the server.

If you don't care about inline Flow errors in code blocks and try-flow page, you can run the following command instead:

```bash
$ yarn install
$ NO_INLINE_FLOW_ERRORS=1 yarn start
```
