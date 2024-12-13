# Instructions for Developers

## Selfhost Setup

- install Code: https://code.visualstudio.com/
- git clone https://github.com/flowtype/flow-for-vscode.git
- run `yarn install --pure-lockfile`
- open Code on `flow-for-vscode`
- link the extension into Code: `ln -s ABSOLUTE_PATH_OF_FLOW_FOR_VSCODE ~/.vscode/extensions/flow-for-vscode`
- reload Code (`Cmd+Shift+P`, `Reload Window`) to enable the extension
- make changes to the extension and reload to selfhost on your changes

## Development

- you can use the built-in babel task to start babel in watch mode (`Cmd+Shift+B` to start it, `Cmd+Shift+U` to toggle output)
- make changes
- open debug viewlet (`Cmd+Shift+D`) and run the extension to try changes (`F5`)
- from the opened instance open a folder with flow files (`File | Open...`)
