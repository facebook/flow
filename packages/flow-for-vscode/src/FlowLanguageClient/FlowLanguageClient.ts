/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import {
  type ILanguageClient,
  type LanguageClientOptions,
  type ServerOptions,
  type StatusReport,
  CloseAction,
  ErrorAction,
  LanguageClient,
  RevealOutputChannelOn,
} from '../utils/LanguageClient';

import * as vscode from 'vscode';
import path from 'path';
import { promises as fs } from 'fs';

import StatusBarWidget from './StatusBarWidget';
import createMiddleware from './createMiddleware';
import ClientCommands from './ClientCommands';
import * as UUID from 'vscode-languageclient/lib/common/utils/uuid';

import getFlowPath from '../utils/getFlowPath';
import getFlowVersion from '../utils/getFlowVersion';
import assertFlowSupportsLSP from '../utils/assertFlowSupportsLSP';
import assertNoLogFileOption from '../utils/assertNoLogFileOption';
import uriToString from '../utils/uriToString';

import Logger, { type LogLevel } from '../utils/Logger';

export type FlowLanguageClientConfig = {
  useNPMPackagedFlow: boolean;
  pathToFlow: string;
  useBundledFlow: boolean;
  stopFlowOnExit: boolean;
  useCodeSnippetOnFunctionSuggest: boolean;
  lazyMode: string | null;
  logLevel: LogLevel;
  coverage: {
    showUncovered: boolean;
    diagnosticSeverity: vscode.DiagnosticSeverity;
  };
};

type Options = {
  flowconfigPath: string;
  workspaceRoot: string;
  outputChannel: vscode.OutputChannel;
  canUseRelativePattern: boolean;
  getConfig: () => FlowLanguageClientConfig;
};

export default class FlowLanguageClient {
  _options: Options;
  _statusBarWidget: StatusBarWidget;
  // @ts-expect-error: not definitely initialized
  _logger: Logger;
  _client: ILanguageClient | null = null;
  _id: string;

  private hiddenDiagnostics: Map<string, vscode.Diagnostic[]> = new Map();

  commands: ClientCommands;

  constructor(options: Options) {
    this._id = UUID.generateUuid();
    this._options = options;

    this.commands = new ClientCommands(this);
    this._statusBarWidget = new StatusBarWidget({
      clientName: this.getName(),
      flowconfig: this._options.flowconfigPath,
      onClickCommand: this.commands.clientActions,
    });

    this._init();
  }

  getID(): string {
    return this._id;
  }

  getStatus(): StatusReport | null {
    return this._statusBarWidget.getStatus();
  }

  dispose(): Promise<void> {
    this._statusBarWidget.dispose();
    this.commands.dispose();
    return this._disposeClient();
  }

  getName(): string {
    const { _options } = this;
    return path.relative(
      path.dirname(_options.workspaceRoot),
      _options.flowconfigPath,
    );
  }

  restart(): void {
    this._logger.info('restarting client');
    // NOTE: re-initializing instead of restarting client
    // as this will handle more error edge cases
    // (example flow is removed so we need to find flow again)
    this._reinit();
  }

  setActive(val: boolean): void {
    if (val) {
      this._statusBarWidget.show();
    } else {
      this._statusBarWidget.hide();
    }
  }

  getWorkspaceRoot(): string {
    return this._options.workspaceRoot;
  }

  getFlowconfig(): string {
    return this._options.flowconfigPath;
  }

  getLogger(): Logger {
    return this._logger;
  }

  getHiddenDiagnosticsForFile(file: vscode.Uri): readonly vscode.Diagnostic[] {
    return this.hiddenDiagnostics.get(file.toString()) ?? [];
  }

  async _reinit(): Promise<void> {
    try {
      await this._disposeClient();
    } catch (err) {
      // ignore error if disposeClient failed
    }
    await this._init();
  }

  async _init(): Promise<void> {
    try {
      const config = this._options.getConfig();
      this._logger = this._createLogger(config);
      this._client = await this._createClient(config);
      this._client.start();
    } catch (err: any) {
      this._handleInitError(err);
    }
  }

  async _createClient(
    config: FlowLanguageClientConfig,
  ): Promise<ILanguageClient> {
    const { _logger, _options, _statusBarWidget } = this;
    const { outputChannel, flowconfigPath, workspaceRoot } = _options;

    const flowconfigDir = path.dirname(flowconfigPath);

    const flowPath = await getFlowPath({
      pathToFlow: config.pathToFlow,
      flowconfigDir: path.dirname(flowconfigPath),
      workspaceRoot,
      useNPMPackagedFlow: config.useNPMPackagedFlow,
      useBundledFlow: config.useBundledFlow,
      logger: _logger,
    });

    const flowVersion = await getFlowVersion(flowPath);

    _logger.info(`Using flow '${flowPath}' (v${flowVersion})`);

    // make sure flow support `flow lsp`
    assertFlowSupportsLSP(flowVersion);

    const flowconfig = await fs.readFile(flowconfigPath, 'utf8');
    assertNoLogFileOption(flowconfig);

    _statusBarWidget.setFlowInfo({ path: flowPath, version: flowVersion });

    const serverOptions: ServerOptions = {
      command: flowPath,
      args: [
        'lsp',
        ...['--from', 'vscode'],
        ...(config.lazyMode ? ['--lazy-mode', config.lazyMode] : []),
        // auto stop flow process
        config.stopFlowOnExit ? '--autostop' : null,
      ].filter((v) => v != null),
      options: { shell: true },

      // see: clientOptions.workspaceFolder below
      // options: { cwd: flowconfigDir },
    };

    const patternGlob = '**/*';
    // @ts-ignore
    const pattern: string = this._options.canUseRelativePattern
      ? // all files inside flowconfigDir
        new vscode.RelativePattern(flowconfigDir, patternGlob)
      : patternGlob;

    const clientOptions: LanguageClientOptions = {
      // NOTE: nested .flowconfig filtering not possible using only documentSelector
      // so also using middleware to filter out nested files request from parent clients
      documentSelector: [
        { scheme: 'file', language: 'javascript', pattern },
        { scheme: 'file', language: 'javascriptreact', pattern },
      ],
      middleware: createMiddleware(flowconfigPath, this.hiddenDiagnostics),

      uriConverters: {
        code2Protocol: uriToString,
        protocol2Code: (value) => vscode.Uri.parse(value),
      },

      outputChannel,

      // flow lsp throws error in many cases and lsp client by default opens up output panel on error
      // Should we make this configurable?? Maybe it's useful to know why some commands not working
      // in some cases.
      revealOutputChannelOn: RevealOutputChannelOn.Never,

      initializationOptions: {
        liveSyntaxErrors: true,
        detailedErrorRendering: true,
        semanticDecorations: vscode.workspace
          .getConfiguration('flow')
          .get<boolean>('semanticDecorations', true),
        refinementInformationOnHover: vscode.workspace
          .getConfiguration('flow')
          .get<boolean>('refinementInformationOnHover', true),
        useCodeSnippetOnFunctionSuggest: config.useCodeSnippetOnFunctionSuggest,
      },

      initializationFailedHandler: (error) => {
        this._handleInitError(error);
        // don't initialize again let user decide what to do
        return false;
      },

      // NOTE: we want client rootPath & cwd to be flowconfigPath
      // vscode-languageclient uses clientOptions.workspaceFolder (if passed) for cwd and rootPath
      // so passing dummy workspaceFolder with flowconfigDir as uri
      workspaceFolder: {
        name: flowconfigPath,
        index: 0,
        uri: vscode.Uri.file(flowconfigDir),
      },

      // NOTE: not part of official vscode-languageclient
      extensions: {
        status: {
          onChange: (status) => {
            if (status && status.state === 'error') {
              status.actions = [
                {
                  title: 'Restart Client',
                  command: () => {
                    this.commands.runRestartClientCommand();
                  },
                },
                // NOTE: action 'Restart Client' handle more error cases compared to
                // flow 'restart' action so removing flow restart action
                ...(status.actions || []).filter(
                  (action) => action.title.toLowerCase() !== 'restart',
                ),
              ];
            }
            this._setStatus(status);
          },
        },
        typeCoverage: {
          onChange: (coverage) => {
            this._statusBarWidget.setCoverage(coverage);
          },
          command: this.commands.toggleCoverage,
          defaultShowUncovered: config.coverage.showUncovered,
          diagnosticSeverity: config.coverage.diagnosticSeverity,
        },
      },
    };

    // Create the language client and start the client.
    const client = new LanguageClient(
      'flow',
      'Flow',
      serverOptions,
      clientOptions,
    );

    return client;
  }

  _disposeClient(): Promise<void> {
    this._client?.clientOptions;
    return Promise.resolve(this._client ? this._client.stop() : undefined);
  }

  _createLogger(config: FlowLanguageClientConfig): Logger {
    const { _options } = this;
    return new Logger(this.getName(), _options.outputChannel, config.logLevel);
  }

  _handleInitError(err: Error): void {
    const msg = `Failed to start flow\n${err.toString()}`;
    this._logger.error(msg);
    this._setStatus({
      state: 'error',
      message: `${msg}`,
      actions: [
        {
          title: 'Retry',
          command: () => {
            this._reinit();
          },
        },
      ],
    });
  }

  _setStatus(status: null | StatusReport): void {
    this._statusBarWidget.setStatus(status);
    if (status && status.state === 'error') {
      this.commands.runShowStatusCommand();
    }
  }

  logDebugInfo(): void {
    this._logger.info(
      JSON.stringify(
        {
          flowconfig: this._options.flowconfigPath,
          flow: this._statusBarWidget.getFlowInfo(),
          serverStatus: this._statusBarWidget.getStatus(),
        },
        null,
        2,
      ),
    );
  }
}
