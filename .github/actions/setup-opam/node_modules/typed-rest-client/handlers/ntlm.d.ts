/// <reference types="node" />
import ifm = require('../Interfaces');
import http = require("http");
export declare class NtlmCredentialHandler implements ifm.IRequestHandler {
    private _ntlmOptions;
    constructor(username: string, password: string, workstation?: string, domain?: string);
    prepareRequest(options: http.RequestOptions): void;
    canHandleAuthentication(response: ifm.IHttpClientResponse): boolean;
    handleAuthentication(httpClient: ifm.IHttpClient, requestInfo: ifm.IRequestInfo, objs: any): Promise<ifm.IHttpClientResponse>;
    private handleAuthenticationPrivate(httpClient, requestInfo, objs, finalCallback);
    private sendType1Message(httpClient, requestInfo, objs, finalCallback);
    private sendType3Message(httpClient, requestInfo, objs, res, callback);
}
