/// <reference types="node" />
import http = require("http");
import url = require("url");
export interface IHeaders {
    [key: string]: any;
}
export interface IBasicCredentials {
    username: string;
    password: string;
}
export interface IHttpClient {
    options(requestUrl: string, additionalHeaders?: IHeaders): Promise<IHttpClientResponse>;
    get(requestUrl: string, additionalHeaders?: IHeaders): Promise<IHttpClientResponse>;
    del(requestUrl: string, additionalHeaders?: IHeaders): Promise<IHttpClientResponse>;
    post(requestUrl: string, data: string, additionalHeaders?: IHeaders): Promise<IHttpClientResponse>;
    patch(requestUrl: string, data: string, additionalHeaders?: IHeaders): Promise<IHttpClientResponse>;
    put(requestUrl: string, data: string, additionalHeaders?: IHeaders): Promise<IHttpClientResponse>;
    sendStream(verb: string, requestUrl: string, stream: NodeJS.ReadableStream, additionalHeaders?: IHeaders): Promise<IHttpClientResponse>;
    request(verb: string, requestUrl: string, data: string | NodeJS.ReadableStream, headers: IHeaders): Promise<IHttpClientResponse>;
    requestRaw(info: IRequestInfo, data: string | NodeJS.ReadableStream): Promise<IHttpClientResponse>;
    requestRawWithCallback(info: IRequestInfo, data: string | NodeJS.ReadableStream, onResult: (err: any, res: IHttpClientResponse) => void): void;
}
export interface IRequestHandler {
    prepareRequest(options: http.RequestOptions): void;
    canHandleAuthentication(response: IHttpClientResponse): boolean;
    handleAuthentication(httpClient: IHttpClient, requestInfo: IRequestInfo, objs: any): Promise<IHttpClientResponse>;
}
export interface IHttpClientResponse {
    message: http.IncomingMessage;
    readBody(): Promise<string>;
}
export interface IRequestInfo {
    options: http.RequestOptions;
    parsedUrl: url.Url;
    httpModule: any;
}
export interface IRequestOptions {
    headers?: IHeaders;
    socketTimeout?: number;
    ignoreSslError?: boolean;
    proxy?: IProxyConfiguration;
    cert?: ICertConfiguration;
    allowRedirects?: boolean;
    maxRedirects?: number;
    maxSockets?: number;
    keepAlive?: boolean;
    presignedUrlPatterns?: RegExp[];
    allowRetries?: boolean;
    maxRetries?: number;
}
export interface IProxyConfiguration {
    proxyUrl: string;
    proxyUsername?: string;
    proxyPassword?: string;
    proxyBypassHosts?: string[];
}
export interface ICertConfiguration {
    caFile?: string;
    certFile?: string;
    keyFile?: string;
    passphrase?: string;
}
