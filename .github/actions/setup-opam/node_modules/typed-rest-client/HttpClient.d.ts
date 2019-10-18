/// <reference types="node" />
import url = require("url");
import http = require("http");
import ifm = require('./Interfaces');
export declare enum HttpCodes {
    OK = 200,
    MultipleChoices = 300,
    MovedPermanently = 301,
    ResourceMoved = 302,
    SeeOther = 303,
    NotModified = 304,
    UseProxy = 305,
    SwitchProxy = 306,
    TemporaryRedirect = 307,
    PermanentRedirect = 308,
    BadRequest = 400,
    Unauthorized = 401,
    PaymentRequired = 402,
    Forbidden = 403,
    NotFound = 404,
    MethodNotAllowed = 405,
    NotAcceptable = 406,
    ProxyAuthenticationRequired = 407,
    RequestTimeout = 408,
    Conflict = 409,
    Gone = 410,
    InternalServerError = 500,
    NotImplemented = 501,
    BadGateway = 502,
    ServiceUnavailable = 503,
    GatewayTimeout = 504,
}
export declare class HttpClientResponse implements ifm.IHttpClientResponse {
    constructor(message: http.IncomingMessage);
    message: http.IncomingMessage;
    readBody(): Promise<string>;
}
export interface RequestInfo {
    options: http.RequestOptions;
    parsedUrl: url.Url;
    httpModule: any;
}
export declare function isHttps(requestUrl: string): boolean;
export declare class HttpClient implements ifm.IHttpClient {
    userAgent: string;
    handlers: ifm.IRequestHandler[];
    requestOptions: ifm.IRequestOptions;
    private _ignoreSslError;
    private _socketTimeout;
    private _httpProxy;
    private _httpProxyBypassHosts;
    private _allowRedirects;
    private _maxRedirects;
    private _allowRetries;
    private _maxRetries;
    private _agent;
    private _proxyAgent;
    private _keepAlive;
    private _disposed;
    private _certConfig;
    private _ca;
    private _cert;
    private _key;
    constructor(userAgent: string, handlers?: ifm.IRequestHandler[], requestOptions?: ifm.IRequestOptions);
    options(requestUrl: string, additionalHeaders?: ifm.IHeaders): Promise<ifm.IHttpClientResponse>;
    get(requestUrl: string, additionalHeaders?: ifm.IHeaders): Promise<ifm.IHttpClientResponse>;
    del(requestUrl: string, additionalHeaders?: ifm.IHeaders): Promise<ifm.IHttpClientResponse>;
    post(requestUrl: string, data: string, additionalHeaders?: ifm.IHeaders): Promise<ifm.IHttpClientResponse>;
    patch(requestUrl: string, data: string, additionalHeaders?: ifm.IHeaders): Promise<ifm.IHttpClientResponse>;
    put(requestUrl: string, data: string, additionalHeaders?: ifm.IHeaders): Promise<ifm.IHttpClientResponse>;
    head(requestUrl: string, additionalHeaders?: ifm.IHeaders): Promise<ifm.IHttpClientResponse>;
    sendStream(verb: string, requestUrl: string, stream: NodeJS.ReadableStream, additionalHeaders?: ifm.IHeaders): Promise<ifm.IHttpClientResponse>;
    /**
     * Makes a raw http request.
     * All other methods such as get, post, patch, and request ultimately call this.
     * Prefer get, del, post and patch
     */
    request(verb: string, requestUrl: string, data: string | NodeJS.ReadableStream, headers: ifm.IHeaders): Promise<ifm.IHttpClientResponse>;
    /**
     * Needs to be called if keepAlive is set to true in request options.
     */
    dispose(): void;
    /**
     * Raw request.
     * @param info
     * @param data
     */
    requestRaw(info: ifm.IRequestInfo, data: string | NodeJS.ReadableStream): Promise<ifm.IHttpClientResponse>;
    /**
     * Raw request with callback.
     * @param info
     * @param data
     * @param onResult
     */
    requestRawWithCallback(info: ifm.IRequestInfo, data: string | NodeJS.ReadableStream, onResult: (err: any, res: ifm.IHttpClientResponse) => void): void;
    private _prepareRequest(method, requestUrl, headers);
    private _isPresigned(requestUrl);
    private _mergeHeaders(headers);
    private _getAgent(requestUrl);
    private _getProxy(requestUrl);
    private _isBypassProxy(requestUrl);
    private _performExponentialBackoff(retryNumber);
}
