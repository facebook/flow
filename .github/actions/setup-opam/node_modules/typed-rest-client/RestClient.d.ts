/// <reference types="node" />
import httpm = require('./HttpClient');
import ifm = require("./Interfaces");
export interface IRestResponse<T> {
    statusCode: number;
    result: T | null;
    headers: Object;
}
export interface IRequestOptions {
    acceptHeader?: string;
    additionalHeaders?: ifm.IHeaders;
    responseProcessor?: Function;
    deserializeDates?: boolean;
}
export declare class RestClient {
    client: httpm.HttpClient;
    versionParam: string;
    /**
     * Creates an instance of the RestClient
     * @constructor
     * @param {string} userAgent - userAgent for requests
     * @param {string} baseUrl - (Optional) If not specified, use full urls per request.  If supplied and a function passes a relative url, it will be appended to this
     * @param {ifm.IRequestHandler[]} handlers - handlers are typically auth handlers (basic, bearer, ntlm supplied)
     * @param {ifm.IRequestOptions} requestOptions - options for each http requests (http proxy setting, socket timeout)
     */
    constructor(userAgent: string, baseUrl?: string, handlers?: ifm.IRequestHandler[], requestOptions?: ifm.IRequestOptions);
    private _baseUrl;
    /**
     * Gets a resource from an endpoint
     * Be aware that not found returns a null.  Other error conditions reject the promise
     * @param {string} requestUrl - fully qualified or relative url
     * @param {IRequestOptions} requestOptions - (optional) requestOptions object
     */
    options<T>(requestUrl: string, options?: IRequestOptions): Promise<IRestResponse<T>>;
    /**
     * Gets a resource from an endpoint
     * Be aware that not found returns a null.  Other error conditions reject the promise
     * @param {string} resource - fully qualified url or relative path
     * @param {IRequestOptions} requestOptions - (optional) requestOptions object
     */
    get<T>(resource: string, options?: IRequestOptions): Promise<IRestResponse<T>>;
    /**
     * Deletes a resource from an endpoint
     * Be aware that not found returns a null.  Other error conditions reject the promise
     * @param {string} resource - fully qualified or relative url
     * @param {IRequestOptions} requestOptions - (optional) requestOptions object
     */
    del<T>(resource: string, options?: IRequestOptions): Promise<IRestResponse<T>>;
    /**
     * Creates resource(s) from an endpoint
     * T type of object returned.
     * Be aware that not found returns a null.  Other error conditions reject the promise
     * @param {string} resource - fully qualified or relative url
     * @param {IRequestOptions} requestOptions - (optional) requestOptions object
     */
    create<T>(resource: string, resources: any, options?: IRequestOptions): Promise<IRestResponse<T>>;
    /**
     * Updates resource(s) from an endpoint
     * T type of object returned.
     * Be aware that not found returns a null.  Other error conditions reject the promise
     * @param {string} resource - fully qualified or relative url
     * @param {IRequestOptions} requestOptions - (optional) requestOptions object
     */
    update<T>(resource: string, resources: any, options?: IRequestOptions): Promise<IRestResponse<T>>;
    /**
     * Replaces resource(s) from an endpoint
     * T type of object returned.
     * Be aware that not found returns a null.  Other error conditions reject the promise
     * @param {string} resource - fully qualified or relative url
     * @param {IRequestOptions} requestOptions - (optional) requestOptions object
     */
    replace<T>(resource: string, resources: any, options?: IRequestOptions): Promise<IRestResponse<T>>;
    uploadStream<T>(verb: string, requestUrl: string, stream: NodeJS.ReadableStream, options?: IRequestOptions): Promise<IRestResponse<T>>;
    private _headersFromOptions(options, contentType?);
    private static dateTimeDeserializer(key, value);
    private _processResponse<T>(res, options);
}
