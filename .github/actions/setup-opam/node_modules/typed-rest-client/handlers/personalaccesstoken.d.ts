import ifm = require('../Interfaces');
export declare class PersonalAccessTokenCredentialHandler implements ifm.IRequestHandler {
    token: string;
    constructor(token: string);
    prepareRequest(options: any): void;
    canHandleAuthentication(response: ifm.IHttpClientResponse): boolean;
    handleAuthentication(httpClient: ifm.IHttpClient, requestInfo: ifm.IRequestInfo, objs: any): Promise<ifm.IHttpClientResponse>;
}
