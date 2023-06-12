import * as e from './Scaffold/src/index';

export const mkApiClient = function(host) {
    return () => {
        let cl = new e.ApiClient(host)
        cl.defaultHeaders = [];
        return cl;
    }
};

export const mkSendGridApi = function(api) {
    return () => {
        return new e.SendGridApi(api);
    }
}

export const mkScaffoldApiControllerSendGridSendMailRequest =
    function(body) {
        return () => {
            let req = new e.ScaffoldApiControllerSendGridSendMailRequest();
            return e.ScaffoldApiControllerSendGridSendMailRequest.constructFromObject(body, req)
        };
    }

export const send =
    function(req, api) {
        return function(onError, onOk) {
            api.apiSendgridSendPost(req).then(onOk).catch(onError)
        };
    }

export const getDataFromResponseImpl = left => right => resp => { 
    e.Response.validateJSON(resp); 
    let success = e.Response.constructFromObject(resp).getSuccess(); 
    let error = e.Response.constructFromObject(resp).getError();
    return () => { return success !== undefined ? right(success) : left(error); };
}

export const printError = (err) => { return err.getMessage(); }