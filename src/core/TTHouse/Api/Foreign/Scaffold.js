import * as e from './Scaffold/src/index';
import ScaffoldApiControllerFrontendContentContent from './Scaffold/src/model/ScaffoldApiControllerFrontendContentContent';

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
    let success = resp.getSuccess();
    let error = resp.getError();
    return () => {
        return success !== undefined ? right(success) : left(error);
    };
}

export const printError = (err) => {
    return err.getMessage();
}

export const mkFrontApi = function(api) {
    return () => {
        return new e.FrontApi(api);
    }
}

export const init =
    function(api) {
        return function(onError, onOk) {
            api.apiFrontendInitGet().then(onOk).catch(onError)
        };
    }

export const printScaffoldApiControllerFrontendContentContent =
    function(obj) {
        return "{" + obj.getHome() + ", " + obj.getAbout() + ", " + obj.getService();
    }

export const getHomeContent = (obj) => { return obj.getHome(); }

export const getAboutContent = (obj) => { return obj.getHome(); }

export const getServiceContent = (obj) => { return obj.getHome(); }