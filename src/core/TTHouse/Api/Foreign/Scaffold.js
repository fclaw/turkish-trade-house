import * as e from './Scaffold/src/index';

export const mkApiClient = function(host) {
    return () => {
        let cl = new e.ApiClient(host)
        cl.defaultHeaders = [];
        return cl;
    }
};

export const mkForeignApi = function(api) {
    return () => {
        return new e.ForeignApi(api);
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
            api.apiForeignSendgridSendPost(req).then(onOk).catch(onError)
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

export const printScaffoldApiControllerFrontendInitInit =
    function(obj) {
        return "{ content: " + obj.getContent().getHome() + ", " + obj.getContent().getAbout() + ", " + obj.getContent().getService() + ", shaCommit: " + obj.getShaCommit() + "}";
    }

export const getHomeContent = (obj) => {
    return obj.getContent().getHome();
}

export const getAboutContent = (obj) => {
    return obj.getContent().getHome();
}

export const getServiceContent = (obj) => {
    return obj.getContent().getHome();
}

export const getShaCommit = (obj) => {
    return obj.getShaCommit();
}