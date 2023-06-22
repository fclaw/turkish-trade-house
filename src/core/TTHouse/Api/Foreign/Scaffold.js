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

export const mkSendGridSendMailRequest =
    function(body) {
        return () => {
            let req = new e.SendGridSendMailRequest();
            return e.SendGridSendMailRequest.constructFromObject(body, req)
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

export const getDataFromObjImpl = left => right => resp => {
    let success = resp.getSuccess();
    let errMsg = (xs) => {
        tmp = '';
        xs.forEach(e => {
            tmp += e.getMessage();
        });
        return tmp;
    }
    return () => {
        return success !== undefined ?
            right(success) :
            left(errMsg(resp.getErrors()));
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

export const printFrontendInit =
    function(obj) {
        let msg =
            "{ content: " +
            obj.getContent().getHome() + ", " +
            obj.getContent().getAbout() + ", " +
            obj.getContent().getService() +
            ", shaCommit: " + obj.getShaCommit() + "}";
        return msg
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

export const getShaCSSCommit = (obj) => {
    return obj.getShaCommitCss();
}

export const getCookiesInit = (obj) => {
    return obj.getCookies();
}

export const loadTranslationImpl =
    function(resource, lang, loc, api) {
        return function(onError, onOk) {
            api.apiFrontendTranslateResourceLangGet(resource, lang, loc).then(onOk).catch(onError)
        };
    }

export const getTranslatedContent = (obj) => {
    return obj.getTranslationContent();
}

export const getTranslatedMenuArray = (obj) => {
    return obj.getTranslationMenu();
}

export const getMenuItemKey = (obj) => {
    return obj.getKey();
}

export const getMenuItemVal = (obj) => {
    return obj.getValue();
}

export const mkLogReq = function(build, payload) {
    let req = new e.FrontendLogRequest();
    req.setBuild(build);
    req.setPayload(payload);
    return () => {
        return req;
    };
}

export const sendLog = function(req, api) {
    return function(onError, onOk) {
        api.apiFrontendLogPut(req).then(onOk).catch(onError)
    };
}

export const getCookies = function(api) {
    return function(onError, onOk) {
        api.apiFrontendCookiesGet().then(onOk).catch(onError)
    };
}

export const showCookieImpl = cookie => {
    return cookie.getName()
}

export const getMetaImpl = function(page, api) {
    console.log(page);
    return function(onError, onOk) {
        api.apiFrontendMetaGet(page).then(onOk).catch(onError)
    };
}

export const getMetaDescription = meta => { return meta.getDescription(); }
