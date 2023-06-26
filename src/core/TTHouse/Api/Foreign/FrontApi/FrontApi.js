import * as e from '../TTHouse.Api.Foreign.Scaffold/Scaffold/src/index';

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

export const _showInit =
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

export const _loadTranslation =
    function(lang, api) {
        return function(onError, onOk) {
            api.apiFrontendTranslateLangGet(lang).then(onOk).catch(onError)
        };
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

export const _showCookie = cookie => {
    return cookie.getName()
}

export const getCookies = function(api) {
    return function(onError, onOk) {
        api.apiFrontendCookiesGet().then(onOk).catch(onError)
    };
}

export const _getMeta = function(page, api) {
    return function(onError, onOk) {
        api.apiFrontendMetaGet(page).then(onOk).catch(onError)
    };
}

export const getMetaDescription = meta => {
    return meta.getDescription();
}

export const _getKeyText = obj => {
    return obj.getKey();
}
export const _getValText = obj => {
    return obj.getValue();
}

export const getCookiesInit = (obj) => {
    return obj.getCookies();
}

export const _getTranslationMenu = obj => {
    return obj.getMenu();
}

export const _getTranslationPage = obj => {
    return obj.getPage();
}

export const _showMapMenuText = menu => {
    return "{ key: " + menu.getKey() + ", value: " + menu.getValue() + " }";
}

export const getTranslationCopyright = obj => { return obj.getCopyright(); }

export const _getTranslationMessenger = obj => {
    return obj.getMessenger();
}