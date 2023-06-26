import * as e from './Scaffold/src/index';


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


export const mkReCaptchaApi = function(api) {
    return () => {
        return new e.ReCaptchaApi(api);
    }
}

export const goReCaptcha = function(key, api) {
    return function(onError, onOk) {
        grecaptcha.ready(function() {
            grecaptcha.execute(key, {
                action: 'submit'
            }).then(function(token) {
                api.apiCaptchaVerifyPost('\"' + token + '\"').then(onOk).catch(onError)
            });
        });
    }
}

export const getSuccessReCaptcha = captcha => {
    return captcha.getSuccess();
}