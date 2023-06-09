import * as e from './SendGrid/src/index';

export const mkApiClient = function(api_key) {
    let cl = new e.ApiClient('https://api.sendgrid.com');
    cl.defaultHeaders = {
        'Authorization': api_key
    };
    return () => {
        return cl;
    };
}

export const mkMailSendApi = function(api) {
    return () => {
        return new e.MailSendApi(api);
    }
}

export const mkPOSTMailSendRequest = 
  function(xs, email, subject, content) {
    return () => { return new e.POSTMailSendRequest(xs, email, subject, content); };
  }

export const send = 
  function(req, api) {
    return function (onError, onOk) { 
        api.pOSTMailSend(req).then(onOk).catch(onError) 
    };
  }

export const print = function(obj) { return JSON.stringify(obj); }