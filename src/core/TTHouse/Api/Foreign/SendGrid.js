import * as e from './SendGrid/src/index';

export const mkApiClient = function(api_key) {
  let cl = new e.ApiClient(); 
  cl.defaultHeaders = {'Authorization':  api_key};
  return ()  => { return cl; };
}

export const mkMailSendApi = function(api) {
    return () => { return new e.MailSendApi(api); }
}

export const send = function(personalizations, from, subject, content) {}