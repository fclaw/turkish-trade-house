import * as e from '../TTHouse.Api.Foreign.Scaffold/Scaffold/src/index';

export const mkApiClient = function(host) {
    return () => {
        let cl = new e.ApiClient(host)
        cl.defaultHeaders = [];
        return cl;
    }
}

export const _getDataFromObj = left => right => resp => {
    let success = resp.getSuccess();

    let errMsg = (xs) => {
        tmp = '';
        xs.forEach(e => {
            tmp += e.getMessage();
        });
        return tmp;
    }
    let err = resp.getErrors() !== undefined ? errMsg(resp.getErrors()) : 'malformed resp: ' + JSON.stringify(resp);
    return () => {
        return success !== undefined ? right(success) : left(err);
    };
}

export const _printError = (err) => {
    return err.getMessage();
}