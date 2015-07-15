import {makeHTTPDriver} from '@cycle/http';
import {Rx} from '@cycle/core';

module.exports = {
  driver, input, output
};

function driver () {
  return makeHTTPDriver();
}

// INPUT

function input (HTTP$) {
  let http$$ = HTTP$.share();
  return {
    loginRes$     : commonParse(login(), http$$),
    logoutRes$    : commonParse(logout(), http$$),
    registerRes$  : commonParse(register(), http$$),
    checkLoginRes$: commonParse(checkLogin(), http$$),
    loadLogsRes$  : commonParse(loadLogs(), http$$),
    createLogRes$ : commonParse(createLog(), http$$),
    createTagRes$ : commonParse(createTag(), http$$),
    loadTagsRes$  : commonParse(loadTags(), http$$)
  };
}

function commonParse (req, http$$) {
  return http$$
    .filter(x$ => isMatchReq(req, x$.request))
    .flatMap(x$ => x$
      .map(e => { return {succ: e.body}; })
      .catch(e => Rx.Observable.just({fail:e.response.body}))
    );
}

function isMatchReq (req1, req2) {
  return req1.__type === req2.__type;
}

// OUTPUT

function output (model$) {
  return model$.flatMap(m => m.sideFx).map(act).filter(e => !!e);
}

function act (sideFx) {
  switch(sideFx.type) {
    case 'login'      : return login(sideFx.data);
    case 'logout'     : return logout();
    case 'register'   : return register(sideFx.data);
    case 'checkLogin' : return checkLogin(sideFx.data);
    case 'loadLogs'   : return loadLogs(sideFx.data);
    case 'createLog'  : return createLog(sideFx.data);
    case 'createTag'  : return createTag(sideFx.data);
    case 'loadTags'   : return loadTags();
    default           : 
      console.log('unknown sideFx type:', sideFx.type);
      return null;
  }
}

function checkLogin () {
  return {
    __type: 0,
    method: 'GET',
    url: '/sessions'
  };
}

function register (data) {
  return {
    __type: 1,
    method: 'POST',
    url: '/users',
    send: data
  };
}

function login (data) {
  return {
    __type: 2,
    method: 'POST',
    url: '/sessions',
    send: data
  };
}

function logout () {
  return {
    __type: 3,
    method: 'DEL',
    url: '/sessions'
  };
}

function loadLogs () {
  return {
    __type: 4,
    method: 'GET',
    url: '/logs?sort=-createdAt'
  };
}

function createLog (data) {
  return {
    __type: 5,
    method: 'POST',
    url: '/logs',
    send: data
  };
}

function createTag (data) {
  return {
    __type: 6,
    method: 'POST',
    url: '/tags',
    send: data
  };
}

function loadTags () {
  return {
    __type: 7,
    method: 'GET',
    url: '/tags'
  };
}