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
    loginRes$     : parseLoginRes(http$$),
    logoutRes$    : parseLogoutRes(http$$),
    registerRes$  : parseRegisterRes(http$$),
    checkLoginRes$: parseCheckLoginRes(http$$)
  };
}

function parseCheckLoginRes (http$$) {
  return http$$
    .filter(x$ => x$.request.url === '/sessions' && x$.request.method === 'GET')
    .flatMap(x$ => x$
      .map(e => { return {succ: e}; })
      .catch(e => Rx.Observable.just({fail:e.response.body}))
    );
}

function parseLoginRes (HTTP$) {
  let ret = HTTP$
    .filter(x$ => x$.request.url === '/sessions' && x$.request.method === 'POST')
    .flatMap(x$ => x$.catch(e => Rx.Observable.just({fail:e.response.body})));
  return ret;
}

function parseLogoutRes (HTTP$) {
  let ret = HTTP$
    .filter(x$ => x$.request.url === '/sessions' && x$.request.method === 'DEL')
    .flatMap(x$ => x$.catch(e => Rx.Observable.just({fail:e.response.body})));
  return ret;
}

function parseRegisterRes (HTTP$) {
  let ret = HTTP$
    .filter(x$ => x$.request.url === '/users')
    .flatMap(x$ => x$.catch(e => Rx.Observable.just({fail:e.response.body})));
  return ret;
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
    default           : 
      console.log('unknown sideFx type:', sideFx.type);
      return null;
  }
}

function checkLogin () {
  return {
    method: 'GET',
    url: '/sessions'
  };
}

function register (data) {
  return {
    method: 'POST',
    url: '/users',
    send: data
  };
}

function login (data) {
  return {
    method: 'POST',
    url: '/sessions',
    send: data
  };
}

function logout () {
  return {
    method: 'DEL',
    url: '/sessions'
  };
}