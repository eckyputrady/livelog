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
    loginRes$   : parseLoginRes(http$$),
    registerRes$: parseRegisterRes(http$$)
  };
}

function parseLoginRes (HTTP$) {
  let ret = HTTP$
    .filter(x$ => x$.request.url === '/sessions')
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
    case 'login'    : return login(sideFx.data);
    case 'register' : return register(sideFx.data);
    default         : return null;
  }
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