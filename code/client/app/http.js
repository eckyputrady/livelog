"use strict";

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
  return {
    loginRes$: parseLoginRes(HTTP$),
    registerRes$: parseRegisterRes(HTTP$)
  };
}

function parseLoginRes (HTTP$) {
  let ret = HTTP$
    .filter(x$ => x$.request.url === '/sessions')
    .mergeAll()
    .catch(e => Rx.Observable.just({fail:e}));
  return ret;
}

function parseRegisterRes (HTTP$) {
  let ret = HTTP$
    .filter(x$ => x$.request.url === '/users')
    .mergeAll()
    .catch(e => Rx.Observable.just({fail:e}));
  return ret;
}

// OUTPUT

function output (actions) {
  return Rx.Observable.merge(
    actions.register$.map(register),
    actions.login$.map(login)
  );
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