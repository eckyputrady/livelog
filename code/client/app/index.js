"use strict";

import {run, Rx} from '@cycle/core';
// import {makeHTTPDriver} from '@cycle/http';
import _ from 'lodash';

let DOM = require('./dom.js');
let Domain = require('./domain.js');
// let HTTP = require('./http.js');

// main
document.querySelector('body').appendChild(document.createElement('div'));
run(main, {
  DOM: DOM.driver('body > div'),
  // HTTP: makeHTTPDriver()
});

function main (responses) {
  let actions = mergeIntents([
    DOM.input(responses),
    // HTTP.input(responses)
  ]);
  let model$ = Domain.update(actions);
  return {
    DOM: DOM.output(model$),
    // HTTP: HTTP.output(actions)
  };
}

function mergeIntents (intents) {
  let combiner = (acc, x) => _.merge(acc, x, Rx.Observable.merge);
  let merged = _.reduce(intents, combiner, {});
  return _.map(merged, e => e.publish().refcount());
}

// function parseLoginRes (HTTP) {
//   return HTTP.filter(res$ => res$.request === '/sessions').mergeAll();
// }

// // http

// function toHTTP (actions) {
//   let ret = Rx.Observable.merge(
//     actions.register$.map(toHTTP_register),
//     actions.login$.map(toHTTP_login)
//   );
//   // ret.forEach((x) => console.log(x));
//   return ret;
// }
// function toHTTP_register (register) {
//   return {
//     method: 'POST',
//     url: '/users',
//     send: register
//   };
// }
// function toHTTP_login (register) {
//   return {
//     method: 'POST',
//     url: '/sessions',
//     send: register
//   };
// }
