"use strict";

import {run, Rx} from '@cycle/core';
import _ from 'lodash';

let DOM = require('./dom.js');
let Domain = require('./domain.js');
let HTTP = require('./http.js');

// main
document.querySelector('body').appendChild(document.createElement('div'));
run(main, {
  DOM: DOM.driver('body > div'),
  HTTP: HTTP.driver()
});

function main (responses) {
  let actions = mergeIntents([
    DOM.input(responses.DOM),
    HTTP.input(responses.HTTP)
  ]);
  logActions(actions);
  let model$ = Domain.update(actions);
  return {
    DOM: DOM.output(model$),
    HTTP: HTTP.output(actions)
  };
}

function mergeIntents (intents) {
  let combiner = (acc, x) => _.merge(acc, x, (a,b) => Rx.Observable.merge(a,b));
  let merged = _.reduce(intents, combiner, {});
  return _.mapValues(merged, e => e.publish().refCount());
}

function logActions (actions) {
  return _.chain(actions)
          .pairs()
          .map(xs => xs[1].map(x => {return {type: xs[0], data: x}}))
          .reduce((a,b) => Rx.Observable.merge(a,b))
          .value()
          .forEach(x => console.log('Action:', x));
}