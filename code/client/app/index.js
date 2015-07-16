import {run, Rx} from '@cycle/core';
import _ from 'lodash';

let DOM = require('./dom.js');
let Domain = require('./domain.js');
let HTTP = require('./http.js');

// main
console.log('Livelog v0.0.1');
document.querySelector('body').appendChild(document.createElement('div'));
run(main, {
  DOM: DOM.driver('body > div'),
  HTTP: HTTP.driver()
});

function main (responses) {
  return output(update(input(responses)));
}

function output (model$) {
  return {
    DOM: DOM.output(model$),
    HTTP: HTTP.output(model$),
  };
}

function update (intents) {
  let model$ = Domain.update(intents).map(trace('model:')).share();
  model$.subscribe(() => {}, trace('Err'));
  return model$;
}

function input (responses) {
  return mergeIntents([
    DOM.input(responses.DOM),
    HTTP.input(responses.HTTP)
  ]);
}

function mergeIntents (intents) {
  let flatten = (xs) => _.filter(_.flatten(xs, true));
  let combiner = (acc, x) => _.merge(acc, x, (a,b) => [a].concat([b]));
  let merged = _.reduce(intents, combiner, {});
  let ret = _.mapValues(merged, e => Rx.Observable.merge(flatten(e)).map(trace('intent:')).share());
  return ret;
}

function trace (prefix) {
  return function _trace (e) {
    // console.log(prefix, e);
    return e;
  };
}