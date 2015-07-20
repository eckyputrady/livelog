import {run, Rx} from '@cycle/core';
import _ from 'lodash';

let DOM = require('./DOM/index.js');
let Domain = require('./domain.js');
let HTTP = require('./http.js');

// main
console.log('Livelog v0.0.1');
document.querySelector('body').appendChild(document.createElement('div'));
run(main, {
  DOM: DOM.driver('body > div'),
  HTTP: HTTP.driver()
});

////

function main (responses) {
  return output(update(input(responses)));
}

function output ({model, inputs}) {
  let ret = {
    DOM: DOM.output(model, inputs),
    HTTP: HTTP.output(model, inputs),
  };
  // Rx.Observable.merge(ret.DOM, ret.HTTP).subscribe(trace('ok'), trace('err'));
  return ret;
}

function update (inputs) {
  let model = Domain.update(inputs);
  return {model: model, inputs: inputs};
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
    console.log(prefix, e);
    return e;
  };
}