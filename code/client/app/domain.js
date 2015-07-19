import {Rx} from '@cycle/core';
import _ from 'lodash';
import moment from 'moment';
import {trace} from './util.js'

module.exports = {
  update
};

function update (inputs) {
  return {
    state$: Rx.Observable.just('Logs'),
    logGroups$: logGroups(inputs),
    logs$: logs(inputs),
    tags$: tags(inputs),
    taggings$: taggings(inputs),
    curUser$: Rx.Observable.just('Logs'),
    isUserLoading$: isUserLoading(inputs)
  };
}

function isUserLoading ({register$, userCreated$, login$, sessionCreated$}) {
  return Rx.Observable.merge([
      register$.map(() => 1),
      login$.map(() => 1),
      userCreated$.map(() => -1),
      sessionCreated$.map(() => -1),
    ])
    .do()
    .scan(0, (a,b) => a + b)
    .startWith(0)
    .map(a => a > 0);
}

function logGroups ({logsLoaded$}) {
  // return logsLoaded$.map(logs => {
  //   // TODO
  // });
return Rx.Observable.just([]);
}

function logs ({logsLoaded$}) {
  // return logsLoaded$.map(logs => {
  //   return _.chain(logs)
  //           .map(log => [log.id, log])
  //           .zipObject()
  //           .value();
  // }).startWith({})

  return Rx.Observable.just({});
}

function tags ({tagsLoaded$}) {
  // return tagsLoaded$.map(tags => {
  //   return _.chain(tag)
  //           .map(tag => [tag.id, tag])
  //           .zipObject()
  //           .value();
  // }).startWith({});
  return Rx.Observable.just({});
}

function taggings ({taggingsLoaded$}) {
  // TODO fix this
  // return taggingsLoaded$.map(taggings => {
  //   return _.chain(tag)
  //           .map(tag => [tag.id, tag])
  //           .zipObject()
  //           .value();
  // }).startWith({});
  return Rx.Observable.just({});
}