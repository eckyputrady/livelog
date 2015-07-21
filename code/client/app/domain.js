import {Rx} from '@cycle/core';
import _ from 'lodash';
import moment from 'moment';
import {trace} from './util.js';

module.exports = {
  update
};

function update (inputs) {
  return _.mapValues({
    state$: state(inputs),
    logGroups$: logGroups(inputs).do(trace('logGroups')),
    logs$: logs(inputs).do(trace('logs')),
    tags$: tags(inputs),
    taggings$: taggings(inputs),
    curUser$: curUser(inputs),
    isUserLoading$: isUserLoading(inputs),
    curLogId$: curLogId(inputs)
  }, x => x.shareReplay(1));
}

function state ({setState$}) {
  return setState$.startWith('Logs');
}

function curLogId ({logsLoaded$}) {
  return logsLoaded$.filter(xs => !xs.fail).map(xs => xs.succ[0] ? xs.succ[0].id : null).startWith(null);
}

function curUser ({sessionLoaded$, sessionRemoved$, logout$}) {
  let loggedIn$ = sessionLoaded$.filter(x => !x.fail).map(x => x.succ.name);
  let loggedOut$ = Rx.Observable.merge([sessionRemoved$, logout$]).map(() => null);
  return Rx.Observable.merge([loggedIn$, loggedOut$]).startWith(null);
}

function isUserLoading ({register$, userCreated$, login$, sessionCreated$}) {
  return Rx.Observable.merge([
      register$.map(() => 1),
      login$.map(() => 1),
      userCreated$.map(() => -1),
      sessionCreated$.map(() => -1),
    ])
    .scan(0, (a,b) => a + b)
    .startWith(0)
    .map(a => a > 0);
}

function logGroups ({logsLoaded$}) {
  return logsLoaded$.filter(e => !e.fail).map(logs => {
    return _.chain(logs.succ)
            .map(x => { 
              return {
                rawDate: x.createdAt,
                date: moment.utc(x.createdAt).format('MMMM DD'),
                logId: x.id
              }; 
            })
            .groupBy('date')
            .value();
  })
  .startWith({});
}

function logs ({logsLoaded$}) {
  return logsLoaded$
    .filter(e => !e.fail)
    .map(e => e.succ)
    .map(logs => {
      return _.chain(logs)
              .map(log => [log.id, log])
              .zipObject()
              .value();
    }).startWith({});

  // return Rx.Observable.just({});
}

function tags ({tagsLoaded$}) {
  return tagsLoaded$.map(tags => {
    return _.chain(tags.succ)
            .map(tag => [tag.id, tag])
            .zipObject()
            .value();
  }).startWith({});
}

function taggings ({taggingsLoaded$}) {
  let newTagging$ = taggingsLoaded$.filter(x => !x.fail)
    .map(x => {
      let ret = {};
      ret[x.request.send.logId] = _.map(x.succ, 'id');
      return ret;
    });
  return newTagging$.scan(_.assign).startWith({});
  // return Rx.Observable.just({});
}