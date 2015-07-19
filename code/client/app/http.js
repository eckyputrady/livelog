import {makeHTTPDriver} from '@cycle/http';
import {Rx} from '@cycle/core';
import {trace} from './util.js';

module.exports = {
  driver, input, output
};

//// DRIVER

function driver () {
  return makeHTTPDriver();
}

//// INPUT

function input (HTTP$$) {
  let http$$ = HTTP$$.share();
  return {
    // logAdded$:
    // logRemoved$:
    // logsLoaded$:
    // tagAdded$:
    // tagRemoved$:
    // tagsLoaded$:
    // taggingAdded$:
    // taggingRemoved$:
    // taggingsLoaded$:
    userCreated$: commonParse(3, http$$),
    sessionCreated$: commonParse(4, http$$)
  };
}

function commonParse (type, http$$) {
  return http$$
    .filter(x$ => x$.request.__type === type)
    .flatMapLatest(x$ => {
      return x$ .map(x => { return { succ: x.body }; })
                .catch(x => { return Rx.Observable.just({ fail: x }); })
                .map(x => {
                  x.request = x$.request;
                  return x;
                });
    })
}

//// OUTPUT

function output (model, inputs) {
  return Rx.Observable.merge([
    loadLogs(model, inputs),
    loadTags(model, inputs),
    createUser(model, inputs),
    createSession(model, inputs)
  ]);
}

function loadLogs ({state$}) {
  return state$.distinctUntilChanged().filter(e => e === 'Logs').map(_ => {
    return {
      __type: 1,
      method: 'GET',
      url: '/logs?sort=-createdAt',
    };
  });
}

function loadTags ({state$}) {
  return state$.distinctUntilChanged().filter(e => e === 'Tags').map(_ => {
    return {
      __type: 2,
      method: 'GET',
      url: '/tags',
    };
  });
}

function createUser (model, {register$}) {
  return register$.map(data => {
    return {
      __type: 3,
      method: 'POST',
      url: '/users',
      send: data
    };
  });
}

function createSession (model, {userCreated$, login$}) {
  let afterRegister$ = userCreated$.filter(e => !!e.succ).map(e => e.request.send);
  return Rx.Observable.merge(login$, afterRegister$).map(data => {
    return {
      __type: 4,
      method: 'POST',
      url: '/sessions',
      send: data
    };
  });
}