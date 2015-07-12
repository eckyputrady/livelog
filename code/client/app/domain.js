"use strict";

import {Rx} from '@cycle/core';

module.exports = {
  update
};

function update (actions) {
  let mergedActions = Rx.Observable.merge(
    actions.register$.map(setHandler(handleRegister)),
    actions.login$.map(setHandler(handleRegister))
  );
  return mergedActions.scan(defaultModel(), (acc, x) => x.__handler(acc, x)).startWith(defaultModel());
}

function setHandler (f) {
  return function _setHandler (x) {
    x.__handler = f;
    return x;
  };
}

function handleRegister (model) {
  model.user.isLoading = true;
  return model;
}

// model
/**
  model = {
    user :: Loadable (Maybe User),
    logs :: Loadable [Loadable Log]
    tags :: Loadable [Loadable Tag]
    state :: Logs | Tags
  }

  User = { name :: String }
  Log = {
    id :: Int
    message :: String
    createdAt :: DateTime
    tags :: Loadable [Loadable Tag]
  }
  Tag = {
    id :: Int
    name :: String
  }
  Loadable a = {
    isLoading :: Boolean
    isSucc :: Boolean
    sVal :: a
    eVal :: Obj
  }
*/
function defaultModel () {
  return {
    user: defaultLoadable(null),
    logs: defaultLoadable([defaultLoadable(dummyLogs()), defaultLoadable(dummyLogs())]),
    tags: defaultLoadable([defaultLoadable(dummyTags()), defaultLoadable(dummyTags())]),
    state: 'Logs'
  };
}
function defaultLoadable (val) {
  return {
    isLoading: false,
    isSucc: true,
    sVal: val,
    eVal: {}
  };
}
function dummyLogs () {
  return {
    id: 1,
    message: 'Dummy log',
    createdAt: new Date(),
    tags: defaultLoadable([defaultLoadable(dummyTags()), defaultLoadable(dummyTags()), defaultLoadable(dummyTags())])
  };
}
function dummyTags () {
  return {
    id: 1,
    name: 'Dummy tag'
  };
}

//// Intent
/**
  register$ = {name :: String, pass :: String}
  login$ = {name :: String, pass :: String}
  loginRes$ = {succ = {name :: String}, fail :: String}
*/